{-# LANGUAGE BangPatterns #-}

module Canny where

import Prelude                                  as P
import Data.Bits
import Data.Word
import Codec.BMP
import Criterion.Main                           ( defaultMain, whnf, bench, bgroup )
import System.Environment

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A
import Data.Array.Accelerate.CUDA               as A

import Data.Array.Repa.IO.BMP                   ( writeImageToBMP )
import Data.Array.Repa.Repr.Unboxed             ( U )
import qualified Data.Vector.Unboxed.Mutable    as VM
import qualified Data.Vector.Unboxed            as V
import qualified Data.Array.Repa                as R
import qualified Data.Array.Repa.Repr.Unboxed   as R


main :: IO ()
main = do
  (args,rest)   <- break (== "--") `fmap` getArgs

  let (threshLow, threshHigh, fileIn, fileOut) = case args of
        [fi, fo]
          -> (50, 100, fi, fo)

        [tl, th, fi, fo]
          -> (read tl, read th, fi, fo)

        _ -> error
           $ concat [ "acc-canny [<threshLow::Int> <threshHigh::Int>]"
                    , " <fileIn.bmp> <fileOut.bmp>" ]

  -- Read the input image, and run through the algorithm once to produce the
  -- completed edges
  --
  img           <- readImageFromBMP fileIn

  let low       = unit (constant threshLow)
      high      = unit (constant threshHigh)

      grey      = toGreyscale
      blurX     = gaussianX . grey
      blurred   = gaussianY . blurX
      gradX     = gradientX . blurred
      gradY     = gradientY . blurred
      magdir a  = gradientMagDir low (gradX a) (gradY a)
      suppress  = nonMaximumSuppression low high . magdir

      stage1    = A.toRepa . run $ suppress (use img)

  strong        <- selectStrong stage1
  edges         <- wildfire stage1 strong

  writeImageToBMP fileOut (R.zip3 edges edges edges)


  -- Now, benchmark each (accelerate) stage. The "kernels" group is intended to
  -- be run under nvvp, whereas the canny group is for end-to-end benchmarks.
  --
  let opts      = if null rest then [] else P.tail rest
      force arr = A.indexArray arr (Z:.0:.0 :: DIM2) `seq` ()

      -- Need to force partial results so that we benchmark individual stages
      --
      grey'     = run $ toGreyscale (use img)
      blurX'    = run $ gaussianX (use grey')
      blurred'  = run $ gaussianY (use blurX')
      gradX'    = run $ gradientX (use blurred')
      gradY'    = run $ gradientY (use blurred')
      magdir'   = run $ gradientMagDir low (use gradX') (use gradY')

  withArgs opts $ defaultMain
    [ bgroup "kernels"
      [ bench "greyscale"   $ whnf (force . run . grey) (use img)
      , bench "blur-x"      $ whnf (force . run . gaussianX) (use grey')
      , bench "blur-y"      $ whnf (force . run . gaussianY) (use blurX')
      , bench "grad-x"      $ whnf (force . run . gradientX) (use blurred')
      , bench "grad-y"      $ whnf (force . run . gradientY) (use blurred')
      , bench "mag-orient"  $ whnf (force . run . gradientMagDir low (use gradX')) (use gradY')
      , bench "suppress"    $ whnf (force . run . nonMaximumSuppression low high) (use magdir')
      ]

    , bgroup "canny"
      [ bench "run"         $ whnf (force . run . suppress) (use img)
      , bench "run1"        $ whnf (force . run1 suppress) img
      ]
    ]

-- BMP Files -------------------------------------------------------------------

type RGBA               = Word32
type Image a            = Array DIM2 a


readImageFromBMP :: FilePath -> IO (Image RGBA)
readImageFromBMP file = do
  bmp           <- either (error . show) id `fmap` readBMP file
  let (w,h)     =  bmpDimensions bmp
  --
  A.fromByteString (Z :. h :. w) ((), unpackBMPToRGBA32 bmp)


-- Accelerate component --------------------------------------------------------

type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

-- Classification of the output pixel
data Orient     = Undef | PosD | Vert | NegD | Horiz
data Edge       = None  | Weak | Strong

orient :: Orient -> Int
orient Undef    = 0
orient PosD     = 64
orient Vert     = 128
orient NegD     = 192
orient Horiz    = 255

orient' :: Orient -> Exp Int
orient' = constant . orient

edge :: Edge -> Float
edge None       = 0
edge Weak       = 0.5
edge Strong     = 1.0

edge' :: Edge -> Exp Float
edge' = constant . edge

convolve5x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]


-- RGB to Greyscale conversion, in the range [0,255]
--
toGreyscale :: Acc (Image RGBA) -> Acc (Image Float)
toGreyscale = A.map luminanceOfRGBA

luminanceOfRGBA :: (Elt a, IsFloating a) => Exp RGBA -> Exp a
luminanceOfRGBA rgba =
  let r = 0.3  * A.fromIntegral (rgba                 .&. 0xFF)
      g = 0.59 * A.fromIntegral ((rgba `div` 0x100)   .&. 0xFF)
      b = 0.11 * A.fromIntegral ((rgba `div` 0x10000) .&. 0xFF)
  in
  r + g + b


rgbaOfLuminance :: (Elt a, IsFloating a) => Bool -> Exp a -> Exp RGBA
rgbaOfLuminance invert val = r + b + g + a
  where
    u   = A.truncate (255 * (0 `A.max` val `A.min` 1))
    v   = if invert then 0xFF - u else u
    r   = v
    g   = v * 0x100
    b   = v * 0x10000
    a   =     0xFF000000


-- Separable Gaussian blur in the x- and y-directions
--
gaussianX :: Acc (Image Float) -> Acc (Image Float)
gaussianX = stencil (convolve5x1 gaussian) Clamp
  where
    gaussian = [ 1, 4, 6, 4, 1 ]

gaussianY :: Acc (Image Float) -> Acc (Image Float)
gaussianY = stencil (convolve1x5 gaussian) Clamp
  where
    gaussian = P.map (/256) [ 1, 4, 6, 4, 1 ]


-- Gradients in the x- and y- directions
--
gradientX :: Acc (Image Float) -> Acc (Image Float)
gradientX = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((u, _, x)
         ,(v, _, y)
         ,(w, _, z)) = x + (2*y) + z - u - (2*v) - w

gradientY :: Acc (Image Float) -> Acc (Image Float)
gradientY = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((x, y, z)
         ,(_, _, _)
         ,(u, v, w)) = x + (2*y) + z - u - (2*v) - w


-- Classify the magnitude and orientation of the image gradient
--
gradientMagDir
  :: Acc (Scalar Float)
  -> Acc (Image Float)
  -> Acc (Image Float)
  -> Acc (Image (Float, Int))
gradientMagDir threshLow = A.zipWith (\dx dy -> lift (magnitude dx dy, direction dx dy))
  where
    low                 = the threshLow
    magnitude dx dy     = sqrt (dx * dx + dy + dy)
    direction dx dy =
      let -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          --
          theta         = atan2 dy dx
          alpha         = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          --
          norm          = alpha + 8 * A.fromIntegral (boolToInt (alpha <=* 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          --
          undef         = abs dx <=* low &&* abs dy <=* low
      in
      boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `A.min` 255)


-- Non-maximum suppression classifies pixels that are the local maximum along
-- the direction of the image gradient as either strong or weak edges. All other
-- pixels are not considered edges at all.
--
-- The image intensity is in the range [0,1]
--
nonMaximumSuppression
  :: Acc (Scalar Float)
  -> Acc (Scalar Float)
  -> Acc (Image (Float,Int))
  -> Acc (Image Float)
nonMaximumSuppression threshLow threshHigh magdir =
  generate (shape magdir) $ \ix ->
    let -- The input parameters
        --
        low             = the threshLow
        high            = the threshHigh
        (mag, dir)      = unlift (magdir ! ix)
        Z :. h :. w     = unlift (shape magdir)
        Z :. y :. x     = unlift ix

        -- Determine the points that lie either side of this point along to the
        -- direction of the image gradient.
        --
        -- The direction coding:
        --
        --   192   128   64
        --          |
        --   255 --- ---
        --
        offsetx         = dir >* orient' Vert  ? (-1, dir <* orient' Vert ? (1, 0))
        offsety         = dir <* orient' Horiz ? (-1, 0)

        (fwd, _)        = unlift $ magdir ! lift (clamp (Z :. y+offsety :. x+offsetx)) :: (Exp Float, Exp Int)
        (rev, _)        = unlift $ magdir ! lift (clamp (Z :. y-offsety :. x-offsetx)) :: (Exp Float, Exp Int)

        clamp (Z:.u:.v) = Z :. 0 `A.max` u `A.min` (h-1) :. 0 `A.max` v `A.min` (w-1)

        -- Try to avoid doing explicit tests to avoid warp divergence.
        --
        none            = dir ==* orient' Undef ||* mag <* low ||* mag <* fwd ||* mag <* rev
        strong          = mag >=* high
    in
    A.fromIntegral (boolToInt (A.not none) * (1 + boolToInt strong)) * 0.5


-- Repa component --------------------------------------------------------------

-- | Select indices of strong edges.
--   TODO: If would better if we could merge this into the above stage, and
--         record the strong edge during non-maximum suppression, but Repa
--         doesn't provide a fused mapFilter primitive yet.
--
selectStrong
    :: R.Array A R.DIM2 Float
    -> IO (R.Array U R.DIM1 Int)
selectStrong img = R.selectP match process (R.size $ R.extent img)
  where
    {-# INLINE match   #-}
    {-# INLINE process #-}
    match ix    = img `R.unsafeLinearIndex` ix == edge Strong
    process ix  = ix


-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
--
wildfire
    :: R.Array A R.DIM2 Float           -- ^ Image with strong and weak edges set.
    -> R.Array U R.DIM1 Int             -- ^ Array containing flat indices of strong edges.
    -> IO (R.Array U R.DIM2 Word8)

wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO
        return  $ sh `seq` vec `seq` R.fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        shImg           = R.extent img
        vStrong         = R.toUnboxed arrStrong

        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong' <- V.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)

                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- V.unsafeFreeze vImg
                return  (R.extent img, vImg')


        burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top == 0
         = return ()

         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (R.Z R.:. y R.:. x) = R.fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push ix t =
                      if R.inShape shImg ix
                         then pushWeak vImg vStack ix t
                         else return t

                VM.write vImg n 255
                 >>  push (R.Z R.:. y - 1 R.:. x - 1) top'
                 >>= push (R.Z R.:. y - 1 R.:. x    )
                 >>= push (R.Z R.:. y - 1 R.:. x + 1)

                 >>= push (R.Z R.:. y     R.:. x - 1)
                 >>= push (R.Z R.:. y     R.:. x + 1)

                 >>= push (R.Z R.:. y + 1 R.:. x - 1)
                 >>= push (R.Z R.:. y + 1 R.:. x    )
                 >>= push (R.Z R.:. y + 1 R.:. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = R.toIndex (R.extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst == 0
                  && xSrc == edge Weak
                 then do
                        VM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                        return (top + 1)

                 else   return top
{-# NOINLINE wildfire #-}

