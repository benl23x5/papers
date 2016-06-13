{-# LANGUAGE TypeOperators #-}

-- | Canny edge detector
--
--   NOTE: for best performance this needs to be compiled with the following GHC options:
--         -fllvm -optlo-O3 -Odph -fno-liberate-case
--         -funfolding-use-threshold100 -funfolding-keeness-factor100
--

import Accelerate
import Repa

-- standard library
import Prelude                                          as P
import Criterion.Main                                   ( defaultMain, bgroup, bench, whnf )
import Data.List
import System.Environment

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.CUDA                       as A
import qualified Data.Array.Accelerate.IO               as A
import qualified Data.Array.Repa.IO.BMP                 as R
import qualified Data.Array.Repa.Repr.Unboxed           as R


-- Main routine ----------------------------------------------------------------

main :: IO ()
main = do
  (args, rest)  <- break ("--" `isPrefixOf`) `fmap` getArgs
  let opts      = if P.null rest then [] else P.tail rest

  withArgs opts $ case args of

    [fileIn, fileOut]
      -> canny 50 100 fileIn fileOut

    [low, high, fileIn, fileOut]
      | [(threshLow,[])]      <- reads low
      , [(threshHigh,[])]     <- reads high
      -> canny threshLow threshHigh fileIn fileOut

    _ -> putStrLn "usage: canny [threshLow threshHigh] fileIn.bmp fileOut.bmp [-- Criterion options]"


canny :: Float -> Float -> FilePath -> FilePath -> IO ()
canny threshLow threshHigh fileIn fileOut = do

  -- Read in the image file
  --
  img   <- either (error . show) id `fmap` A.readImageFromBMP fileIn

  -- Set up the Accelerate kernel stages, which identify strong and weak edges
  -- in the image
  --
  let low               = constant threshLow
      high              = constant threshHigh

      grey              = toGreyscale
      blurred           = gaussianY . gaussianX . grey
      magdir            = gradientMagDir low . blurred
      suppress          = nonMaximumSuppression low high . magdir
      stage1 x          = let s = suppress x
                          in (s, selectStrong s)

      (image, strong)   = run $ A.lift (stage1 (use img))

      -- Set up partial results so that we can benchmark individual stages
      --
      grey'             = run $ toGreyscale (use img)
      blurX'            = run $ gaussianX (use grey')
      blurred'          = run $ gaussianY (use blurX')
      magdir'           = run $ gradientMagDir low (use blurred')
      suppress'         = run $ nonMaximumSuppression low high (use magdir')

  -- Now use Repa to trace out weak edges connected to strong edges. This forces
  -- the pipeline so that all kernels are compiled before benchmarking begins
  --
  edges <- wildfire (A.toRepa image) (A.toRepa strong)
  R.writeImageToBMP fileOut (R.zip3 edges edges edges)

  defaultMain
    [ bgroup "kernels"
      [ bench "greyscale"   $ whnf (run1 grey) img
      , bench "blur-x"      $ whnf (run1 gaussianX) grey'
      , bench "blur-y"      $ whnf (run1 gaussianY) blurX'
      , bench "grad-x"      $ whnf (run1 gradientX) blurred'
      , bench "grad-y"      $ whnf (run1 gradientY) blurred'
      , bench "mag-orient"  $ whnf (run1 (gradientMagDir low)) blurred'
      , bench "suppress"    $ whnf (run1 (nonMaximumSuppression low high)) magdir'
      , bench "select"      $ whnf (run1 selectStrong) suppress'
      ]

    , bgroup "canny"
      [ bench "run"     $ whnf (run . (P.snd . stage1)) (use img)
      , bench "run1"    $ whnf (run1 (P.snd . stage1)) img
      ]
    ]

