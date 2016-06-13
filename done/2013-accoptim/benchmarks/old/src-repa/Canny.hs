{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, MagicHash, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Canny edge detector.
--
--   NOTE: for best performance this needs to be compiled with the following GHC options:
--         -fllvm -optlo-O3 -Odph -fno-liberate-case
--         -funfolding-use-threshold100 -funfolding-keeness-factor100
--
module Canny where

import Data.List
import Data.Word
import Data.Int
import Criterion.Main
import Control.Monad
import System.Environment
import Data.Array.Repa 				as R
import Data.Array.Repa.Repr.Unboxed             as U
import Data.Array.Repa.Repr.Cursored            as C
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Specialised.Dim2
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.IO.BMP
import GHC.Exts
import qualified Data.Vector.Unboxed.Mutable	as VM
import qualified Data.Vector.Unboxed		as V
import Prelude					as P
import Prelude                                  hiding (compare)


type Image a	= Array U DIM2 a

-- Constants ------------------------------------------------------------------
orientUndef	= 0	:: Word8
orientPosDiag	= 64	:: Word8
orientVert	= 128	:: Word8
orientNegDiag	= 192	:: Word8
orientHoriz	= 255	:: Word8

data Edge	= None | Weak | Strong
edge None	= 0 	:: Word8
edge Weak	= 128 	:: Word8
edge Strong	= 255	:: Word8


-- Main routine ---------------------------------------------------------------
main
 = do	args	<- getArgs
        let (threshLow, threshHigh, fileIn, fileOut) = case args of
              [fi, fo]
                -> (50, 100, fi, fo)

              [tl, th, fi, fo]
                -> (read tl, read th, fi, fo)

              _ -> error
                 $ concat [ "repa-canny [<threshLow::Int> <threshHigh::Int>]"
                          , " <fileIn.bmp> <fileOut.bmp>" ]

        -- Read the input image, and run the through once to produce the
        -- completed output edges
        --
     	arrInput        <- liftM (either (error . show) id)
		        $  readImageFromBMP fileIn

        arrGrey         <- toGreyScale arrInput
        arrBlurredX     <- blurSepX arrGrey
        arrBlurred      <- blurSepY arrBlurredX
        arrDX           <- gradientX arrBlurred
        arrDY           <- gradientY arrBlurred
        arrMagOrient    <- gradientMagOrient threshLow arrDX arrDY
        arrSuppress     <- suppress threshLow threshHigh arrMagOrient
        arrStrong       <- selectStrong arrSuppress
        arrEdges        <- wildfire arrSuppress arrStrong

	writeImageToBMP fileOut (U.zip3 arrEdges arrEdges arrEdges)

        -- Now, benchmark each stage separately with criterion
        --
        let force fn    =  do
              arr       <- fn
              arr `deepSeqArray` return ()

        withArgs [] $ defaultMain [ bgroup "canny"
          [ bench "greyscale"   $ whnfIO (force (toGreyScale arrInput))
          , bench "blur-x"      $ whnfIO (force (blurSepX arrGrey))
          , bench "blur-y"      $ whnfIO (force (blurSepY arrBlurredX))
          , bench "deriv-x"     $ whnfIO (force (gradientX arrBlurred))
          , bench "deriv-y"     $ whnfIO (force (gradientY arrBlurred))
          , bench "mag-orient"  $ whnfIO (force (gradientMagOrient threshLow arrDX arrDY))
          , bench "suppress"    $ whnfIO (force (suppress threshLow threshHigh arrMagOrient))
          , bench "strong"      $ whnfIO (force (selectStrong arrSuppress))
          , bench "wildfile"    $ whnfIO (force (wildfire arrSuppress arrStrong))
          ]
         ]


-------------------------------------------------------------------------------
-- | RGB to greyscale conversion.
toGreyScale :: Monad m => Image (Word8, Word8, Word8) -> m (Image Float)
toGreyScale arr
        = computeP
        $ R.map (* 255)
        $ R.map floatLuminanceOfRGB8 arr
{-# NOINLINE toGreyScale #-}


-- | Separable Gaussian blur in the X direction.
blurSepX :: Monad m => Image Float -> m (Image Float)
blurSepX arr
        = computeP
        $ forStencil2  BoundClamp arr
          [stencil2|	1 4 6 4 1 |]
{-# NOINLINE blurSepX #-}


-- | Separable Gaussian blur in the Y direction.
blurSepY :: Monad m => Image Float -> m (Image Float)
blurSepY arr
	= computeP
	$ R.cmap (/ 256)
	$ forStencil2  BoundClamp arr
	  [stencil2|	1
	 		4
			6
			4
			1 |]
{-# NOINLINE blurSepY #-}


-- | Compute gradient in the X direction.
gradientX :: Monad m => Image Float -> m (Image Float)
gradientX img
 	= computeP
    	$ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]
{-# NOINLINE gradientX #-}


-- | Compute gradient in the Y direction.
gradientY :: Monad m => Image Float -> m (Image Float)
gradientY img
	= computeP
	$ forStencil2 BoundClamp img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |]
{-# NOINLINE gradientY #-}


-- | Classify the magnitude and orientation of the vector gradient.
gradientMagOrient
        :: Monad m
        => Float -> Image Float -> Image Float -> m (Image (Float, Word8))

gradientMagOrient !threshLow dX dY
        = computeP
        $ R.zipWith magOrient dX dY

 where	magOrient :: Float -> Float -> (Float, Word8)
	magOrient !x !y
		= (magnitude x y, orientation x y)
	{-# INLINE magOrient #-}

	magnitude :: Float -> Float -> Float
	magnitude !x !y
		= sqrt (x * x + y * y)
        {-# INLINE magnitude #-}

        {-# INLINE orientation #-}
	orientation :: Float -> Float -> Word8
	orientation !x !y

	 -- Don't bother computing orientation if vector is below threshold.
 	 | x >= negate threshLow, x < threshLow
 	 , y >= negate threshLow, y < threshLow
 	 = orientUndef

	 | otherwise
	 = let	-- Determine the angle of the vector and rotate it around a bit
		-- to make the segments easier to classify.
		!d	= atan2 y x
		!dRot	= (d - (pi/8)) * (4/pi)

		-- Normalise angle to beween 0..8
		!dNorm	= if dRot < 0 then dRot + 8 else dRot

		-- Doing explicit tests seems to be faster than using the FP floor function.
	   in fromIntegral
               $ I# (if dNorm >= 4
		     then if dNorm >= 6
	   		  then if dNorm >= 7
			  	then 255#               -- 7
				else 192#               -- 6

			  else if dNorm >= 5
				then 128#               -- 5
				else 64#                -- 4

		     else if dNorm >= 2
			then if dNorm >= 3
				then 255#               -- 3
				else 192#               -- 2

			else if dNorm >= 1
				then 128#               -- 1
				else 64#)               -- 0
{-# NOINLINE gradientMagOrient #-}


-- | Suppress pixels that are not local maxima, and use the magnitude to classify maxima
--   into strong and weak (potential) edges.
suppress :: Monad m => Float -> Float -> Image (Float, Word8) -> m (Image Word8)
suppress !threshLow !threshHigh !dMagOrient
 = computeP
 $ makeBordered2
        (extent dMagOrient) 1
 	(makeCursored (extent dMagOrient) id addDim comparePts)
 	(fromFunction (extent dMagOrient) (const 0))

 where	{-# INLINE comparePts #-}
	comparePts d@(sh :. i :. j)
	 | o == orientUndef     = edge None
         | o == orientHoriz	= isMax (getMag (sh :. i   :. j-1)) (getMag (sh :. i   :. j+1))
         | o == orientVert	= isMax (getMag (sh :. i-1 :. j))   (getMag (sh :. i+1 :. j))
         | o == orientNegDiag   = isMax (getMag (sh :. i-1 :. j-1)) (getMag (sh :. i+1 :. j+1))
         | o == orientPosDiag   = isMax (getMag (sh :. i-1 :. j+1)) (getMag (sh :. i+1 :. j-1))
         | otherwise            = edge None

         where
          !o 		= getOrient d
          !m		= getMag    (Z :. i :. j)

	  getMag 	= fst . (R.unsafeIndex dMagOrient)
	  getOrient	= snd . (R.unsafeIndex dMagOrient)

	  {-# INLINE isMax #-}
          isMax !intensity1 !intensity2
            | m < threshLow 	= edge None
            | m < intensity1 	= edge None
            | m < intensity2 	= edge None
	    | m < threshHigh	= edge Weak
	    | otherwise		= edge Strong
{-# NOINLINE suppress #-}


-- | Select indices of strong edges.
--   TODO: If would better if we could medge this into the above stage, and
--         record the strong edge during non-maximum suppression, but Repa
--         doesn't provide a fused mapFilter primitive yet.
selectStrong :: Monad m => Image Word8 -> m (Array U DIM1 Int)
selectStrong img
 = let 	vec             = toUnboxed img

	match ix	= vec `V.unsafeIndex` ix == edge Strong
        {-# INLINE match #-}

	process' ix	= ix
        {-# INLINE process' #-}

   in	selectP match process' (size $ extent img)
{-# NOINLINE selectStrong #-}


-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
wildfire
        :: Image Word8	     -- ^ Image with strong and weak edges set.
	-> Array U DIM1 Int  -- ^ Array containing flat indices of strong edges.
	-> IO (Image Word8)

wildfire img arrStrong
 = do	(sh, vec)	<- wildfireIO
	return	$ sh `seq` vec `seq` fromUnboxed sh vec

 where	lenImg		= R.size $ R.extent img
	lenStrong	= R.size $ R.extent arrStrong
	vStrong		= toUnboxed arrStrong

	wildfireIO
  	 = do	-- Stack of image indices we still need to consider.
		vStrong' <- V.thaw vStrong
		vStack	 <- VM.grow vStrong' (lenImg - lenStrong)

		-- Burn in new edges.
		vImg	<- VM.unsafeNew lenImg
		VM.set vImg 0
		burn vImg vStack lenStrong
		vImg'	<- V.unsafeFreeze vImg
		return	(extent img, vImg')


	burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
	burn !vImg !vStack !top
	 | top == 0
	 = return ()

	 | otherwise
	 = do	let !top'		=  top - 1
		n			<- VM.unsafeRead vStack top'
		let (Z :. y :. x)	= fromIndex (R.extent img) n

		let {-# INLINE push #-}
		    push t		= pushWeak vImg vStack t

		VM.write vImg n (edge Strong)
	    	 >>  push (Z :. y - 1 :. x - 1) top'
	    	 >>= push (Z :. y - 1 :. x    )
	    	 >>= push (Z :. y - 1 :. x + 1)

	    	 >>= push (Z :. y     :. x - 1)
	    	 >>= push (Z :. y     :. x + 1)

	    	 >>= push (Z :. y + 1 :. x - 1)
	    	 >>= push (Z :. y + 1 :. x    )
	    	 >>= push (Z :. y + 1 :. x + 1)

	    	 >>= burn vImg vStack

	-- If this ix is weak in the source then set it to strong in the
	-- result and push the ix onto the stack.
	{-# INLINE pushWeak #-}
	pushWeak vImg vStack ix top
	 = do	let n		= toIndex (extent img) ix
		xDst		<- VM.unsafeRead vImg n
		let xSrc	= img `R.unsafeIndex` ix

		if   xDst == edge None
		  && xSrc == edge Weak
		 then do
			VM.unsafeWrite vStack top (toIndex (extent img) ix)
			return (top + 1)

		 else	return top
{-# NOINLINE wildfire #-}


