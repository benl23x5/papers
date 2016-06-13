
module RepaLite
        ( Array  (..)
        , deepSeqArray
        , Shape  (..)
        , DIM1   (..)
        , DIM2   (..)
        , extent
        , force
        , index
        , map
        , zipWith
        , delay
        , traverse
        , fromList)
where
import Data.Vector.Unboxed                      (Vector, Unbox)
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as MV
import GHC.Base (quotInt, remInt)
import Prelude                                  hiding (map, zipWith)
import System.IO.Unsafe


-- Array ----------------------------------------------------------------------
data Array sh e
        = Manifest !sh !(Vector e)
        | Delayed  !sh  (sh -> e)

instance (Show sh, Show e, Unbox e) => Show (Array sh e) where
 show (Manifest sh vec) 
        = "Manifest (" ++ show sh ++ ") (" ++ show vec ++ ")"

 show (Delayed sh f)
        = "Delayed ("  ++ show sh ++ ") <FUNCTION>"


-- | Ensure the structure of an array is fully evaluated.
deepSeqArray :: Shape sh => Array sh a -> b -> b
deepSeqArray arr x
 = case arr of
        Manifest sh vec 
         -> sh `deepSeqShape` vec `seq` x

        Delayed sh f
         -> sh `deepSeqShape` f `seq` x
{-# INLINE deepSeqArray #-}


-- Shape ----------------------------------------------------------------------
class Shape sh where
  fromIndex    :: sh -> Int -> sh
  toIndex      :: sh -> sh -> Int
  size         :: sh -> Int
  deepSeqShape :: sh -> b -> b

data DIM1 
        = DIM1 !Int
        deriving Show

data DIM2 
        = DIM2 !Int !Int
        deriving Show


instance Shape DIM1 where

  fromIndex (DIM1 _) ix         = (DIM1 ix)
  {-# INLINE fromIndex #-}

  toIndex   (DIM1 _) (DIM1 x)   = x
  {-# INLINE toIndex #-}

  size (DIM1 w)                 = w
  {-# INLINE size #-}

  deepSeqShape (DIM1 _) x       = x 
  {-# INLINE deepSeqShape #-}

instance Shape DIM2 where

  fromIndex (DIM2 h _) ix
        = DIM2 (ix `quotInt` h) (ix `remInt` h)
  {-# INLINE fromIndex #-}

  toIndex   (DIM2 h w) (DIM2 y x)
        = y * h + x
  {-# INLINE toIndex #-}

  size      (DIM2 h w)          = h * w
  {-# INLINE size #-}

  deepSeqShape (DIM2 _ _) x     = x
  {-# INLINE deepSeqShape #-}


-- Operators ------------------------------------------------------------------
-- | Convert a delayed array to a manifest one
force   :: (Shape sh, Unbox e)
        => Array sh e -> Array sh e
force arr
  = unsafePerformIO
  $ do  (sh, vec)       <- forceIO arr
        return $ sh `seq` vec `seq` 
                Manifest sh vec

 where  forceIO arr'
         = case arr' of
                Manifest sh vec    
                 ->     return  (sh, vec)

                Delayed sh f    
                 -> do  mvec    <- MV.unsafeNew (size sh)
                        fillIO (size sh) mvec (f . fromIndex sh)
                        vec     <- V.unsafeFreeze mvec
                        return  (sh, vec)
{-# INLINE force #-}


fillIO  :: Unbox a 
        => Int -> MV.IOVector a -> (Int -> a) -> IO ()
fillIO size mvec f
 = fill 0
 where  fill ix 
         | ix >= size   
         = return ()

         | otherwise
         = do   MV.unsafeWrite mvec ix (f ix)
                fill (ix + 1)
        {-# INLINE fill #-}

{-# INLINE fillIO #-}


-- | Take the extent of an array.
extent :: Array sh e -> sh
extent arr
 = case arr of
        Manifest sh _   -> sh
        Delayed  sh _   -> sh
{-# INLINE extent #-}


-- | Lookup a single element from an array.
index   :: (Shape sh, Unbox e) 
        => Array sh e -> sh -> e

index arr ix
 = case arr of
        Manifest sh vec -> V.unsafeIndex vec (toIndex sh ix)
        Delayed  sh f   -> f ix
{-# INLINE index #-}


-- | Apply a worker function to every element in an array.
map     :: (Shape sh, Unbox a, Unbox b)
        => (a -> b) -> Array sh a -> Array sh b
map f arr
 = let  (sh, g)  = delay arr
   in   Delayed sh (f . g)
{-# INLINE map #-}


-- | Combine corresponding elemenets of two arrays
zipWith  :: (Shape sh, Unbox a, Unbox b)
         => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f arr1 arr2
 = let  (sh1,  f1)       = delay arr1
        (_sh2, f2)       = delay arr2

        get ix          = f (f1 ix) (f2 ix)
        {-# INLINE get #-}

   in   Delayed sh1 get
{-# INLINE zipWith #-}


-- | Convert an array to delayed form
delay     :: (Shape sh, Unbox e)
          => Array sh e
          -> (sh, sh -> e)
delay arr
 = case arr of
        Delayed  sh f    
         -> (sh, f)

        Manifest sh vec  
         -> let {-# INLINE get #-}
                get ix  = V.unsafeIndex vec (toIndex sh ix)
            in  (sh, get)
{-# INLINE delay #-}


-- | Generic traversal over an array.
traverse  :: (Shape sh1, Shape sh2, Unbox a)
          => (sh1 -> sh2)
          -> ((sh1 -> a) -> sh2 -> b)
          -> Array sh1 a
          -> Array sh2 b

traverse shapeFn elemFn arr
 = let  (sh, f) = delay arr
   in   Delayed (shapeFn sh) (elemFn f)
{-# INLINE traverse #-}


-- | Convert a list to an array.
fromList 
        :: (Shape sh, Unbox a) 
        => sh -> [a] -> Array sh a

fromList sh xs
        = Manifest sh (V.fromList xs)
{-# INLINE fromList #-}
