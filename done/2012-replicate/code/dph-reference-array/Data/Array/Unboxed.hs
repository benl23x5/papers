
-- | This module exports all the unboxed vector primtiives we use.
--   A parallel implementation of our array representation would 
--   need to provide parallel implementations of these primtives.
module Data.Array.Unboxed 
        ( Vector
        , Unbox
        , singleton
        , replicate
        , replicates
        , (++)
        , enumFromN
        , ranges
        , concat
        , zip
        , empty
        , null
        , length
        , (!)
        , init
        , slice
        , pack
        , combine2
        , backpermute
        , backpermuteDft
        , map
        , zipWith
        , sum
        , scanl
        , toList
        , fromList
        , convert)
where
import Data.Vector.Unboxed
import qualified Prelude        as P
import Prelude (Int, Bool, ($), fst, snd)


-- | Segmented replicate.
replicates :: Unbox a => Vector Int -> Vector a -> Vector a
replicates reps elems
        = concat 
        $ P.zipWith replicate 
                    (toList reps)
                    (toList elems)

-- | Ranges. 
--
--   @ranges [3 4 1] = [0 1 2  0 1 2 3  0]@
ranges :: Vector Int -> Vector Int
ranges lengths
        = concat
        $ P.map (enumFromN 0) 
        $ toList lengths


-- | Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
backpermuteDft 
        :: Unbox e
        => Int                  -- ^ Length of result array.
        -> (Int -> e)           -- ^ Initialiser function.
        -> Vector (Int,e)       -- ^ Index-value pairs.
        -> Vector e

backpermuteDft n inits
        = update (map inits (enumFromN 0 n))


-- | Extract all elements from an array according to a given flag array
--
--   O(length result)
pack :: Unbox a => Vector a -> Vector Bool -> Vector a
pack xs fs
        = map fst $ filter snd $ zip xs fs


-- | Combine two arrays based on a flags vector.
--   Parallelising this involves precomputing a selector that encodes
--   a plan of what each thread should do at runtime. This is passed 
--   along with the flags vector. See the real DPH implementation for details.
--
--   O(length result)
combine2 :: Unbox a => Vector Bool -> Vector a -> Vector a -> Vector a
combine2 flags vec1 vec2
 = let  go [] [] []                         = []
        go (P.True  : bs) (x : xs) ys       = x : go bs xs ys
        go (P.False : bs) xs       (y : ys) = y : go bs xs ys
        go _ _ _ = P.error "combine: length mismatch"
 
   in   fromList $ go (toList flags) (toList vec1) (toList vec2)

