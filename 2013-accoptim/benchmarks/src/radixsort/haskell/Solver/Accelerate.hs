{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Radix sort for a subclass of element types
--

module Solver.Accelerate
  where

import Prelude                  as P
import Data.Array.Accelerate    as A

import Data.Bits


-- Radix sort
-- ----------

class Elt e => Radix e where
  passes :: e {- dummy -} -> Int
  radix  :: Exp Int -> Exp e -> Exp Int

instance Radix Int32 where
  passes        = bitSize
  radix i e     = i ==* (passes' - 1) ? (radix' (e `xor` minBound), radix' e)
    where
      radix' x = A.fromIntegral $ (x `A.shiftR` i) .&. 1
      passes'  = constant (passes (undefined :: Int32))

instance Radix Word32 where
  passes        = bitSize
  radix i e     = A.fromIntegral $ (e `A.shiftR` i) .&. 1


-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
sort :: Radix a => Acc (Vector a) -> Acc (Vector a)
sort = sortBy id

sortBy :: forall a r. (Elt a, Radix r) => (Exp a -> Exp r) -> Acc (Vector a) -> Acc (Vector a)
sortBy rdx arr = foldr1 (>->) (P.map radixPass [0..p-1]) arr
  where
    p = passes (undefined :: r)
    --
    deal f x      = let (a,b)   = unlift x in (f ==* 0) ? (a,b)
    radixPass k v = let k'      = unit (constant k)
                        flags   = A.map (radix (the k') . rdx) v
                        idown   = prescanl (+) 0 . A.map (xor 1)        $ flags
                        iup     = A.map (size v - 1 -) . prescanr (+) 0 $ flags
                        index   = A.zipWith deal flags (A.zip idown iup)
                    in
                    permute const v (\ix -> index1 (index!ix)) v

