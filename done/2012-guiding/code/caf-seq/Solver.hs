{-# LANGUAGE BangPatterns #-}

module Solver 
        (foo, arr, bar)
where
import Data.Array.Repa  as R


foo :: Array U DIM1 Int -> Array U DIM2 Int -> IO (Array U DIM1 Int)
foo xs !ys
 = computeP $ R.map (\i -> ys `R.unsafeIndex` (Z :. i :. i)) xs


arr :: Array U DIM2 Int
arr = R.fromListUnboxed (Z :. 2 :. 2) [1, 2, 3, 4]


bar :: Array U DIM1 Int -> IO (Array U DIM1 Int)
bar !xs
 = arr `seq` computeP $ R.map (\i -> arr `R.unsafeIndex` (Z :. i :. i)) xs
