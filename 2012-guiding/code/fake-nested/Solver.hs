{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Solver 
        (foo)
where
import Data.Array.Repa          as R
import Data.Array.Repa.Eval     as R


foo :: Array U DIM1 Int -> Array U DIM2 Int
    -> Array U DIM1 Int
foo xs ys
 = let  ys' :: Array U DIM2 Int
        !ys' = suspendedComputeP $ R.map (+ 1) ys
   in   suspendedComputeP $ R.map (\i -> ys' ! (Z :. i :. i)) xs
        
