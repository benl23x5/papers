module Solver.Accelerate
  where

import Prelude                          as P
import Data.Array.Accelerate            as A


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys =
  A.fold (+) 0 (A.zipWith (*) xs ys)

