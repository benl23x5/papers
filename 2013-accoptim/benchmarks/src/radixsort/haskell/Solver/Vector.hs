
module Solver.Vector
  where

import Data.Vector.Unboxed              as V
import Data.Vector.Algorithms.Radix     as V

sort :: (Unbox e, Radix e) => Vector e -> Vector e
sort vec = create $ do
  mvec  <- V.thaw vec
  V.sort mvec
  return mvec

