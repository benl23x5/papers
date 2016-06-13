module Solver.Vector
  where

import Data.Vector.Storable             as V

dotp :: Vector Float -> Vector Float -> Float
dotp xs ys =
  V.sum (V.zipWith (*) xs ys)

