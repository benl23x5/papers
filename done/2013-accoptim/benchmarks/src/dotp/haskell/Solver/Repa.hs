
module Solver.Repa
  where

import Data.Array.Repa                  as R

dotp :: Array U DIM1 Float -> Array U DIM1 Float -> IO Float
dotp xs ys
  = sumAllP
  $ R.zipWith (*) xs ys

