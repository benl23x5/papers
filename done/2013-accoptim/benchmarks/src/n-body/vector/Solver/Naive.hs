
-- | Naive n^2 computation of accelerations.
--
module Solver.Naive (calcAccels)
  where

import Common.Type
import Common.Body
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V

-- | Calculate accelerations on these point in a naive O(n^2) way
--
{-# INLINE calcAccels #-}
calcAccels :: R -> Vector Body -> Vector Accel
calcAccels epsilon bodies
  = V.map (calcAccel epsilon bodies) bodies

{-# INLINE calcAccel #-}
calcAccel :: R -> Vector Body -> Body -> Accel
calcAccel epsilon bodies body
 = let (xs, ys, zs)     = V.unzip3 $ V.map (accel epsilon body) bodies
   in  (V.sum xs, V.sum ys, V.sum zs)

