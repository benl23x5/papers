
module Common.World (

  World(..), advanceBodies, advanceWorld

) where

import Common.Type
import Common.Util
import Common.Body

import Data.Vector.Unboxed	        (Vector)
import qualified Data.Vector.Unboxed	as V


data World
  = World
  {
    worldBodies :: !(Vector Body)                       -- ^ Bodies in the simulation
  , worldSteps  :: {-# UNPACK #-} !Int                  -- ^ Number of steps taken in the simulation so far
  , worldTime   :: {-# UNPACK #-} !Time                 -- ^ Current simulation time
  }


-- | Move bodies under the influence of acceleration
--
{-# INLINE advanceBodies #-}
advanceBodies
    :: (Vector Body -> Vector Accel)            -- ^ Function to compute accelerations at each point
    -> Time                                     -- ^ Time step
    -> (Vector Body)                            -- ^ Bodies
    -> (Vector Body)
advanceBodies calcAccels timeStep bodies
  = let
        -- Calculate the accelerations on each body.
        accels          = calcAccels bodies

        -- Apply the accelerations to the bodies and advance them
        advance b a     = let m         = massOfPointMass (pointMassOfBody b)
                              a'        = m *. a
                          in advanceBody timeStep (setAccelOfBody a' b)
    in
    V.zipWith advance bodies accels


-- | Advance a cluster of bodies forward in time
--
{-# INLINE advanceWorld #-}
advanceWorld
    :: (Time -> Vector Body -> Vector Body)     -- ^ Function to update body positions
    -> Time
    -> World
    -> World
advanceWorld advance timeStep world
  = let
        -- Update the bodies
        bodies' = advance timeStep (worldBodies world)

        -- Update the world
        steps'  = worldSteps world + 1
        time'   = worldTime  world + timeStep

    in  world   { worldBodies   = bodies'
                , worldSteps    = steps'
                , worldTime     = time' }

