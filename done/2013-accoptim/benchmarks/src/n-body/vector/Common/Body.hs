--
-- Simulation bodies with mass
--

module Common.Body (

  -- * Types
  Velocity, Accel, PointMass, Body,

  -- * Calculations
  accel, advanceBody,

  -- ** Getters
  pointMassOfBody, velocityOfBody, accelOfBody, positionOfPointMass,
  massOfPointMass,

  -- ** Setters
  unitBody, setMassOfBody, setAccelOfBody, setStartVelOfBody,

) where

import Common.Type
import Common.Util


-- Acceleration ----------------------------------------------------------------
--
-- | Calculate the acceleration on a point due to some other point as an inverse
--   separation-squared relation. The force on a body i caused by its
--   gravitational attraction to a body j is given by:
--
--                m_i m_j     r_ij
--      f_ij = G --------- . ------
--                |r_ij|^2   |r_ij|
--
--   The total force on a body F is given by this interaction to all other
--   bodies:
--
--      F_i = Sum_{i/=j} f_ij
--
--                               m_j r_ij
--          = G m_i . Sum_{j/=i} --------
--                                |r_ij|^3
--
--   As the bodies approach each other, the force between them grows without
--   bound, which is an undesirable situation for numerical integration. Since
--   collisions between bodies are precluded, a softening factor (epsilon^2 > 0)
--   is added:
--
--                                  m_j r_ij
--      F_i = G m_i . Sum -----------------------------
--                        ( |r_ij|^2 + epsilon^2) ^ 3/2
--
--   Note that the condition (i /= j) is no longer required, because (f_ii = 0)
--   when (epsilon^2 > 0). The softening factor models the interaction between
--   two Plummer point masses: bodies that behave as if they were spherical
--   galaxies (and thus may pass through each other).
--
--   To integrate over time, we need the acceleration (a_i = F_i / m_i), and so
--   the above can be simplified by removing m_i from the RHS. This function
--   computes the component of the Sum for two bodies i and j.
--
accel   :: R                    -- ^ Smoothing parameter
        -> Body                 -- ^ The point being accelerated
        -> Body                 -- ^ Neighbouring point
        -> Accel
{-# INLINE accel #-}
accel epsilon bodyi bodyj = s *. r
  where
    pmi         = pointMassOfBody bodyi
    pmj         = pointMassOfBody bodyj
    mj          = massOfPointMass pmj

    r           = positionOfPointMass pmj .-. positionOfPointMass pmi
    rsqr        = dot r r + epsilon * epsilon
    invr        = 1 / sqrt rsqr
    invr3       = invr * invr * invr

    s           = mj * invr3


-- Body ------------------------------------------------------------------------
--

-- | Make a stationary Body of unit mass
--
{-# INLINE unitBody #-}
unitBody :: Vec R -> Body
unitBody pos = (pointmass, vec 0, vec 0)
  where
    pointmass = (pos, 1)


-- | Take the Velocity of a Body
--
{-# INLINE velocityOfBody #-}
velocityOfBody :: Body -> Velocity
velocityOfBody body = vel
  where
    (_, vel, _) = body


-- | Take the Acceleration of a Body
--
{-# INLINE accelOfBody #-}
accelOfBody :: Body -> Accel
accelOfBody body = acc
  where
    (_, _, acc) = body


-- | Take the PointMass of a Body
--
{-# INLINE pointMassOfBody #-}
pointMassOfBody :: Body -> PointMass
pointMassOfBody body = mp
  where
    (mp, _, _)  = body


-- | Take the position or mass of a PointMass
--
{-# INLINE positionOfPointMass #-}
positionOfPointMass :: PointMass -> Position
positionOfPointMass = fst

{-# INLINE massOfPointMass #-}
massOfPointMass :: PointMass -> Mass
massOfPointMass = snd


-- | Set the mass of a Body.
--
{-# INLINE setMassOfBody #-}
setMassOfBody :: Mass -> Body -> Body
setMassOfBody mass body = (pointmass, vel, acc)
  where
    vel         = velocityOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass (pointMassOfBody body)
    pointmass   = (pos, mass)


-- | Set the acceleration of a Body.
--
{-# INLINE setAccelOfBody #-}
setAccelOfBody :: Accel -> Body -> Body
setAccelOfBody acc body = (pm, vel, acc)
  where
    pm          = pointMassOfBody body
    vel         = velocityOfBody body


-- | Set the starting velocity of a Body.
--   It is set to rotate around the origin, with the speed proportional
--   to the sqrt of the distance from it. This seems to make nice simulations.
--
{-# INLINE setStartVelOfBody #-}
setStartVelOfBody :: R -> Body -> Body
setStartVelOfBody startVel body = (pm, vel'', acc)
  where
    pm          = pointMassOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass pm

    pos'        = normalise pos
    vel'        = (y', -x', z')
    vel''       = (sqrt (magnitude pos) * startVel) *. vel'

    (x',y',z')  = pos'


-- | Advance a body forwards in time.
--
{-# INLINE advanceBody #-}
advanceBody :: Time -> Body -> Body
advanceBody time body = ( pm', vel', acc )
  where
    pm          = pointMassOfBody body
    pos         = positionOfPointMass pm
    vel         = velocityOfBody body
    acc         = accelOfBody body
    mass        = massOfPointMass pm

    pm'         = (pos', mass)
    pos'        = pos .+. time *. vel
    vel'        = vel .+. time *. acc

