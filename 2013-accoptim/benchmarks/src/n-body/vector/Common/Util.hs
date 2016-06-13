
module Common.Util (

  magnitude, dot, normalise, vec,

  (.+.), (.-.), (.*.), (+.), (-.), (*.),

) where

import Common.Type

-- | The magnitude of a vector.
--
{-# INLINE magnitude #-}
magnitude :: Floating a => Vec a -> a
magnitude v = sqrt (dot v v)


-- | Dot product of a vector
--
{-# INLINE dot #-}
dot :: Num a => Vec a -> Vec a -> a
dot v1 v2
  = let (x1,y1,z1) = v1
        (x2,y2,z2) = v2
    in
    x1 * x2 + y1 * y2 + z1 * z2


-- | Normalise a vector, so it has a magnitude of 1.
--
{-# INLINE normalise #-}
normalise :: Floating a => Vec a -> Vec a
normalise v = (1 / magnitude v) *. v

-- | Replicate a value into a vector
--
{-# INLINE vec #-}
vec :: a -> Vec a
vec x = (x,x,x)

-- | Basic arithmetic component-wise
--
infixl 7 .*.
infixl 6 .+.
infixl 6 .-.

{-# INLINE (.+.) #-}
{-# INLINE (.-.) #-}
{-# INLINE (.*.) #-}
(.+.), (.-.), (.*.) :: Num a => Vec a -> Vec a -> Vec a
(.+.) = vzipWith (+)
(.-.) = vzipWith (-)
(.*.) = vzipWith (*)

-- | Apply a scalar value component-wise to each element of the vector
--
infixl 7 *.
infixl 6 +.
infixl 6 -.

{-# INLINE (+.) #-}
{-# INLINE (-.) #-}
{-# INLINE (*.) #-}
(+.), (-.), (*.) :: Num a => a -> Vec a -> Vec a
(+.) c = vmap (c+)
(-.) c = vmap (c-)
(*.) c = vmap (c*)

-- | Arithmetic lifted to our vector type. As far as possible, want to gloss
--   over whether we are calculating in 2D or 3D.
--
{-# INLINE vmap #-}
vmap :: (a -> b) -> Vec a -> Vec b
vmap f v
  = let (x1,y1,z1) = v
    in
    (f x1, f y1, f z1)

{-# INLINE vzipWith #-}
vzipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
vzipWith f v1 v2
  = let (x1,y1,z1) = v1
        (x2,y2,z2) = v2
    in
    (f x1 x2, f y1 y2, f z1 z2)

