{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables #-}
-- Little test of the shape type classes

module Replicate where
import Prelude hiding( replicate )

infixl 3 :.
data Z            = Z
data tail :. head = tail :. head

type DIM0 = Z
type DIM1 = DIM0:.Int
type DIM2 = DIM1:.Int
type DIM3 = DIM2:.Int

data Array sh e = Array sh e
extent (Array sh _) = sh

class Elt e where

class Shape sh where
  rank  :: sh -> Int
  size  :: sh -> Int        -- Number of elements
  index :: sh -> sh -> Int  -- Index into row-major
                            --  representation
  
instance             Shape Z         
instance Shape sh => Shape (sh:.Int) 

data All    = All
data Any sh = Any

backpermute :: (Shape sh, Shape sh', Elt e)
            => sh' -> (sh' -> sh)
            -> Array sh e -> Array sh' e
backpermute = error "urk"

class Slice sl where
  type FullShape  sl
  type SliceShape sl
  replicate :: Elt e
            => sl
            -> Array (SliceShape sl) e
            -> Array (FullShape  sl) e
  slice     :: Elt e
            => Array (FullShape  sl) e
            -> sl
            -> Array (SliceShape sl) e  
  
instance Slice Z where
  type FullShape  Z = Z
  type SliceShape Z = Z

instance Slice (Any sh) where
  type FullShape  (Any sh) = sh
  type SliceShape (Any sh) = sh
  replicate Any a = a
  slice a Any = a

instance (Shape (FullShape sl), 
          Shape (SliceShape sl),
          Slice sl) => Slice (sl:.Int) where
  type FullShape  (sl:.Int) = FullShape  sl :. Int
  type SliceShape (sl:.Int) = SliceShape sl

  replicate (sl:.i) arr = backpermute (ex:.i) drop arr2 
     where
       ex = extent arr2
       drop (is:._) = is
       arr2 = replicate sl arr

  slice arr (sl:.i) = slice arr2  sl
     where
       (ex:._) = extent arr 
       add is = is:.i
       arr2 = backpermute ex add arr

instance Slice ss => Slice (ss:.All) where
  type FullShape  (ss:.All) = FullShape  ss :. Int
  type SliceShape (ss:.All) = SliceShape ss :. Int
