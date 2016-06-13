{-# LANGUAGE RankNTypes #-}

module Random.Vector
  where

import Control.Monad.ST
import System.Random.MWC

import Data.Vector.Unboxed	        ( Vector, Unbox )
import qualified Data.Vector.Unboxed	as V


{-# INLINE randomVectorOf #-}
randomVectorOf
    :: Unbox e
    => (forall s. Int -> GenST s -> ST s e)
    -> Int
    -> Vector e
randomVectorOf f n
  = runST $ do
      gen <- create
      vec <- V.generateM n (\i -> f i gen)
      return vec

