
-- | This module exports all the boxed vector primitives we use.
--   In the reference implementation we store data chunks as boxed vectors
--   of unboxed vectors. 
--   In the real DPH implementation we instead use a 2D vectors type, 
--   which ensures that the inner vectors are also unboxed.
module Data.Array.Vector
        ( Vector
        , empty
        , singleton
        , length
        , concat
        , (++)
        , (!)
        , pack
        , map
        , toList
        , fromList
        , convert
        , unzip3
        , prescanl)
where
import Data.Vector
import qualified Data.Vector.Unboxed    as U
import qualified Prelude                as P
import Prelude (Int, Bool, ($), fst, snd)


-- | Extract all elements from an array according to a given flag array
pack :: Vector a -> U.Vector Bool -> Vector a
pack xs fs
        = map fst $ filter snd $ zip xs (U.convert fs)

