{-# LANGUAGE TypeOperators #-}

-- | Closure converted array combinators.
module Data.Array.Lifted
        ( indexPP
        , addPP
        , maximumPP
        , mapPP
        , zipWithPP)
where
import Data.Array.Closure
import Data.Array.PArray.Closure
import Data.Array.PArray.Base  
import qualified Data.Array.PArray      as PA
import qualified Prelude as P
import Prelude (($), Int)


-- | Indexing.
indexPP     :: PR a => PArray a :-> Int :-> a
indexPP     = closure2 PA.index PA.index_l 


-- | Addition.
addPP       :: Int :-> Int :-> Int
addPP       = closure2 (P.+) PA.add_l


-- | Maximum.
maximumPP   :: PArray Int :-> Int
maximumPP   = closure1 PA.maximum   PA.maximum_l


-- | Apply a worker function to all elements of an array.
mapPP   :: (PR a, PR b) 
        => (a :-> b) 
        :-> PArray a :-> PArray b
mapPP   = closure2 mapPP_v mapPP_l
 where 
       mapPP_v :: (PR a, PR b) 
               => (a :-> b) -> PArray a -> PArray b
       mapPP_v f as
        =   PA.replicate (PA.length as) f $:^ as

       mapPP_l :: (PR a, PR b) 
               => Int -> PArray (a :-> b) -> PArray (PArray a) -> PArray (PArray b)
       mapPP_l _ fs ass
        =   PA.unconcat ass 
        $   PA.replicates (PA.lengths ass) fs
        $:^ PA.concat ass


-- | Apply a worker function to corresponding pairs in two arrays.
zipWithPP  
        :: (PR a, PR b, PR c) 
        => (a :-> b :-> c) 
        :-> PArray a  :-> PArray b :-> PArray c
zipWithPP   = closure3 zipWithPP_v zipWithPP_l
 where 
       zipWithPP_v f xs ys
        =   PA.replicate (PA.length xs) f $:^ xs $:^ ys

       zipWithPP_l _ fs ass bss
        =   PA.unconcat ass 
        $   PA.replicates (PA.lengths ass) fs
        $:^ PA.concat ass
        $:^ PA.concat bss

