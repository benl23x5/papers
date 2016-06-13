
-- | PR intance for integers.
module Data.Array.PArray.Int
        ( PData (..)
        , PDatas(..))
where
import Data.Array.PArray.Base
import Data.Array.PArray.Segds
import qualified Data.Array.Unboxed     as U
import qualified Data.Array.Vector      as V


instance PR Int where

 -- O(1).
 emptyPR
  = PInt U.empty

 -- O(1).
 lengthPR (PInt xs)
  = U.length xs

 -- O(length result) = O(n).
 replicatePR n x
  = PInt result
  where result   = U.replicate n x

 -- O(length result) = O(sum ns).
 replicatesPR ns (PInt xs)
  = PInt result
  where result  = U.concat $ zipWith U.replicate (U.toList ns) (U.toList xs)

 -- O(length result) = O(length xs + length ys).
 appendPR  (PInt xs) (PInt ys)
  = PInt result
  where result  = xs U.++ ys

 -- O(1).
 indexPR   (PInt xs) i
  = xs U.! i

 -- O(length result) = O(length segixs)
 indexvsPR (PInts xss) 
         (VSegd segmap (SSegd sources starts _))
         segixs
  = PInt result
  where result  
         = U.map (\(segix, elemix) 
                  -> let psegid   = segmap  U.! segix
                         source   = sources U.! psegid
                         start    = starts  U.! psegid
                         xs       = xss     V.! source
                     in  xs U.! (start + elemix))
                 segixs

 -- O(1).
 -- Constant time extract depends on O(1) slice being provided by the
 -- Data.Vector library. If U.slice was O(length result) then so would
 -- extractPR. This wouldn't affect the complexity of vectorised code,
 -- because it only uses the lifted versions of operators.
 extractPR (PInt arr) start len 
  = PInt $ U.slice start len arr

 -- O(length result).
 extractvsPR (PInts arrs) vsegd
  = PInt result
  where
        -- Demote the vsegd to a plain ssegd.
        -- O(length vsegd)
        ssegd    = demoteVSegd  vsegd                   

        -- Projections 
        -- O(1).
        lengths  = segdLengths $ ssegdSegd ssegd
        sources  = ssegdSources ssegd
        starts   = ssegdStarts  ssegd

        -- Get the source id for each result element.
        -- O(length xsources) = O(length result)
        xsources = U.replicates  lengths sources

        -- Get the index for each result element in its flat source array.
        -- O(length xindices) = O(length result)
        xindices = U.zipWith (+) (U.ranges     lengths)
                                 (U.replicates lengths starts)

        -- O(length result)
        result   = U.zipWith  (\src ix -> (arrs V.! src) U.! ix)
                              xsources xindices

 packPR (PInt xs) flags
  = PInt $ U.pack xs flags

 combinePR flags (PInt xs) (PInt ys)
  = PInt $ U.combine2 flags xs ys

 fromListPR xs
  = PInt $ U.fromList xs

 -- O(1).
 emptydPR 
  = PInts V.empty

 -- O(1).
 singletondPR (PInt xs)
  = PInts $ V.singleton xs

 -- O(length result) = O(length pdatas1 + length pdatas2)
 appenddPR (PInts pdatas1) (PInts pdatas2)
  = PInts $ pdatas1 V.++ pdatas2

 -- O(1).
 lengthdPR (PInts pdatas)
  = V.length pdatas

 -- O(1).
 indexdPR (PInts pdatas) ix
  = PInt $ pdatas V.! ix

 -- O(max (length result, length pdatass))
 concatdPR pdatass 
  = PInts result
  where result  = V.concat $ V.toList
                $ V.map (\(PInts pdatas) -> pdatas) pdatass

 packdPR (PInts pdatas) flags 
  = PInts (V.pack pdatas flags)

