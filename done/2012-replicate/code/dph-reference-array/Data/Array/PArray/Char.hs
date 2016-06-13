
-- | PR instance for Characters.
module Data.Array.PArray.Char
        ( PData (..)
        , PDatas(..))
where
import Data.Array.PArray.Base
import Data.Array.PArray.Segds
import qualified Data.Array.Unboxed     as U
import qualified Data.Array.Vector      as V


instance PR Char where

 -- O(1).
 emptyPR
  = PChar U.empty

 -- O(1).
 lengthPR (PChar xs)
  = U.length xs

 -- O(length result) = O(n).
 replicatePR n x
  = PChar result
  where result   = U.replicate n x

 -- O(length result) = O(sum ns).
 replicatesPR ns (PChar xs)
  = PChar result
  where result  = U.concat $ zipWith U.replicate (U.toList ns) (U.toList xs)

 -- O(length result) = O(length xs + length ys).
 appendPR  (PChar xs) (PChar ys)
  = PChar result
  where result  = xs U.++ ys

 -- O(1).
 indexPR   (PChar xs) i
  = xs U.! i

 -- O(length result) = O(length segixs)
 indexvsPR (PChars xss) 
         (VSegd segmap (SSegd sources starts _))
         segixs
  = PChar result
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
 extractPR (PChar arr) start len 
  = PChar $ U.slice start len arr

 -- O(length result).
 extractvsPR (PChars arrs) vsegd
  = PChar result
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

 packPR (PChar xs) flags
  = PChar $ U.pack xs flags

 combinePR flags (PChar xs) (PChar ys)
  = PChar $ U.combine2 flags xs ys

 fromListPR xs
  = PChar $ U.fromList xs

 -- O(1).
 emptydPR 
  = PChars V.empty

 -- O(1).
 singletondPR (PChar xs)
  = PChars $ V.singleton xs

 -- O(length result) = O(length pdatas1 + length pdatas2)
 appenddPR (PChars pdatas1) (PChars pdatas2)
  = PChars $ pdatas1 V.++ pdatas2

 -- O(1).
 lengthdPR (PChars pdatas)
  = V.length pdatas

 -- O(1).
 indexdPR (PChars pdatas) ix
  = PChar $ pdatas V.! ix

 -- O(max (length result, length pdatass))
 concatdPR pdatass 
  = PChars result
  where result  = V.concat $ V.toList
                $ V.map (\(PChars pdatas) -> pdatas) pdatass

 packdPR (PChars pdatas) flags 
  = PChars (V.pack pdatas flags)
