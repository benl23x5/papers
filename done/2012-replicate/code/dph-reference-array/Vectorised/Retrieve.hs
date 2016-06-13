{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Vectorisation of the 'retrieve' example function from 
--   Section 2: The Asymptotic Complexity Problem.
module Vectorised.Retrieve where
import Data.Array.PArray.Closure
import Data.Array.PArray
import Data.Array.Closure
import Data.Array.Lifted
import Data.Array.PArray.Pretty         ()
import qualified Prelude                as P
import qualified Data.Vector.Unboxed    as U
import Prelude (Int, ($))

{- Source code.

   retrieve :: [:[:Char:]:] -> [:[:Int:]:] -> [:[:Char:]:]
   retrieve xss iss
    = zipWithP mapP (mapP indexP xss) iss
-}


-- Result of applying the vectorisation transform.
retrieve_v0 
        :: PR a 
        => PArray (PArray a) -> PArray (PArray Int) -> PArray (PArray a)

retrieve_v0 xss iss
 = zipWithPP $: mapPP $: (mapPP $: indexPP $: xss) $: iss


-- Shift inner computation into a separate binding for clarity.
retrieve_v1 xss iss
 = let  fs = mapPP $: indexPP $: xss
   in   zipWithPP $: mapPP $: fs $: iss


-- Inline mapPP and closure application.
retrieve_v2 xss iss
 = let  c  = length xss
        fs = replicate c indexPP $:^ xss
   in   replicate c mapPP $:^ fs $:^ iss


-- Inline indexPP and mapPP
retrieve_v3 xss iss
 = unconcat iss
 $ (let ns      = lengths iss
        n       = U.sum ns
    in  PArray n (AClo index indexlPR
                    (replicatesPR ns (parrayData xss))))
 $:^ concat iss


-- Inline lifted application.
retrieve_v4 (PArray _ xss') iss@(PArray _ iss')
 = unconcat iss
 $ let ns      = lengths iss
       n       = U.sum ns
   in  PArray n (indexlPR n 
                  (replicatesPR ns xss') 
                  (concatPR iss'))

-- Final version.
retrieve_v 
        :: PR a  
        => PArray (PArray a) -> PArray (PArray Int) -> PArray (PArray a)

retrieve_v xss iss
 = let ns = lengths iss
   in  unconcat iss 
        $ index_l (U.sum ns) (replicates ns xss) 
        $ concat iss
