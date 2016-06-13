{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Vectorisation of the 'retsum' example function from 
--   Section 5.4: Reduction and Dynamic Hoisting.
module Vectorised.Retsum where
import Data.Array.PArray.Closure
import Data.Array.PArray
import Data.Array.Closure
import Data.Array.Lifted
import Data.Array.PArray.Pretty         ()
import qualified Prelude                as P
import qualified Data.Vector.Unboxed    as U
import Prelude (Int, ($))


-- The following is a derivation of the vectorised code presented for the 
-- 'retsum' example. At every step we can apply the example arrays and get
-- the same result:
--
--  > logical $ retsum_v arr1 arr2
--  [[5,4,5], [20,21], [16]]
--
--  > logical $ retsum_v12 arr1 arr2
--  [[5,4,5], [20,21], [16]]
--
retsum_v0 :: PArray (PArray Int) -> PArray (PArray Int) -> PArray (PArray Int)

--  Create closure converted version of 'f'.
retsum_v0 xss iss
 = let  fv ys j     = index ys j P.+ sum ys
        fl c yss js = add_l c (index_l c yss js) (sum_l c yss)
        fPP         = closure2 fv fl
        
   in   zipWithPP $: mapPP $: (mapPP $: fPP $: xss) $: iss


-- Shift partial application into own binding and inline zipWithPP.
retsum_v2 xss iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)
        fPP     = closure2 fv fl

        gs      = mapPP $: fPP $: xss

   in   replicate c mapPP $:^ gs $:^ iss


-- Inline closure2.
retsum_v3 xss iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js    = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
        fv_1 _ xa       = Clo  fv fl' xa
        fl_1  _ _ xs    = AClo fv fl' xs

        fPP     = Clo fv_1 fl_1 ()

        gs      = replicate c fPP $:^ xss

   in   replicate c mapPP $:^ gs $:^ iss


-- Inline replicate[:->] instance.
retsum_v4 xss iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
        fv_1 _ xa       = Clo  fv fl' xa
        fl_1  _ _ xs    = AClo fv fl' xs

        gs      = PArray c (AClo fv_1 fl_1 (replicatePR c ())) $:^ xss

   in   replicate c mapPP $:^ gs $:^ iss


-- Inline replicate[:->] instance.
retsum_v5 _xss@(PArray _ xss') iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
        fl_1  _ _ xs    = AClo fv fl' xs

        gs      = PArray c (fl_1 c (replicatePR c ()) xss')

   in   replicate c mapPP $:^ gs $:^ iss


-- Inline fl_1 binding.
retsum_v6 _xss@(PArray _ xss') iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
        gs      = PArray c (AClo fv fl' xss')

   in   replicate c mapPP $:^ gs $:^ iss


-- Inline mapPP and the lifted applications.
retsum_v7 _xss@(PArray _ xss') iss
 = let  c       = length iss

        fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
        gs      = PArray c (AClo fv fl' xss')

   in   unconcat iss
         $   replicates (lengths iss) gs
         $:^ concat iss


-- Inline replicate[:->] instance.
retsum_v8 _xss@(PArray _ xss') iss
 = let  fv ys j = index ys j P.+ sum ys
        fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
   in   unconcat iss
         $   (let ns = lengths iss
                  n  = U.sum ns
              in PArray n (AClo fv fl' (replicatesPR ns xss')))
         $:^ concat iss


-- Inline lifted application.
retsum_v9 _xss@(PArray _ xss') iss@(PArray _ iss')
 = let  fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)

        fl' n pdata1 pdata2
         = case fl n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
   in   unconcat iss
         $  (let ns = lengths iss
                 n  = U.sum ns
             in  PArray n (fl' n (replicatesPR ns xss') (concatPR iss')))


-- Inline lifted application.
retsum_v10 xss iss
 = let  fl c' yss js = add_l c' (index_l c' yss js) (sum_l c' yss)
        
   in   unconcat iss
         $ (let ns = lengths iss
                n  = U.sum ns
            in  fl n (replicates ns xss) (concat iss))


-- Inline fl
retsum_v11 xss iss
 = unconcat iss
 $ (let ns   = lengths iss
        n    = U.sum ns
        yss' = replicates ns xss
    in  add_l n (index_l n yss' (concat iss)) (sum_l n yss'))


-- Float bindings.
retsum_v xss iss
 = let  ns      = lengths iss
        n       = U.sum ns
        yss'    = replicates ns xss
   in   unconcat iss 
         $ add_l n (index_l n yss' (concat iss)) 
                   (sum_l n yss')
