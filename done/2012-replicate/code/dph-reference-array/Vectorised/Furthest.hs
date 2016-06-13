{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Vectorisation of the 'furthest' example function from 
--   Section 5.5: Flattening and Space Usage.
module Vectorised.Furthest where
import Data.Array.PArray.Closure
import Data.Array.PArray
import Data.Array.Closure
import Data.Array.Lifted
import Data.Array.PArray.Pretty         ()
import qualified Prelude                as P
import qualified Data.Vector.Unboxed    as U
import Prelude (Int, ($))

{- Source code.

   furthest :: [:(Float, Float):] -> Float
   furthest ps
        = maximumP (mapP (\x -> maximumP (mapP (distanceP x) ps)) ps)
-}

-- We haven't got a PR Float instance to implement the above directly, 
-- but the actual type used by the points isn't relevant to the
-- vectoriser. We'll do the vectorisation assuming the point is
-- represented by a plain 'Int'.
distancePP :: Int :-> Int :-> Int
distancePP = addPP

distance   = (P.+)
distance_l = add_l


-- Result of applying the vectorisation transform.
furthest_v0 :: PArray Int -> Int
furthest_v0 xs
 = let  -- Unlifted version isn't used by the inner function.
        fv :: Int -> Int
        fv       = P.undefined 
        
        fl :: Int -> PArray Int -> PArray Int
        fl c ys  =    replicate c maximumPP 
                 $:^ (replicate c mapPP 
                        $:^ (replicate c distancePP $:^ ys) 
                        $:^  replicate c xs)

        fPP      :: Int :-> Int
        fPP      = closure1 fv fl

  in    maximumPP $: (mapPP $: fPP $: xs)


-- Inline maximumPP, fPP and last mapPP.
furthest_v1 xs
 = let  fl :: Int -> PArray Int -> PArray Int
        fl c ys  =   maximum_l c 
                 $   replicate c mapPP 
                        $:^ (replicate c distancePP $:^ ys)
                        $:^ replicate c xs

  in    maximum (fl (length xs) xs)


-- Inline inner mapPP.
furthest_v2 xs
 = let  fl :: Int -> PArray Int -> PArray Int
        fl c ys  
         = let xss'     = replicate c xs
           in  maximum_l c 
                $   unconcat xss'
                $   replicates (lengths xss') ((replicate c distancePP) $:^ ys)
                $:^ concat xss'

  in    maximum (fl (length xs) xs)


-- Float bindings.
furthest_v3 xs
 =      maximum (let c    = length xs
                     xss' = replicate c xs
                 in  maximum_l c 
                        $   unconcat xss'
                        $   replicates (lengths xss') 
                                       ((replicate c distancePP) $:^ xs)
                        $:^ concat xss')


-- Inline distancePP closure.
furthest_v4 xs
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'

        fv_1 _ xa    = Clo  distance fl' xa
        fl_1 _ _ xs' = AClo distance fl' xs'
        
        clo         = Clo fv_1 fl_1 ()
        
   in   maximum $   maximum_l c 
                $   unconcat xss'
                $   replicates ns ((replicate c clo) $:^ xs)
                $:^ concat xss'


-- Inline closure replication.
furthest_v5 xs
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'

        fv_1 _ xa    = Clo  distance fl' xa
        fl_1 _ _ xs' = AClo distance fl' xs'
        
   in   maximum $   maximum_l c 
                $   unconcat xss'
                $   replicates ns 
                      (PArray c (AClo fv_1 fl_1 (replicatePR c ())) $:^ xs)
                $:^ concat xss'


-- Inline inner lifted application.
furthest_v6 xs@(PArray _ xs')
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'

        fl_1 _ _ xs2 = AClo distance fl' xs2
             
   in   maximum $   maximum_l c 
                $   unconcat xss'
                $   replicates ns (PArray c (fl_1 c (replicatePR c ()) xs'))
                $:^ concat xss'

-- Inline fl_1.
furthest_v7 xs@(PArray _ xs')
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
        
   in   maximum $   maximum_l c 
                $   unconcat xss'
                $   replicates ns (PArray c (AClo distance fl' xs'))
                $:^ concat xss'


-- Inline replicates of closures.
furthest_v8 xs@(PArray _ xs')
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
                
   in   maximum $   maximum_l c 
                $   unconcat xss'
                $   PArray (U.sum ns) (AClo distance fl' (replicatesPR ns xs'))
                $:^ concat xss'


-- Inline final lifted application.
furthest_v9 xs@(PArray _ xs')
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'

        fl' n pdata1 pdata2
         = case distance_l n (PArray n pdata1) (PArray n pdata2) of
                PArray _ pdata' -> pdata'
                
   in   maximum $   maximum_l c 
                $   unconcat xss'              
                $   (case concat xss' of
                     PArray _ xssd
                      -> PArray (U.sum ns) 
                       $ fl' (U.sum ns) (replicatesPR ns xs') xssd)

-- Final version.
furthest_v :: PArray Int -> Int
furthest_v xs
 = let  c       = length xs
        xss'    = replicate c xs
        ns      = lengths xss'
                
   in   maximum $ maximum_l c 
                $ unconcat xss'              
                $ distance_l (U.sum ns)
                        (replicates ns xs) 
                        (concat xss')


{- Here is an example where 'xs' contains four points, 
   that shows the vectorised version uses quadratic space.

   replicates ns xs  = p1 p1 p1 p1  p2 p2 p2 p2  p3 p3 p3 p3  p4 p4 p4 p4
   replicate  c  xs  = p1 p2 p3 p4  p1 p2 p3 p4  p1 p2 p3 p4  p1 p2 p3 p4
   after distance_l  = d1 d2 d3 d4  d5 d6 d7 d8  d9 dA dB dC  dD dE dF dG
   after maximum_l        m01           m02         m03          m04 
-}          
