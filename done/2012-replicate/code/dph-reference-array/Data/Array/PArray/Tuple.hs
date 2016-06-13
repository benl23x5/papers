{-# LANGUAGE TypeFamilies #-}

-- | PR instance for Tuples.
module Data.Array.PArray.Tuple
        ( PData (..)
        , PDatas(..))
where
import Data.Array.PArray.Base
import qualified Data.Vector            as V


data instance PData (a, b)
        = PTuple2  (PData a)  (PData b)

data instance PDatas (a, b)
        = PTuple2s (PDatas a) (PDatas b)


instance (PR a, PR b) => PR (a, b) where
  emptyPR
   = PTuple2 emptyPR emptyPR
 
  lengthPR (PTuple2 xs _)
   = lengthPR xs

  replicatePR n (x, y)
   = PTuple2 (replicatePR n x)
             (replicatePR n y)

  replicatesPR ns (PTuple2 xs ys)
   = PTuple2 (replicatesPR ns xs)
             (replicatesPR ns ys)

  appendPR   (PTuple2 xs1 ys1) (PTuple2 xs2 ys2)
   = PTuple2 (appendPR xs1 xs2)
             (appendPR ys1 ys2)

  indexPR    (PTuple2 xs ys) i
   = ( indexPR xs i
     , indexPR ys i)

  indexvsPR  (PTuple2s xss yss) vsegd segixs
   = PTuple2 (indexvsPR xss vsegd segixs)
             (indexvsPR yss vsegd segixs)

  extractPR  (PTuple2 xs ys) start len
   = PTuple2 (extractPR xs start len)
             (extractPR ys start len)

  extractvsPR (PTuple2s xss yss) vsegd
   = PTuple2  (extractvsPR xss vsegd)
              (extractvsPR yss vsegd)

  packPR      (PTuple2 xs ys) flags
   = PTuple2  (packPR xs flags)
              (packPR ys flags)

  combinePR   flags (PTuple2 xs1 ys1) (PTuple2 xs2 ys2)
   = PTuple2  (combinePR flags xs1 xs2)
              (combinePR flags ys1 ys2)

  fromListPR  xss
   = PTuple2  (fromListPR $ map fst xss)
              (fromListPR $ map snd xss)

  emptydPR
   = PTuple2s emptydPR
              emptydPR

  singletondPR (PTuple2 xs ys)
   = PTuple2s (singletondPR xs)
              (singletondPR ys)

  appenddPR   (PTuple2s xss1 yss1) (PTuple2s xss2 yss2)
   = PTuple2s (appenddPR xss1 xss2)
              (appenddPR yss1 yss2)

  lengthdPR   (PTuple2s xss _)
   = lengthdPR xss

  indexdPR    (PTuple2s xss yss) i
   = PTuple2  (indexdPR xss i)
              (indexdPR yss i)

  concatdPR vs
   = let (xsss, ysss)
                = V.unzip
                $ V.map (\(PTuple2s xss yss) -> (xss, yss)) vs

     in  PTuple2s (concatdPR xsss)
                  (concatdPR ysss)

  packdPR (PTuple2s xss yss) flags
   = PTuple2s (packdPR xss flags)
              (packdPR yss flags)

