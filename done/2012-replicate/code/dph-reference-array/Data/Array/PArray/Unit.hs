{-# LANGUAGE TypeFamilies #-}

-- | PR instance for units.
module Data.Array.PArray.Unit
        ( PData  (..)
        , PDatas (..))
where
import Data.Array.PArray.Segds
import Data.Array.PArray.Base
import qualified Data.Array.Unboxed     as U
import qualified Data.Array.Vector      as V


-- For arrays of units we can just keep track of the length,
-- because there is only one possible data constructor.
data instance PData ()
        = PUnit Int

data instance PDatas ()
        = PUnits (U.Vector Int)


instance PR () where 
 emptyPR
  = PUnit 0

 lengthPR (PUnit n)
  = n

 replicatePR n _
  = PUnit n

 replicatesPR ns _
  = PUnit $ U.sum ns

 appendPR (PUnit xs) (PUnit ys)
  = PUnit (xs + ys)

 indexPR _ _
  = ()

 indexvsPR _ _ srcixs
  = PUnit $ U.length srcixs

 extractPR _ _ len
  = PUnit len

 extractvsPR _ vsegd
  = PUnit  $ U.sum $ lengthsOfVSegd vsegd

 packPR (PUnit _) flags
  = PUnit  $ U.length flags

 combinePR _ (PUnit n) (PUnit m)
  = PUnit  $ n + m

 fromListPR xs
  = PUnit  $ length xs

 emptydPR 
  = PUnits $ U.empty
  
 singletondPR (PUnit n)
  = PUnits $ U.singleton n

 appenddPR (PUnits ns1) (PUnits ns2)
  = PUnits $ ns1 U.++ ns2

 lengthdPR (PUnits ns)
  = U.length ns

 indexdPR  (PUnits ns) i
  = PUnit $ ns U.! i

 concatdPR vs
  = PUnits $ U.concat $ V.toList 
           $ V.map (\(PUnits ns) -> ns) vs

 packdPR (PUnits vecs) flags
  = PUnits $ U.pack vecs flags
