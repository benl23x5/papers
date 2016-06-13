
module Test where
import Data.Vector.Unboxed                      (Vector, Unbox)
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as MV
import GHC.Base (quotInt, remInt)
import Prelude                                  hiding (map, zipWith)


data Array sh e
        = Manifest sh !(Vector e)
        | Delayed  !sh  (sh -> e)


data DIM1 
        = DIM1 !Int

data DIM2 
        = DIM2 !Int !Int


foo :: DIM2 -> DIM2 -> Int
foo sz ix
 = case Delayed sz (\_ -> 0) of
        Delayed _ f -> f ix


