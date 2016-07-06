{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTSyntax #-}

import Prelude                          as P
import qualified Data.Map.Strict        as M

type Name = String
type Lengths = [Int]
data Col = forall a. Eq a => Col [a]

data Size
    = Lengths [Int]                                 -- ^ number of children per parent
    | Up Int                                        -- ^ ancestor

data Dim where
    Dim ::  { size :: Lengths                       -- ^ size: of this (child) dimension, in relation to the parent
            , dims :: M.Map Name Dim                -- ^ children: child dimensions
            , cols :: M.Map Name Col                -- ^ columns: all columns that share the shape of this dimension
            } -> Dim                                -- ^ resulting dimension (parent x size)

dim = Dim { size=[], dims=M.empty, cols=M.empty }
root = dim { size=[1] }

-- broadcast [1,2,3] "ABC" = "ABBCCC"
broadcast :: Lengths -> [a] -> [a]
broadcast ls xs = concat $ zipWith replicate ls xs

join :: Lengths -> Lengths -> [a] -> [a]
join parent_shape top_shape top_values = broadcast parent_shape $ broadcast top_shape top_values

-- merge [3,4] [3,2,1,4,2,1,3] = [6, 10]
merge :: Lengths -> Lengths -> Lengths
merge (p:ps) cs =
    let (gs, cs') = P.splitAt p cs
    in P.sum gs:merge ps cs'

-------------------------------------------
main :: IO ()
main = do
    putStrLn "done"
