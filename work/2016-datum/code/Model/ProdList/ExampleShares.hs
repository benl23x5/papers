{-# LANGUAGE 
        StandaloneDeriving, 
        MultiParamTypeClasses, FlexibleContexts,
        TypeOperators, TypeFamilies, DataKinds #-}

import Datum.Model.ProdList.Model
import qualified Data.List      as L

type Symbol     = String
type Price      = Double
type Volume     = Int
data Time       = Time Int Int Int

deriving instance Show Time
deriving instance Eq   Time
deriving instance Ord  Time


-- | Flat table of transactions. 
--
--   The transcation data appears in no particular order.
--   It it not sorted by symbol or time.
--   
transactions :: [Symbol :*: Price :*: Volume :*: Time :*: () ]
transactions
 =      [ "BHP" :*: 32.16 :*:   1000 :*: Time 10 01 00 :*: ()
        , "BHP" :*: 32.16 :*:    415 :*: Time 10 01 00 :*: ()
        , "BHP" :*: 32.16 :*:  35344 :*: Time 10 01 00 :*: ()
        , "TLS" :*:  5.11 :*:     13 :*: Time 10 01 05 :*: ()
        , "TLS" :*:  5.12 :*:    100 :*: Time 10 01 05 :*: ()
        , "TLS" :*:  5.13 :*:    500 :*: Time 10 01 12 :*: ()
        , "TLS" :*:  5.14 :*:   1000 :*: Time 10 01 05 :*: ()
        , "TLS" :*:  5.15 :*:   4000 :*: Time 10 01 05 :*: ()
        , "ANZ" :*: 29.11 :*:  11232 :*: Time 10 01 07 :*: ()
        , "QAN" :*:  1.91 :*:   1000 :*: Time 10 01 08 :*: ()
        , "TLS" :*:  5.16 :*: 789141 :*: Time 10 01 05 :*: ()
        , "NAB" :*: 35.23 :*:    123 :*: Time 10 01 16 :*: ()
        , "TLS" :*:  5.17 :*:   1200 :*: Time 10 01 16 :*: ()
        , "TLS" :*:  5.18 :*:  14231 :*: Time 10 01 16 :*: () ]


-- Example: Select just the Price and Volume columns of the transactions table.
ex1 :: [Price :*: Volume :*: ()]
ex1     = map (\r ->  select nat1  r :*: select nat2 r :*: ()) 
        $ transactions


-- Example: Get the list of unqiue symbols from the transactions table.
ex2 :: [Symbol]
ex2     = members nat0 transactions


-- Example: Group the transactions by symbol.
ex3 :: [Symbol :*: [Price :*: Volume :*: Time :*: ()]]
ex3     = group nat0 transactions


-- Example: Group the transactions by time.
ex4 :: [Time :*: [Symbol :*: Price :*: Volume :*: ()]]
ex4     = group nat3 transactions


-- Example: Group the transactions by both symbol and time.
--
-- We need to use the relative offset 'nat2' when grouping the inner
-- table because it no longer has the same number of columns as the
-- original transactions table.
--
ex5 :: [Symbol :*: [Time :*: [Price :*: Volume :*: ()]]]
ex5     = mapv (group nat2)
        $ group nat0 transactions


-- Example: Sum up the number of transactions by symbol and time.
ex6 :: [Symbol :*: [Time :*: Int :*: ()]]
ex6     = mapv (mapv (\r -> length r :*: ()))
        $ mapv (group nat2)
        $ group nat0 transactions


-- Example: Sum up the number of transactions by symbol and time.
ex7 :: [Symbol :*: [Time :*: [Double :*: Price :*: Volume :*: ()]]]
ex7     = mapv (mapv (map (\row  
                -> (select nat0 row * (fromIntegral $ select nat1 row)) 
                :*: row)))
        $ mapv (group nat2)
        $ group nat0 transactions


-- Example: Get the last transaction for each symbol.
-- ex8 :: [Symbol :*: Time :*: Price :*: Volume :*: ()]
-- ex8     = mapv (last . L.sortOn (select nat0) . flatten)
--        $ mapv (group nat2)
--        $ group nat0 transactions


-- Example: Compute the volume weighted average price per time period.
ex9 :: [Symbol :*: [Time :*: Double :*: ()]]
ex9     = mapv (mapv (\rows
                -> ( sum (map (\row -> select nat0 row * (fromIntegral $ select nat1 row)) rows)
                   / sum (map (\row -> fromIntegral $ select nat1 row) rows))
                :*: ()))
        $ mapv (group nat2)
        $ group nat0 transactions

