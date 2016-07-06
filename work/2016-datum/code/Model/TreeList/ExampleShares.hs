

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE UndecidableInstances              #-}

module Datum.Model.TreeList.ExampleShares
        ( module Text.Show.Pretty
        , module Data.Repa.Scalar.Product
        , module Data.Repa.Scalar.Singleton.Nat

        , pp
        , Tree (..))
where
import Text.Show.Pretty
import Data.Repa.Scalar.Product
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.List                      as L

pp x = putStrLn $ ppShow x

-------------------------------------------------
-- Pure keyspace.
-- Values are just keys that don't index anything.

-- data Tree ks where
--        Branch  :: (Show (Head' ks), Show (Tail' ks))
--                => Head' ks -> [Tree (Tail' ks)] -> Tree ks

data Tree ks where
        Branch  :: (Show k, Show ks)
                => k -> [Tree ks] -> Tree (k :*: ks)

deriving instance Show (Tree ks)

data Root = Root
deriving instance Show Root

type Symbol = String
type Time   = String
type Price  = Double
type Volume = Int



-------------------------------------------------
trans :: Tree  (    Root
                :*: (Symbol :*: Price :*: Volume :*: Time :*: ()) 
                :*: ())
trans
 = Branch Root
        [ Branch ("BHP" :*: 32.16 :*:  1000 :*: "10:01:00" :*: ()) []
        , Branch ("BHP" :*: 55.16 :*:   415 :*: "10:01:00" :*: ()) []
        , Branch ("ANZ" :*: 29.11 :*: 11232 :*: "10:01:07" :*: ()) [] ]



-------------------------------------------------
trans2 :: Tree (    Root
                :*: (Symbol :*: ()) 
                :*: (Price  :*: Volume :*: Time :*: ()) 
                :*: ())
trans2
 = Branch Root
        [ Branch ("BHP" :*: ())
                [ Branch (32.16 :*: 1000  :*: "10:01:00" :*: ()) []
                , Branch (55.16 :*:  415  :*: "10:01:00" :*: ()) [] ]

        , Branch ("ANZ" :*: ())
                [ Branch (29.11 :*: 11232 :*: "10:01:07" :*: ()) [] ]
        ]


-------------------------------------------------
trans3 :: Tree (    Root   
                :*: (Symbol :*: Time   :*: ()) 
                :*: (Price  :*: Volume :*: ()) 
                :*: ())
trans3
 = Branch Root
        [ Branch ("BHP" :*: "10:01:00" :*: ())
                [ Branch (32.16 :*: 1000  :*: ()) []
                , Branch (55.16 :*:  415  :*: ()) [] ]

        , Branch ("ANZ" :*: "10:01:07" :*: ())
                [ Branch (29.11 :*: 11232 :*: ()) [] ]
        ]


-------------------------------------------------
trans4 :: Tree (    Root
                :*: (String :*: ()) 
                :*: (Time   :*: ())
                :*: (Double :*: Int  :*: ()) 
                :*: ())
trans4
 = Branch Root
        [ Branch ("BHP" :*: ())
                [ Branch ("10:01:00" :*: ())
                        [ Branch (32.16 :*: 1000  :*: ()) []
                        , Branch (55.16 :*:  415  :*: ()) [] ]]

        , Branch ("ANZ" :*: ())
                [ Branch ("10:01:07" :*: ())
                        [ Branch (29.11 :*: 11232 :*: ()) [] ]]
        ]


