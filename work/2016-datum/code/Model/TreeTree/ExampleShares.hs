
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PatternSynonyms            #-}

module Datum.Model.Tree
        ( module Text.Show.Pretty
        , module Data.Repa.Scalar.Product
        , module Data.Repa.Scalar.Singleton.Nat

        , pp)
where
import Text.Show.Pretty
import Data.Repa.Scalar.Product
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.List                      as L
import GHC.TypeLits

pp x = putStrLn $ ppShow x

type family   L a
type instance L ()              = ()
type instance L (v1 :*: vs)     = [v1] :*: L vs


data T k vs where
        T :: k -> L vs -> T k vs

deriving instance (Show k, Show (L vs)) => Show (T k vs)


type    C k = T k ()
pattern C k = T k ()

data Root = Root
deriving instance Show Root


type Ticker  = String
type Time    = String
type Price   = Double
type Volume  = Int
type Address = String

trans2  :: T Root 
             ( (T (Ticker :*: ())
                  (   C (Price :*: Volume :*: Time :*: ()) 
                  :*: C (Address :*: ())
                  :*: ()))
             :*: ())
trans2
 = T    Root
        ( [ T  ("BHP" :*: ())
               (   [ C (32.16 :*:  1000 :*: "10:01:00" :*: ())
                   , C (55.16 :*:   415 :*: "10:01:00" :*: ())
                   , C (32.16 :*: 35344 :*: "10:01:00" :*: ()) ]

               :*: [ C ("address1" :*: ())
                   , C ("address2" :*: ()) ]

               :*: ())

          , T ("TLS" :*: ())
               (   [ C (5.11  :*:    13 :*: "10:01:05" :*: ())
                   , C (5.12  :*:   100 :*: "10:01:05" :*: ()) ]

               :*: [ C ("telstra address1" :*: ())
                   , C ("telstra address2" :*: ()) ]

               :*: ())
          ]
        :*: ())



