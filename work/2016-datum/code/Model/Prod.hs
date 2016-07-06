
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE UndecidableInstances              #-}

module Datum.Model.PRod
        ( module Text.Show.Pretty
        , module Data.Repa.Scalar.Product
        , module Data.Repa.Scalar.Singleton.Nat
        , Init (..)
        , Last (..)
        , Head (..)
        , Tail (..))
where
import Text.Show.Pretty
import Data.Repa.Scalar.Product
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.List                      as L


-----------------------------------------------------------
class Init t where
 type Init' t
 initt :: t -> Init' t

instance Init (t1 :*: ()) where
 type Init' (t1 :*: ())         = ()
 initt      (t1 :*: ())         = ()

instance Init (t2 :*: ts) =>  Init (t1 :*: t2 :*: ts) where
 type Init' (t1 :*: t2 :*: ts)  = t1 :*: Init' (t2 :*: ts)
 initt      (t1 :*: t2 :*: ts)  = t1 :*: initt (t2 :*: ts)


-----------------------------------------------------------
class Last t where
 type Last' t
 lastt :: t -> Last' t

instance Last (t1 :*: ()) where
 type Last' (t1 :*: ())         = t1
 lastt      (t1 :*: ())         = t1

instance Last (t2 :*: ts) => Last (t1 :*: t2 :*: ts) where
 type Last' (t1 :*: t2 :*: ts)  = Last' (t2 :*: ts)
 lastt      (t1 :*: t2 :*: ts)  = lastt (t2 :*: ts)


-----------------------------------------------------------
class Head t where
 type Head' t
 headt :: t -> Head' t

instance Head (t1 :*: ts) where
 type Head' (t1 :*: ts)         = t1
 headt      (t1 :*: ts)         = t1


-----------------------------------------------------------
class Tail t where
 type Tail' t
 tailt  :: t -> Tail' t

instance Tail (t1 :*: ts) where
 type Tail' (t1 :*: ts)         = ts
 tailt      (t1 :*: ts)         = ts
