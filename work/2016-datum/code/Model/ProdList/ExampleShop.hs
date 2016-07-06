{-# LANGUAGE 
        StandaloneDeriving, 
        MultiParamTypeClasses, FlexibleContexts,
        TypeOperators, TypeFamilies, DataKinds #-}

import Datum.Model.ProdList.Model
import qualified Data.List      as L

type Region     = String
type Customer   = String
data Date       = Date Int Int Int
data Desc       = String
data Price      = Double

deriving instance Show Date
deriving instance Eq   Date
deriving instance Ord  Date


-- | Tree of invoice data.
invoice
 =      [ "Sydney" 
        :*: [ "Julie"
            :*: [ (100 :*: Date 2000 1 1)
                :*: [ 1 :*: "Mug"               :*: 10.00
                    , 2 :*: "Shirt"             :*: 15.00
                    , 3 :*: "Bag"               :*: 25.00 ]

                , (101 :*: Date 2000 4 5)
                :*: [ 1 :*: "Chop Sticks"       :*:  2.00
                    , 2 :*: "Guitar"            :*: 50.00
                    , 3 :*: "Hat"               :*:  9.00 ]

                , (102 :*: Date 2000 4 10)
                :*: [ 1 :*: "Socks"             :*: 7.00  ]

                , (103 :*: Date 2001 1 9)
                :*: [ 1 :*: "Bag"               :*: 9.00  ]
                ]

            , "Tom"
            :*: [ (104 :*: Date 2000 5 1)
                :*: [ 1 :*: "Book"              :*: 10.00 
                    , 2 :*: "Water"             :*:  2.00 ]

                , (105 :*: Date 2002 7 1)
                :*: [ 1 :*: "Jacket"            :*: 30.00 ]
                ]

            , "Steve"
            :*: [ (106 :*: Date 2003 7 4)
                :*: [ 1 :*: "Eggs"              :*:  3.00 ]
                ]

            , "Adrian"
            :*: [ (107 :*: Date 2003 7 1)
                :*: [ 1 :*: "Bread"             :*:  2.00 ]

                , (108 :*: Date 2003 9 1)
                :*: [ 1 :*: "Corn"              :*:  4.00 ]
                ]
            ]
        ]
