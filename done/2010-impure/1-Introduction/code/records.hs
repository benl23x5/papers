

data R1 = R1 { r1Field1  :: Int
             , r1Field2  :: R2 }
		deriving Show

data R2 = R2 { r2Field1  :: Char
             , r2Field2  :: R3 }
		deriving Show

data R3 = R3 { r3Field1  :: Bool
             , r3Count   :: Int }
		deriving Show

record1 = R1 { r1Field1 = 5
            , r1Field2 = 
                 R2 { r2Field1 = 'a'
                    , r2Field2 =
                         R3 { r3Field1 = False
                            , r3Count  = 0 }}}


count  = (r3Count . r2Field2 . r1Field2) record1

record2
  = record1 { r1Field2 = 
      (r1Field2 record1) { r2Field2 = 
         ((r2Field2 . r1Field2) record1) { r3Count = 1 }}}


record3
  = record2 { 
      r1Field2 = (r1Field2 record2) {
      r2Field2 = ((r2Field2 . r1Field2) record2) {
      r3Count  = (r3Count . r2Field2 . r1Field2) record2 + 1 
    }}}