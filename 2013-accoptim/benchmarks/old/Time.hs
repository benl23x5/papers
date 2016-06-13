
module Time where

import Numeric
import Data.List

import System.CPUTime

time :: IO a -> IO (Double, a)
time action = do
  t1     <- getCPUTime
  result <- action
  t2     <- getCPUTime
  let picoseconds = fromIntegral (t2 - t1) * 1E-12
  --
  return (picoseconds, result)

showTime :: Double -> String
showTime t = showFFloatSIBase (Just 3) 1000 t "s"

showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase p b n
  = showString
  . nubBy (\x y -> x == ' ' && y == ' ')
  $ showFFloat p n' [ ' ', si_unit ]
  where
    n'          = n / (b ^^ (pow-4))
    pow         = max 0 . min 8 . (+) 4 . floor $ logBase b n
    si_unit     = "pnµm kMGT" !! pow

