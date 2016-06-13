
module BlackScholes where

import Time
import Random
import Prelude                                  as P
import Data.Array.Accelerate                    as A
import qualified Data.Array.Accelerate.CUDA     as CUDA
import qualified Foreign.CUDA.Driver            as CUDA

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar

import Criterion.Main                           hiding ( run )
import System.IO
import System.Environment
import System.Random.MWC


-- Black-Scholes option pricing ------------------------------------------------
-- ----------------------------                                               --

riskfree, volatility :: Float
riskfree   = 0.02
volatility = 0.30

horner :: Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b


cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k


blackscholes :: Acc (Vector (Float, Float, Float)) -> Acc (Vector (Float, Float))
blackscholes =  A.map go
  where
  go x =
    let (price, strike, years) = A.unlift x
        r       = A.constant riskfree
        v       = A.constant volatility
        v_sqrtT = v * sqrt years
        d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
        d2      = d1 - v_sqrtT
        cnd d   = let c = cnd' d in d >* 0 ? (1.0 - c, c)
        cndD1   = cnd d1
        cndD2   = cnd d2
        x_expRT = strike * exp (-r * years)
    in
    A.lift ( price * cndD1 - x_expRT * cndD2
           , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1))


-- Benchmarking ----------------------------------------------------------------
-- ------------                                                               --

million :: Int
million = 1000000

-- oh hai, hax!
-- we should really add O(1) zip/unzip to the library
--
randoms :: GenIO -> Int -> IO (Vector (Float,Float,Float))
randoms gen n = do
  (Array sh (AD_Pair _ price))  <- randomArrayR gen (5,30)    (Z :. n) :: IO (Vector Float)
  (Array _  (AD_Pair _ strike)) <- randomArrayR gen (1,100)   (Z :. n) :: IO (Vector Float)
  (Array _  (AD_Pair _ years))  <- randomArrayR gen (0.25,10) (Z :. n) :: IO (Vector Float)
  return $ Array sh (AD_Unit `AD_Pair` price `AD_Pair` strike `AD_Pair` years)

main :: IO ()
main = withSystemRandom $ \gen -> do

  putStrLn "Running black-scholes test"
  putStr   "Generating data... " >> hFlush stdout

  psy2  <- randoms gen (2  * million)
  psy4  <- randoms gen (4  * million)
  psy6  <- randoms gen (6  * million)
  psy8  <- randoms gen (8  * million)
  psy10 <- randoms gen (10 * million)
  psy12 <- randoms gen (12 * million)
  psy14 <- randoms gen (14 * million)
  psy16 <- randoms gen (16 * million)
  psy18 <- randoms gen (18 * million)
  psy20 <- randoms gen (20 * million)

  _     <- psy2  `seq` psy4  `seq` psy6  `seq` psy8  `seq` psy10 `seq` return ()
  _     <- psy12 `seq` psy14 `seq` psy16 `seq` psy18 `seq` psy20 `seq` return ()
  putStrLn "done"

  -- Pre-initialise the device. This is more expensive on some devices than on
  -- others, so do it first to avoid polluting the other benchmarks.
  --
  (initTime, _) <- time $ CUDA.initialise []
  putStrLn $ "Initialising CUDA device: " ++ showTime initTime

  -- Check the execution time of the very first invocation. This includes all
  -- backend compilation, data transfer, cache setup, etc.
  --
  (compileTime, _) <- time $
    let r = CUDA.run $ blackscholes (use psy2)
    in  r `seq` return ()
  putStrLn $ "First-run compile & execute time: " ++ showTime compileTime

  -- Run the criterion benchmarks.
  -- Must select how the program should be executed:
  --
  --  * run: includes frontend processing (nameless conversion, sharing
  --         recovery, etc)
  --
  --  * run1: skips directly to the execution phase (on second and subsequent
  --          invocations)
  --
  putStrLn $ "Starting criterion benchmarks"
  let -- run  = CUDA.run . blackscholes . use
      run  = CUDA.run1  blackscholes

  args  <- takeWhile (/= "--") `fmap` getArgs
  withArgs args $ defaultMain
    [ bgroup "black-scholes"
    $ P.map (\(label,array) -> bench label (whnf run array))
        [ ("2M",  psy2)
        , ("4M",  psy4)
        , ("6M",  psy6)
        , ("8M",  psy8)
        , ("10M", psy10)
        , ("12M", psy12)
        , ("14M", psy14)
        , ("16M", psy16)
        , ("18M", psy18)
        , ("20M", psy20)
        ]
    ]

