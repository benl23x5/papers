
module DotP where

import Time
import Random
import Prelude                                  as P
import Data.Array.Accelerate                    as A
import qualified Data.Array.Accelerate.CUDA     as CUDA
import qualified Foreign.CUDA.Driver            as CUDA

import Criterion.Main                           hiding ( run )
import System.IO
import System.Environment
import System.Random.MWC


-- Dot Product -----------------------------------------------------------------
-- -----------                                                                --

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys
  = A.fold (+) 0
  $ A.zipWith (*) xs ys


-- Benchmarking ----------------------------------------------------------------
-- ------------                                                               --

million :: Int
million = 1000000


main :: IO ()
main = withSystemRandom $ \gen -> do

  putStrLn "Running dot-product test"
  putStr   "Generating data... " >> hFlush stdout

  x2    <- randomArray gen (Z :.  2 * million)
  y2    <- randomArray gen (Z :.  2 * million)
  x4    <- randomArray gen (Z :.  4 * million)
  y4    <- randomArray gen (Z :.  4 * million)
  x6    <- randomArray gen (Z :.  6 * million)
  y6    <- randomArray gen (Z :.  6 * million)
  x8    <- randomArray gen (Z :.  8 * million)
  y8    <- randomArray gen (Z :.  8 * million)
  x10   <- randomArray gen (Z :. 10 * million)
  y10   <- randomArray gen (Z :. 10 * million)
  x12   <- randomArray gen (Z :. 12 * million)
  y12   <- randomArray gen (Z :. 12 * million)
  x14   <- randomArray gen (Z :. 14 * million)
  y14   <- randomArray gen (Z :. 14 * million)
  x16   <- randomArray gen (Z :. 16 * million)
  y16   <- randomArray gen (Z :. 16 * million)
  x18   <- randomArray gen (Z :. 18 * million)
  y18   <- randomArray gen (Z :. 18 * million)
  x20   <- randomArray gen (Z :. 20 * million)
  y20   <- randomArray gen (Z :. 20 * million)

  _     <- x2 `seq` x4 `seq` x6 `seq` x8 `seq` x10 `seq` x12 `seq` x14 `seq` x16 `seq` x18 `seq` x20 `seq` return ()
  _     <- y2 `seq` y4 `seq` y6 `seq` y8 `seq` y10 `seq` y12 `seq` y14 `seq` y16 `seq` y18 `seq` y20 `seq` return ()
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
    let r = CUDA.run $ dotp (use x2) (use y2)
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
  let -- run  xs   = CUDA.run . dotp (use xs) . use
      run xs   = CUDA.run1 (dotp (use xs))

  args  <- takeWhile (/= "--") `fmap` getArgs
  withArgs args $ defaultMain
    [ bgroup "dotp"
    $ P.map (\(label, xs, ys) -> bench label $ whnf (run xs) ys)
        [ ("2M",  x2,  y2)
        , ("4M",  x4,  y4)
        , ("6M",  x6,  y6)
        , ("8M",  x8,  y8)
        , ("10M", x10, y10)
        , ("12M", x12, y12)
        , ("14M", x14, y14)
        , ("16M", x16, y16)
        , ("18M", x18, y18)
        , ("20M", x20, y20)
        ]
    ]

