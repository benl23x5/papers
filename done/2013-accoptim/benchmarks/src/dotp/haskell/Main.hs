
module Main where

-- friends
import Random.Array

import qualified Solver.Repa                    as R
import qualified Solver.Vector                  as V
import qualified Solver.Accelerate              as A
import qualified Solver.CUBLAS                  as B

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A
import Data.Array.Accelerate.CUDA               as A


-- standard library
import Prelude                                  as P
import Data.List
import Control.Monad
import System.Environment
import System.Random.MWC
import Foreign.CUDA                             as CUDA
import Foreign.CUDA.BLAS                        as BLAS
import qualified Data.Vector.Storable           as V
import qualified Data.Array.Repa                as R

import Criterion
import Criterion.Main
import Criterion.Monad
import Criterion.Config
import Criterion.Environment


doTest :: Config -> (String -> Bool) -> Environment -> Handle -> Int -> IO ()
doTest cfg shouldRun env hdl x =
  let n         = x * 1000000   -- x million
      name grp  = grp ++ "/" ++ shows x "M"

      copyToDevice p = do
        d <- CUDA.mallocArray n
        CUDA.pokeArray n p d
        return d
  in when (P.any shouldRun (P.map name ["vector", "repa", "cublas", "accelerate"])) $ do

    -- Generate random data
    xs_arr      <- randomArrayIO (const uniform) (Z :. n)
    ys_arr      <- randomArrayIO (const uniform) (Z :. n)

    let ((),xs_vec) = toVectors xs_arr
        ((),ys_vec) = toVectors ys_arr

    xs_dev      <- V.unsafeWith xs_vec copyToDevice
    ys_dev      <- V.unsafeWith ys_vec copyToDevice

    xs_repa     <- R.computeUnboxedP (R.delay $ toRepa xs_arr)
    ys_repa     <- R.computeUnboxedP (R.delay $ toRepa ys_arr)

    -- Run the benchmark. A hacked up version of criterion's 'defaultMain' so
    -- that we don't continually call 'measureEnvironment'. All non-benchmarking
    -- paths are elided.
    --
    withConfig cfg $
      runAndAnalyse shouldRun env
      $ bgroup ""
        [ bench (name "vector")     $ nf (V.dotp xs_vec) ys_vec
        , bench (name "repa")       $ nfIO (R.dotp xs_repa ys_repa)
        , bench (name "cublas")     $ B.dotp hdl n xs_dev ys_dev
        , bench (name "accelerate") $ whnf (A.run1 (A.dotp (A.use xs_arr))) ys_arr
        ]

    -- cleanup
    CUDA.free xs_dev
    CUDA.free ys_dev


main :: IO ()
main = do
  -- Initialise the criterion environment
  (cfg, args)   <- parseArgs defaultConfig defaultOptions =<< getArgs
  env           <- withConfig cfg measureEnvironment

  let shouldRun b = P.null args || P.any (`isPrefixOf` b) args

  -- Initialise CUBLAS environment
  hdl <- BLAS.create

  -- Setup and run the benchmark suite. This reinitialises the environment every
  -- time, but means we don't keep so much live memory around.
  --
  mapM_ (doTest cfg shouldRun env hdl) [2,4..20]

  -- Shutdown CUBLAS and release device memory
  BLAS.destroy hdl

