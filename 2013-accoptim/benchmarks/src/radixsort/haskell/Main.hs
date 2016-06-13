
module Main where

-- friends
import Random.Array

import qualified Solver.Vector                  as V
import qualified Solver.Accelerate              as A

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A
import Data.Array.Accelerate.CUDA               as A

-- standard library
import Prelude                                  as P
import Data.List
import Control.Monad
import System.Environment
import System.Random.MWC
import qualified Data.Vector.Unboxed            as V

import Criterion
import Criterion.Main
import Criterion.Monad
import Criterion.Config
import Criterion.Environment


doTest :: Config -> (String -> Bool) -> Environment -> Int -> IO ()
doTest cfg shouldRun env x =
  let n         = x * 1000000   -- x million
      name grp  = grp ++ "/" ++ shows x "M"

  in when (P.any shouldRun (P.map name ["vector", "accelerate"])) $ do

    -- Generate random data
    xs_arr      <- randomArrayIO (const uniform) (Z :. n)       :: IO (Vector Int32)
    let xs_vec  =  V.convert $ P.snd (toVectors xs_arr)

    -- Run the benchmark. A hacked up version of criterion's 'defaultMain' so
    -- that we don't continually call 'measureEnvironment'. All non-benchmarking
    -- paths are elided.
    --
    withConfig cfg $
      runAndAnalyse shouldRun env
      $ bgroup ""
        [ bench (name "vector")     $ nf V.sort xs_vec
        , bench (name "accelerate") $ whnf (A.run1 A.sort) xs_arr
        ]


main :: IO ()
main = do
  -- Initialise the criterion environment
  (cfg, args)   <- parseArgs defaultConfig defaultOptions =<< getArgs
  env           <- withConfig cfg measureEnvironment

  let shouldRun b = P.null args || P.any (`isPrefixOf` b) args

  -- Setup and run the benchmark suite. This reinitialises the environment every
  -- time, but means we don't keep so much live memory around.
  --
  mapM_ (doTest cfg shouldRun env) [2,4..20]

