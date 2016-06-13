
module Main where

-- friends
import Random.Array

import qualified Solver.Accelerate      as A
import qualified Solver.SDK             as S

import Data.Array.Accelerate            as A
import Data.Array.Accelerate.IO         as A
import Data.Array.Accelerate.CUDA       as A

-- standard library
import Prelude                          as P
import Data.List
import Control.Monad
import System.Environment
import System.Random.MWC
import Foreign.CUDA                     as CUDA
import qualified Data.Vector.Storable   as V

import Criterion
import Criterion.Main
import Criterion.Monad
import Criterion.Config
import Criterion.Environment


-- Do a test at a specific number of elements (million) and return that result.
--
doTest :: Config -> (String -> Bool) -> Environment -> Int -> IO ()
doTest cfg shouldRun env x =
  let n         = x * 1000000   -- million
      name grp  = grp ++ "/" ++ shows x "M"

      copyToDevice vec =
        V.unsafeWith vec $ \ptr -> do
          dev <- CUDA.mallocArray n
          CUDA.pokeArray n ptr dev
          return dev

  in
  when (P.any shouldRun (P.map name ["sdk", "accelerate"])) $ do

    -- generate some random data
    --
    psy         <- randomArrayIO (\_ -> uniformR ((5,1,0.25), (30,100,10))) (Z:.n)
    let ((((),sp_vec), os_vec), oy_vec) = toVectors psy

    sp_dev      <- copyToDevice sp_vec
    os_dev      <- copyToDevice os_vec
    oy_dev      <- copyToDevice oy_vec

    -- Run the benchmark. A hacked up version of criterion's 'defaultMain' so
    -- that we don't continually call 'measureEnvironment'. All non-benchmarking
    -- paths are elided.
    --
    withConfig cfg $
      runAndAnalyse shouldRun env
      $ bgroup ""
        [ bench (name "sdk")        $ S.blackscholes n sp_dev os_dev oy_dev
        , bench (name "accelerate") $ whnf (run1 A.blackscholes) psy
        ]

    -- Cleanup
    --
    CUDA.free sp_dev
    CUDA.free os_dev
    CUDA.free oy_dev


main :: IO ()
main = do
  (cfg, args)   <- parseArgs defaultConfig defaultOptions =<< getArgs
  env           <- withConfig cfg measureEnvironment

  let shouldRun b = P.null args || P.any (`isPrefixOf` b) args

  mapM_ (doTest cfg shouldRun env) [2,4..20]

