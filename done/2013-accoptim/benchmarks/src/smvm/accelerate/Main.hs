
import SMVM
import Matrix

import Prelude                                  as P
import Criterion.Main                           ( defaultMain, bench, whnf )
import Data.List
import System.Random.MWC
import System.Environment
import qualified Data.Vector.Unboxed            as V

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.CUDA


main :: IO ()
main = do

  (args, rest)  <- break ("--" `isPrefixOf`) `fmap` getArgs
  let opts      = if P.null rest then [] else P.tail rest

  withArgs opts $ case args of
    [mtx]       -> runSMVM mtx
    _           -> error "usage: smvm matrix.mtx [-- Criterion options]"


runSMVM :: FilePath -> IO ()
runSMVM fileIn = withSystemRandom $ \gen -> do

  -- read matrix file, generate random vector to multiply against
  --
  (segd', svec', cols)  <- readCSRMatrix gen fileIn
  vec'                  <- uniformVector gen cols

  -- Convert to Accelerate arrays
  --
  let (ind',val')       = V.unzip svec'
      vec               = fromVectors (Z :. V.length vec')  ((), V.convert vec')
      segd              = fromVectors (Z :. V.length segd') ((), V.convert segd')
      ind               = fromVectors (Z :. V.length ind')  ((), V.convert ind')
      val               = fromVectors (Z :. V.length val')  ((), V.convert val')

      svec              = lift (use ind, use val)       :: Acc (SparseVector Float)
      smat              = lift (use segd, svec)         :: Acc (Segments Int32, SparseVector Float)

      go                = run1 (smvm smat)

  putStrLn $ "Reading matrix: " ++ fileIn
  putStrLn $ "  with shape: " ++ shows (V.length segd') " x " ++ shows cols " and "
                              ++ shows (V.length svec') " entries\n"

  -- Benchmark
  --
  defaultMain
    [ bench "smvm" $ whnf go vec ]

