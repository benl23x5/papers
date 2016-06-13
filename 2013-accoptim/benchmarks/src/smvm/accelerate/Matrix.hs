{-# LANGUAGE BangPatterns, TupleSections #-}

module Matrix where

import Data.Int
import MatrixMarket
import System.Random.MWC
import System.IO.Unsafe

import Data.Vector.Unboxed                      (Vector, Unbox)
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as M
import qualified Data.Vector.Algorithms.Intro   as V

type CSRMatrix a = (Vector Int32, Vector (Int32, a), Int)


-- Read a sparse matrix from a MatrixMarket file. Pattern matrices are filled
-- with random numbers in the range (-1,1).
--
{-# INLINE readCSRMatrix #-}
readCSRMatrix :: GenIO -> FilePath -> IO (CSRMatrix Float)
readCSRMatrix gen file = do
  mtx <- readMatrix file
  case mtx of
    (RealMatrix    dim l vals) -> csr dim l vals
    (PatternMatrix dim l ix)   -> csr dim l =<< mapM' (\(a,b) -> (a,b,) `fmap` uniformR (-1,1) gen) ix
    (IntMatrix _ _ _)          -> error "IntMatrix type not supported"
    (ComplexMatrix _ _ _)      -> error "ComplexMatrix type not supported"


-- A randomly generated matrix of given size
--
{-# INLINE randomCSRMatrix #-}
randomCSRMatrix :: GenIO -> Int -> Int -> IO (CSRMatrix Float)
randomCSRMatrix gen rows cols = do
  segd <- randomVectorR ( 0, fromIntegral cols `div` 3) gen rows
  let nnz = fromIntegral $ V.sum segd
  inds <- randomVectorR ( 0, fromIntegral cols-1) gen nnz
  vals <- randomVectorR (-1,1) gen nnz
  return (segd, V.zip inds vals, cols)
  where
    randomVectorR :: (Unbox a, Variate a) => (a,a) -> GenIO -> Int -> IO (Vector a)
    randomVectorR r g n = V.replicateM n (uniformR r g)


-- Read elements into unboxed arrays, convert to zero-indexed compress sparse
-- row format.
--
{-# INLINE csr #-}
csr :: (Int,Int) -> Int -> [(Int32,Int32,Float)] -> IO (Vector Int32, Vector (Int32,Float), Int)
csr (m,_n) l elems = do
  mu <- M.new l         :: IO (M.IOVector (Int32,Int32,Float))

  let goe :: Int -> [(Int32,Int32,Float)] -> IO ()
      goe  _ []     = return ()
      goe !n (x:xs) = let (i,j,v) = x in M.unsafeWrite mu n (i-1,j-1,v) >> goe (n+1) xs
  goe 0 elems

  let cmp (x1,y1,_) (x2,y2,_) | x1 == x2  = compare y1 y2
                              | otherwise = compare x1 x2
  V.sortBy cmp mu

  (i,j,v) <- V.unzip3 `fmap` V.unsafeFreeze mu
  mseg    <- M.new m

  let gos :: Int -> Vector Int32 -> IO (Vector Int32)
      gos !n rows
        | n >= m        = V.unsafeFreeze mseg
        | otherwise     = let (s,ss) = V.span (== fromIntegral n) rows
                          in M.unsafeWrite mseg n (fromIntegral $ V.length s) >> gos (n+1) ss

  seg <- gos 0 i
  return (seg , V.zip j v, _n)


-- Lazier versions of things in Control.Monad
--
sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f as = sequence' (map f as)

