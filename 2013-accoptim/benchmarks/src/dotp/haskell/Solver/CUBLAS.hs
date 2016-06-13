module Solver.CUBLAS
  where

import Foreign.CUDA
import Foreign.CUDA.BLAS


dotp :: Handle -> Int -> DevicePtr Float -> DevicePtr Float -> IO Float
dotp hdl n xs ys =
  sdot hdl n xs 1 ys 1

