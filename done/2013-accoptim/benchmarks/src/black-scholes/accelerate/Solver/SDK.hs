{-# LANGUAGE ForeignFunctionInterface #-}

module Solver.SDK
  where

import Solver.Common

import Foreign
import Foreign.C
import Foreign.CUDA                     ( DevicePtr, withDevicePtr )


blackscholes :: Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> IO ()
blackscholes nopt price strike years =
  withDevicePtr price  $ \p_price  ->
  withDevicePtr strike $ \p_strike ->
  withDevicePtr years  $ \p_years  ->
  allocaArray nopt     $ \p_call   ->
  allocaArray nopt     $ \p_put    ->
    c_blackscholes p_call p_put p_price p_strike p_years (realToFrac riskfree) (realToFrac volatility) (fromIntegral nopt)

foreign import ccall unsafe "blackscholes"
  c_blackscholes :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float -> CFloat -> CFloat -> CInt -> IO ()

