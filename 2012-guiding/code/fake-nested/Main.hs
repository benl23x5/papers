{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa
import Solver
import Debug.Trace

main 
 = do   let !xs  = fromListUnboxed (Z :. 100)        (trace "xs" 0 : [1..99])
        let !ys  = fromListUnboxed (Z :. 100 :. 100) (trace "ys" 0 : [1..9999])
        print   $ foo xs ys