{-# LANGUAGE BangPatterns #-}

module Test2 where
import RepaLite
import Prelude          hiding (map, zipWith)


powerAdd :: Array DIM2 Int -> Array DIM2 Int -> Int -> Array DIM2 Int
powerAdd arr1@Manifest{} arr2@Manifest{} i
 = go i arr1
 where  go 0  acc = acc
        go !n acc = go (n - 1) (force $ zipWith (+) (zipWith (*) arr1 arr1) arr2)

