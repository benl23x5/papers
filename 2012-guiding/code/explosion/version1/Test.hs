

-- | Examples of API problems in Repa 1.
module Test 
        ( doubleObvious
        , doubleForce
        , doubleDeepSeq
        , doubleManifest
        , doubleDeepSeqManifest)
where
import RepaLite
import Prelude          hiding (map, zipWith)


-- | This is the obvious way to write the function, but because we haven't used
--  'force' we get a delayed array which recomputes elements on the fly when we
--   index into it.
doubleObvious :: Array DIM2 Int -> Array DIM2 Int -> Array DIM2 Int
doubleObvious arr1 arr2
 = map (* 2) $ zipWith (+) arr1 arr2


-- | Adding the force function gets us a manifest array, but this fact isn't
--   obvious in the type signature. The code is also fairly slow repeatedly
--   tests whether the second array is Delayed for Manfiest for every iteration.
--   We have two copies of the worker function in the Core IR.
doubleForce :: Array DIM2 Int -> Array DIM2 Int -> Array DIM2 Int
doubleForce arr1 arr2
 = force $ map (* 2) $ zipWith (+) arr1 arr2


-- | Cutting away the Delayed cases means the program does not need to 
--   test the representation for each iteration.
doubleManifest :: Array DIM2 Int -> Array DIM2 Int -> Array DIM2 Int
doubleManifest arr1@Manifest{} arr2@Manifest{}
 = force $ map (* 2) $ zipWith (+) arr1 arr2


-- | Applying deepSeqArray by itself actually produces worse code, because this
--   function itself branches on whether the array is Manifest or Delayed.
--   We end up with four copies of the worker function in the Core IR.
doubleDeepSeq :: Array DIM2 Int -> Array DIM2 Int -> Array DIM2 Int
doubleDeepSeq arr1 arr2
 = arr1 `deepSeqArray` arr2 `deepSeqArray`
   force $ map (* 2) $ zipWith (+) arr1 arr2


-- | Also apply deepSeqArray to ensure the array shape is unboxed outside
--   the loop.
doubleDeepSeqManifest :: Array DIM2 Int -> Array DIM2 Int -> Array DIM2 Int
doubleDeepSeqManifest arr1@Manifest{} arr2@Manifest{}
 = arr1 `deepSeqArray` arr2 `deepSeqArray`
   force $ map (* 2) $ zipWith (+) arr1 arr2



