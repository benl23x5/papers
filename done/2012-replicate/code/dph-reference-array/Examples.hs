
-- These are the examples from the paper.
-- In GHCi you can display the logical and physical views of an array like:
--
-- @ > logical arr5
-- ["AB", "CDE", "H"]
-- @
--
-- @ > physical arr5
--  PArray 3
--   PNested
--    VSegd  segmap: [0,1,2]
--    SSegd sources: [0,0,1]
--           starts: [1,3,4]
--     Segd lengths: [2,3,1]
--          indices: [0,2,5]
--    PChars 0: "XABCDE"
--           1: "FGXXHXXX"
-- @
module Examples
        ( module Data.Array.PArray.Pretty
        , module Vectorised.Furthest
        , module Vectorised.Retrieve
        , module Vectorised.Retsum
        , uvec, parr, pparr, psarr

        -- Section 2: The Asymptotic Complexity Problem
        , sec2_xss
        , sec2_iss
        , sec2_result
        , sec2_xss1
        , sec2_xss2
        , sec2_xss3

        -- Section 4: New Representation for Nested Arrays
        , figure3
        , arr1
        , arr2
        , arr3
        , arr4
        , arr5

        -- Section 5: Projection, concatenation and Reduction
        , arr6

        , arrC0, arrC0'
        , arrC1, arrC1', arrC1''

        , arrC2
        , arrC2'
        , arrC2''
        , arrC2'''

        , arr7
        , arr7_ssegd
        , arr7_segd

        , arr8

        , sec5_xss
        , sec5_iss
        , sec5_result

        , arr9)
where
import Vectorised.Furthest
import Vectorised.Retrieve
import Vectorised.Retsum
import Data.Array.Lifted                ()
import Data.Array.PArray
import Data.Array.PArray.Pretty 
import Data.Array.PArray.Base
import Data.Array.PArray.Nested         ()
import Data.Array.PArray.Segds
import qualified Data.Vector            as V
import qualified Prelude                as P
import Prelude (Int, Char, Bool(..), ($))


-------------------------------------------------------------------------------
-- Section 2: The Asymptotic Complexity Problem.
--
sec2_xss :: PArray (PArray Char)
sec2_xss  = psarr ["AB", "CDE", "FG", "H"]

sec2_iss :: PArray (PArray Int)
sec2_iss  = psarr [[1, 0, 1], [2], [1, 0], [0]]

sec2_result :: PArray (PArray Char)
sec2_result 
          = retrieve_v sec2_xss sec2_iss

sec2_xss1 :: PArray (PArray Char)
sec2_xss1 = replicates (uvec [3, 1, 2, 1]) sec2_xss

sec2_xss2 :: PArray Char
sec2_xss2 = index_l 7 sec2_xss1 (concat sec2_iss)

sec2_xss3 :: PArray (PArray Char)
sec2_xss3 = unconcat sec2_iss sec2_xss2


-------------------------------------------------------------------------------
-- Section 4: New Representation of Nested Arrays.
--
figure3 :: PArray (PArray Char)
figure3 
        = PArray 7
        $ PNested
          ( VSegd (uvec [0, 0, 0, 1, 2, 2, 3])
          $ SSegd (uvec [0, 0, 1, 1])             -- sources
                  (uvec [1, 3, 0, 4])             -- starts
          $  Segd (uvec [2, 3, 2, 1])             -- lengths
                  (uvec [0, 2, 5, 7]))            -- indices
          ( PChars
          $ V.fromList
                [ uvec "XABCDE"
                , uvec "FGXXHXXX"])


-- Section 4.3: Replicates again ----------------
-- This produces the culled version, 
-- that obeys invariants 6 and 7.
arr1    :: PArray (PArray Char)
arr1    = replicates (uvec [0, 0, 1, 1, 0, 0, 1]) 
                     figure3


-- Section 4.4: Plain replicate -----------------
arr2    :: PArray (PArray (PArray Char))
arr2    = replicate 2 figure3


-- Section 4.5: Append --------------------------
arr3    :: PArray (PArray Char)
arr3    = psarr ["K", "", "LMNO"]

arr4    :: PArray (PArray Char)
arr4    = append figure3 arr3

arr5    :: PArray (PArray Char)
arr5    = arr1


-------------------------------------------------------------------------------
-- Section 5: Projection, Concatenation and Reduction
--
-- Section 5.1: Index and Extract ---------------
arr6    :: PArray (PArray Char)
arr6    = extract arr4 4 2 


-- Section 5.2: Concatenation -------------------
-- These arrays aren't in the paper, but they are nice examples for how 
-- concatenation works.
arrC0 :: PArray (PArray Int)
arrC0   = append (replicates (uvec [3, 2]) 
                        (psarr [[1, 2], [4, 5, 6]]))
                 (replicate 2 
                        (parr [7]))

arrC0' :: PArray Int
arrC0'  = concat arrC0


-- Here is a triply nested array with multiple data blocks.
--  Note how all the data blocks go into the concatenated array, 
--  but we don't need to touch the elements in them. 
--  Only the outermost segment descriptors are merged.
arrC1   :: PArray (PArray (PArray Int))
arrC1   = append 
         (replicate 2 
                (replicates (uvec [2, 1, 1]) 
                        (pparr [1, 2] `append` pparr [] `append` pparr [3])))
         (replicate 2
                (psarr [[5, 6], [7]]))

arrC1'  :: PArray (PArray Int)
arrC1'  = concat arrC1

arrC1'' :: PArray Int
arrC1'' = concat arrC1'


arrD1   :: PArray (PArray (PArray Int))
arrD1   = append 
         (replicate 1
                (replicates (uvec [1, 2])
                        (pparr [8, 9] `append` pparr [3])))
         (replicate 3
                (replicates (uvec [1, 2, 1]) 
                        (pparr [] `append` pparr [0, 1] `append` pparr [2])))


-- Here is a quad-nested array, which has three layers of segment descriptors.
arrC2   :: PArray (PArray (PArray (PArray Int)))
arrC2   = append (replicate 2 (singleton $ concat arrC1))
                 (singleton (append (singleton arrC0) (singleton sec2_iss)))


-- Concatenating a quad-nested array merges
--   the two outer-most segment descriptor layers, 
--   leaving the inner ones untouched.
arrC2'  :: PArray (PArray (PArray Int))
arrC2'  = concat arrC2


-- Concatenating it again
--   merges the segment descriptors, 
--   but leaves the inner data blocks untouched.
arrC2'' :: PArray (PArray Int)
arrC2'' = concat arrC2'


-- The element data is pulled through the segment descriptor
-- in the final concatenation.
arrC2''' :: PArray Int
arrC2''' = concat arrC2''


-- Section 5.3.1: Demotion ----------------------
arr7    :: PArray (PArray Char)
arr7    = PArray 6
        $ PNested
          ( VSegd (uvec [2, 1, 4, 2, 3, 0])     -- segmap
          $ SSegd (uvec [1, 0, 1, 0, 0])        -- sources
                  (uvec [0, 2, 1, 0, 0])        -- starts
          $  Segd (uvec [1, 1, 3, 2, 0])        -- lengths
                  (uvec [0, 1, 2, 4, 6]))       -- indices
          ( PChars
          $ V.fromList
                [ uvec "EFG"
                , uvec "ABCD" ])


-- Demote the VSegd of arr7.
arr7_ssegd :: SSegd
arr7_ssegd = demoteVSegd $ pnestedVSegd $ parrayData arr7


-- Demote above SSegd again.
arr7_segd :: Segd
arr7_segd  = demoteSSegd arr7_ssegd


-- Section 5.3.3: Unconcatenation ---------------
arr8 :: PArray (PArray Char)
arr8    = normalise2 arr7


-- Section 5.4: Reduction and Dynamic Hoisting --
sec5_xss    :: PArray (PArray Int)
sec5_xss    = psarr [[1, 2], [4, 5, 6], [8]]

sec5_iss    :: PArray (PArray Int)
sec5_iss    = psarr [[1, 0, 1], [1, 2], [0]]

sec5_result :: PArray (PArray Int)
sec5_result = retsum_v sec5_xss sec5_iss

arr9    :: PArray (PArray Int) 
arr9    = replicates (lengths sec5_iss) sec5_xss

