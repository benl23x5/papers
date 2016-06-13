
-- | User-level interface to nested arrays.
--   These are wrappers for the lower-level *PR functions
--   and segment descriptor operators.
module Data.Array.PArray
        ( PArray(..)
        , PR    (..)
        , length
        , lengths
        , singleton

        -- * Replicate / Replicates
        , replicate
        , replicates

        -- * Append
        , append

        -- * Index and Extract
        , index
        , index_l,  indexlPR
        , extract

        -- * Concat and Unconcat
        , concat,   concatPR
        , unconcat

        -- * Normalise
        , normalise2
        , normalise3
        , normalise4

        -- * Reduction
        , sum
        , sum_l
        , maximum
        , maximum_l
        , add_l

        -- * Pack and Combine
        , pack

        -- * Helpers
        , uvec
        , parr
        , pparr
        , psarr)
where
import Data.Array.PArray.Pretty         ()
import Data.Array.PArray.Nested         ()
import Data.Array.PArray.Tuple          ()
import Data.Array.PArray.Int            ()
import Data.Array.PArray.Char           ()
import Data.Array.PArray.Unit           ()
import Data.Array.PArray.Segds
import Data.Array.PArray.Base
import qualified Data.Vector.Unboxed    as U
import qualified Prelude                as P
import Prelude ( Int, Bool(..)
               , otherwise, error
               , ($), (+), (-), (/=))


-- | Take the length of an array.
length :: PR a => PArray a -> Int
length (PArray n _)
        = n

-- | Take the segment lengths of a nested array.
lengths :: PR a   => PArray (PArray a) -> U.Vector Int
lengths (PArray _ (PNested vsegd _))
        = segdLengths $ demoteSSegd $ demoteVSegd vsegd


-- | Construct a singleton array.
singleton :: PR a => a -> PArray a
singleton x 
        = replicate 1 x


-- Replicate / Replicates -----------------------------------------------------
-- | Replicate an element by the given count.
replicate :: PR a => Int -> a -> PArray a
replicate n arr
        = PArray n (replicatePR n arr)


-- | Segmented replicate.
replicates :: PR a => U.Vector Int -> PArray a -> PArray a
replicates reps (PArray n pdata)
 | U.length reps /= n = error "replicates: length mismatch"
 | otherwise          = PArray (U.sum reps) (replicatesPR reps pdata)


-- Append ---------------------------------------------------------------------
-- | Append two arrays.
append :: PR a => PArray a -> PArray a -> PArray a
append (PArray n1 pdata1) (PArray n2 pdata2)
        = PArray (n1 + n2) (appendPR pdata1 pdata2)


-- Index ----------------------------------------------------------------------
-- | Indexing
index    :: PR a => PArray a -> Int -> a
index (PArray _ pdata) ix
        = indexPR pdata ix


-- | Lifted indexing. 
index_l  :: PR a => Int -> PArray (PArray a) -> PArray Int -> PArray a
index_l c (PArray _ xss) (PArray _ is)
        = PArray c $ indexlPR c xss is


indexlPR :: PR a => Int -> PData (PArray a) -> PData Int -> PData a
indexlPR c (PNested vsegd pdatas) (PInt is)
        = indexvsPR pdatas vsegd 
        $ U.zip (U.enumFromTo 0 (c - 1))
                is


-- | Extract a subarray
extract :: PR a => PArray a -> Int -> Int -> PArray a
extract (PArray _ pdata) start len
        = PArray len (extractPR pdata start len)


-- Concat / Unconcat ----------------------------------------------------------
-- | Concatenate an array.
concat  :: PR a => PArray (PArray a) -> PArray a
concat (PArray _ (PNested vsegd pdatas))
 = let  pdata   = extractvsPR pdatas vsegd
   in   PArray (lengthPR pdata) pdata


concatPR :: PR a => PData (PArray a) -> PData a
concatPR (PNested vsegd pdatas)
        = extractvsPR pdatas vsegd


-- | Impose the binding structure from the first array onto the second.
unconcat :: PR b => PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat (PArray n (PNested vsegd _)) (PArray _ pdata)
 = let  segd    = demoteSSegd  $ demoteVSegd vsegd
        vsegd'  = promoteSSegd $ promoteSegd segd
   in   PArray n (PNested vsegd' (singletondPR pdata))


-- Normalise ------------------------------------------------------------------
-- | Normalise the representation of an array so that all the data 
--   is a contiguous array.
normalise2 :: PR a => PArray (PArray a) -> PArray (PArray a)
normalise2 arr1
 = let  arr0    = concat arr1
   in   unconcat arr1 arr0


-- | Normalise the representation of an array so that all the data 
--   is a contiguous array.
normalise3 :: PR a => PArray (PArray (PArray a)) -> PArray (PArray (PArray a))
normalise3 arr2
 = let  arr1    = concat arr2
        arr0    = concat arr1
   in   unconcat arr2 (unconcat arr1 arr0) 


-- | Normalise the representation of an array so that all the data 
--   is a contiguous array.
normalise4
        :: PR a 
        => PArray (PArray (PArray (PArray a)))
        -> PArray (PArray (PArray (PArray a)))

normalise4 arr3
 = let  arr2    = concat arr3
        arr1    = concat arr2
        arr0    = concat arr1
   in   unconcat arr3 (unconcat arr2 (unconcat arr1 arr0))


-- Reduction ------------------------------------------------------------------
-- | Sum the elements of an integer array.
sum :: PArray Int -> Int
sum (PArray _ (PInt xs))
        = U.sum xs


-- | Lifted sum.
sum_l :: Int -> PArray (PArray Int) -> PArray Int
sum_l c arr
        = PArray c
        $ PInt $ uvec
        $ P.map sum 
        $ P.map (index arr) [0..c P.- 1]


-- | Yield the maximum of an array of integers.
maximum :: PArray Int -> Int
maximum (PArray _ (PInt xs))
        = U.maximum xs


-- | Lifted maximum.
maximum_l :: Int -> PArray (PArray Int) -> PArray Int
maximum_l c arr
        = PArray c
        $ PInt $ uvec
        $ P.map maximum
        $ P.map (index arr) [0..c P.- 1]


-- | Lifted addition.
add_l :: Int -> PArray Int -> PArray Int -> PArray Int
add_l c (PArray _ (PInt xs)) (PArray _ (PInt ys))
        = PArray c (PInt $ U.zipWith (P.+) xs ys)


-- Pack and Combine -----------------------------------------------------------
pack :: PR a => PArray a -> U.Vector Bool -> PArray a
pack (PArray _ pdata) flags
 = let  pdata'  = packPR pdata flags
        n'      = lengthPR pdata'
   in   PArray n' pdata'


-- Helpers for Constructing arrays --------------------------------------------
-- | Construct an unboxed vector of elements.
uvec    :: U.Unbox a => [a] -> U.Vector a
uvec    is = U.fromList is


-- | Construct a parallel array of `Int`.
parr    :: PR a => [a] -> PArray a
parr  is = PArray (P.length is) $ fromListPR is


-- | Construct a nested array containing a single segment of `Int`.
pparr   :: PR a => [a] -> PArray (PArray a)
pparr is = singleton $ PArray (P.length is) $ fromListPR is


-- | Construct a nested array containing several segments of `Int`.
psarr   :: PR a => [[a]] -> PArray (PArray a)
psarr is = PArray  (P.length is)
          $ PNested (promoteSSegd $ promoteSegd 
                        $ segdOfLengths 
                        $ U.fromList $ P.map P.length is)
                    (singletondPR $ fromListPR $ P.concat is)

