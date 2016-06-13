{-# LANGUAGE TypeFamilies #-}

-- | Main types used to represent nested arrays.
module Data.Array.PArray.Base   
        ( PArray(..), PData (..), PDatas(..)
        , PR    (..)
        , VSegd (..)
        , SSegd (..)
        , Segd  (..))
where
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U


-- Array Types ----------------------------------------------------------------
-- | Parallel Arrays. (Called PA in the paper)
data PArray a 
        = PArray 
        { parrayLength  :: Int 
        , parrayData    :: PData a }


-- | Parallel array data with a linear index space.
data family PData a


-- | Parallel array data with a two dimensional index space.
data family PDatas a


-- Int Arrays -----------------------------------------------------------------
data instance PData  Int 
        = PInt  (U.Vector Int)

data instance PDatas Int 
        = PInts (V.Vector (U.Vector Int))


-- Char Arrays ----------------------------------------------------------------
data instance PData Char
        = PChar  (U.Vector Char)

data instance PDatas Char
        = PChars (V.Vector (U.Vector Char))


-- Nested Arrays --------------------------------------------------------------
data instance PData (PArray a)
        = PNested
        { pnestedVSegd  :: VSegd
        , pnestedPData  :: PDatas a }

data instance PDatas (PArray a)
        = PNesteds (V.Vector (PData (PArray a)))


-- | Virtual-segment descriptor.
data VSegd 
        = VSegd 
        { vsegdSegmap   :: U.Vector PsId
        , vsegdSSegd    :: SSegd }


-- | Scattered-segment descriptor.
data SSegd
        = SSegd 
        { ssegdSources  :: U.Vector DbId
        , ssegdStarts   :: U.Vector Int
        , ssegdSegd     :: Segd }


-- | Contiguous-segment descriptor.
data Segd
        = Segd 
        { segdLengths   :: U.Vector Int
        , segdIndices   :: U.Vector Int }


-- | Physical segment identifier.
type PsId       = Int


-- | Data block identifier.
type DbId       = Int


-- Array Operations -----------------------------------------------------------
class PR a where
 -- | Construct an empty data chunk.
 emptyPR      :: PData  a

 -- | Yield the length of a data chunk.
 lengthPR     :: PData  a -> Int                                   

 -- | Given a length and an array element,
 --   replicate the value the given number of times.
 replicatePR  :: Int          -> a       -> PData a

 -- | Given a vector and some array elements, 
 --   replicate each of the elements the corresponding number of times,
 --   returning the result concatenated in a flat array.
 replicatesPR :: U.Vector Int -> PData a -> PData a

 -- | Append two data chunks.
 appendPR     :: PData a  -> PData a -> PData a

 -- | Take the element at the given index in a chunk.
 indexPR      :: PData  a -> Int   -> a

 -- | Given some data chunks, a segment descriptor, and a vector of  
 --   segment ids and element indices within those segments.
 --   Lookup all the described elements and return them concatenated
 --   in a flat array.
 indexvsPR    :: PDatas a -> VSegd -> U.Vector (Int, Int) -> PData a

 -- | Given a data chunk, starting offset, and slice length.
 --   Extract the given elements, returning them in a new chunk.
 extractPR    :: PData  a -> Int   -> Int -> PData a

 -- | Given some data chunks and a segment descriptor,
 --   slice out all the segments described by the segment descriptor
 --   and return them concatenated in a new flat array.
 extractvsPR  :: PDatas a -> VSegd -> PData a

 -- | Given an array of elements and an array of flags of the same length
 --   keep only the elements that have their corresponding flag set
 --   to True.
 packPR       :: PData  a -> U.Vector Bool -> PData a

 -- | Combine two arrays accoring to a flags vector.
 combinePR    :: U.Vector Bool -> PData a  -> PData a -> PData a

 -- | Yield how many chunks there are in the given collection.
 lengthdPR    :: PDatas a -> Int

 -- | Construct an empty collection of element chunks.
 emptydPR     :: PDatas a

 -- | Construct collection containing a single chunk/
 singletondPR :: PData  a -> PDatas a

 -- | Retrieve the chunk with the given index.
 indexdPR     :: PDatas a -> Int        -> PData a

 -- | Append two collections of element chunks.
 appenddPR    :: PDatas a -> PDatas a   -> PDatas a

 -- | Concatenate a vector of chunk collections into a single one.
 concatdPR    :: V.Vector (PDatas a)    -> PDatas a

 -- | Filter some chunks according to some flags.
 packdPR      :: PDatas a -> U.Vector Bool -> PDatas a

 -- | Convert an list to an array. 
 --   Used for testing only.
 fromListPR   :: [a] -> PData a
