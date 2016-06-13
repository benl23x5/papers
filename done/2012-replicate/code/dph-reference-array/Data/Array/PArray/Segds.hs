
-- | Segment descriptors.
module Data.Array.PArray.Segds
        ( -- * Virtual Segment Descriptors
          emptyVSegd
        , lengthVSegd
        , lengthsOfVSegd
        , replicatedVSegd
        , updateSegmapOfVSegd
        , appendVSegd
        , combineVSegd
        , demoteVSegd

          -- * Scattered Segment Descriptors.
        , emptySSegd
        , lengthSSegd
        , singletonSSegd
        , promoteSSegd
        , appendSSegd
        , demoteSSegd

          -- * Contiguous Segment Descriptors
        , emptySegd
        , lengthSegd
        , promoteSegd
        , segdOfLengths
        , appendSegd

          -- * Culling
        , cullOnSegmap
        , cullOnSSegd)
where
import Data.Array.PArray.Base
import Data.Array.PArray.Pretty ()
import Data.Vector.Unboxed              (Vector)
import qualified Data.Array.Unboxed     as U


-- VSegd ------------------------------------------------------------------------
-- | Construct an empty `VSegd` that represents an array with no segments.
--
--   O(1).
emptyVSegd :: VSegd
emptyVSegd
        = VSegd U.empty $ emptySSegd


-- | Yield the number of (logical) segments in a `VSegd`.
-- 
--   O(1).
lengthVSegd :: VSegd -> Int
lengthVSegd (VSegd segmap _)
        = U.length segmap


-- | Yield the lengths of the segments defined by a `VSegd`.
--
--   O(length vsegd)
lengthsOfVSegd :: VSegd -> Vector Int
lengthsOfVSegd (VSegd segmap (SSegd _ _ (Segd lengths _)))
        = U.backpermute lengths segmap


-- | Construct a `VSegd` that represents an array with a single segment replicated
--   multiple times.
--
--   O(reps).
replicatedVSegd :: Int -> Int -> VSegd
replicatedVSegd segLen reps
        = VSegd (U.replicate reps 0) (singletonSSegd segLen)


-- | Update the @segmap@ field of a `VSegd`,
--   then cull down physical segments that are no longer reachable.
updateSegmapOfVSegd :: (Vector Int -> Vector Int) -> VSegd -> VSegd
updateSegmapOfVSegd f (VSegd segmap ssegd)
 = let  (segmap', ssegd') = cullOnSegmap (f segmap) ssegd
   in   VSegd segmap' ssegd'


-- | Append two `VSegd`.
--   The first argument gives the number of PDatas in the first array.
--   
--   O(length vsegd1 + length vsegd2)
--   Depends on the invariant that all physical segments are reachable by
--   some virtual segment so that we have 
--       (length ssegd1 <= length vsegd1)
--   and (length ssegd2 <= length vsegd2)
appendVSegd :: Int -> VSegd -> VSegd -> VSegd
appendVSegd ps1 (VSegd segmap1 ssegd1) (VSegd segmap2 ssegd2)
        = VSegd (segmap1 U.++ (U.map (+ lengthSSegd ssegd1) segmap2))
        $ appendSSegd ps1 ssegd1 ssegd2


-- | Combine two `VSegds` according to a flags vector.
--   
--   O(length segmap')
combineVSegd 
        :: Vector Bool
        -> VSegd -> Int    -- ^ `VSegd` of first array and number of data blocks
        -> VSegd           -- ^ `VSegd` of second array.
        -> VSegd

combineVSegd flags
             (VSegd segmap1 ssegd1) pdatas1
             (VSegd segmap2 ssegd2)

 = let  -- segmap relative to combined psegs
        segmap2' = U.map (+ (U.length segmap1)) segmap2

        -- combine the segmaps
        segmap'  = U.combine2 flags segmap1 segmap2'

        -- All data from the source arrays goes into the result, 
        -- so the ssegds are effectively appended.
        ssegd'   = appendSSegd pdatas1 ssegd1 ssegd2

   in   VSegd segmap' ssegd'


-- | Demote a `VSegd` to a plain `SSegd`.
--
--   O(length vsegd).
demoteVSegd  :: VSegd -> SSegd
demoteVSegd (VSegd segmap (SSegd sources starts (Segd lengths _)))
        = SSegd (U.backpermute sources segmap)
                (U.backpermute starts  segmap)
        $ segdOfLengths 
                (U.backpermute lengths segmap)
        

-- SSegd ----------------------------------------------------------------------
-- | Construct an empty `SSegd` that represents an array with no segments.
--
--   O(1).
emptySSegd :: SSegd
emptySSegd
        = SSegd U.empty U.empty emptySegd


-- | Yield the number of (physical) segments in a `SSegd`.
--
--   O(1).
lengthSSegd :: SSegd -> Int
lengthSSegd (SSegd sources _ _)
        = U.length sources


-- | Construct a `SSegd` that represents an array with a single segment
--   in a single flat data array.
--
--   O(1).
singletonSSegd :: Int -> SSegd
singletonSSegd segLen
        = SSegd (U.singleton 0) (U.singleton 0)
        $ segdOfLengths (U.singleton segLen)


-- | Promote a `SSegd` to a `VSegd`.
--
--   O(length ssegd).
promoteSSegd :: SSegd -> VSegd
promoteSSegd ssegd@(SSegd sources _ _)
        = VSegd (U.enumFromN 0 (U.length sources))
        $ ssegd


-- | Append two `SSegd`.
--
--   O(length ssegd1 + length ssegd2).
--   Depends on the invariant that the length of each Segd is equal
--   to the length of its enclosing SSegd
appendSSegd :: Int -> SSegd -> SSegd -> SSegd
appendSSegd ps1 (SSegd sources1 starts1 segd1)
                (SSegd sources2 starts2 segd2)
        = SSegd (sources1 U.++ U.map (+ ps1) sources2)
                (starts1  U.++ starts2)
        $ appendSegd segd1 segd2


-- | O(1).
--   Demote a `SSegd` to a plain `Segd`.
demoteSSegd  :: SSegd -> Segd
demoteSSegd (SSegd _ _ segd)
        = segd


-- Segd ------------------------------------------------------------------------
-- | Construct an empty `Segd` that represents an array with no segments.
--
--   O(1).
emptySegd :: Segd
emptySegd = Segd U.empty U.empty


-- | Yield the number of segments in a `SSegd`.
--
--   O(1).
lengthSegd :: Segd -> Int
lengthSegd (Segd lengths _)
        = U.length lengths


-- | Promote a `Segd` to a `SSegd`.
--
--   O(length segd).
promoteSegd  :: Segd -> SSegd
promoteSegd segd@(Segd lengths indices)
        = SSegd (U.replicate (U.length lengths) 0)
                indices
        $ segd


-- | Construct a `Segd` from a vector of segments lengths.
--
--   O(length ns).
segdOfLengths :: Vector Int -> Segd
segdOfLengths ns
        = Segd  ns
                (if U.null ns
                        then U.empty
                        else U.init (U.scanl (+) 0 ns))


-- | Append two `Segd`.
--
--   O(length segd1 + length segd2).
appendSegd :: Segd -> Segd -> Segd
appendSegd (Segd lengths1 indices1)
           (Segd lengths2 indices2)
 = let elems1 = U.sum lengths1
   in  Segd  (lengths1 U.++ lengths2)
             (indices1 U.++ U.map (+ elems1) indices2)


-- Cull -----------------------------------------------------------------------
-- | Drop physical segments in a SSegd that are unrechable from some segmap,
--   and rewrite the segmap to match the result.
--  
--   O(max (length segmap, length ssegd))
cullOnSegmap :: Vector Int -> SSegd -> (Vector Int, SSegd)
cullOnSegmap segmap (SSegd sources starts (Segd lengths _))
 = (segmap', ssegd')
 where
        (used_flags, used_map) 
                  = makeCullMap (U.length sources) segmap 

        -- Use the map to rewrite the segmap to point to the corresponding
        -- psegs in the result.
        --  Example:   segmap:  [0 1 1 3 5 5 6 6]
        --           used_map:  [0 1 -1 2 -1 3 4]
        --            segmap':  [0 1 1 2 3 3 4 4]
        segmap'  = U.map (used_map U.!) segmap

        -- Drop unreachable psegs descriptions from the SSegd.
        starts'   = U.pack starts  used_flags
        sources'  = U.pack sources used_flags
        lengths'  = U.pack lengths used_flags

        ssegd'    = SSegd sources' starts' 
                  $ segdOfLengths lengths'


-- | Drop data chunks in a PDatas that are unreachable from a SSegd,
--   and rewrite the SSegd to match the result.
cullOnSSegd :: PR a => SSegd -> PDatas a -> (SSegd, PDatas a)
cullOnSSegd (SSegd sources starts segd) pdatas
 = (ssegd', pdatas')
 where
        (used_flags, used_map)
                = makeCullMap (lengthdPR pdatas) sources

        -- Rebuild the SSegd.
        sources' = U.map (used_map U.!) sources
        ssegd'   = SSegd sources' starts segd

        -- Drop unreachable chunks from the PDatas.
        pdatas'  = packdPR pdatas used_flags 


makeCullMap :: Int -> Vector Int -> (Vector Bool, Vector Int)
makeCullMap total used
 = (flags, used_map)
 where  -- Make an array of flags signalling whether each element
        -- is used not not.
        -- Example: used:  [0 1 1 3 5 5 6 6]
        --       => flags: [T T F T F T T]
        flags
         = U.backpermuteDft total (const False)
         $ U.zip used 
                 (U.replicate (U.length used) True)

        -- Make a set of used indices.
        --  Example: flags:    [T T F T F T T]
        --       =>  uset_set: [0 1 3 5 6]
        used_set
         = U.pack (U.enumFromN 0 (U.length flags))
                  flags

        -- Make am array that maps used elements in the source array
        -- onto elements in the result array.
        -- If a particular element isn't used this maps to -1.
        -- Example: used_set:  [0 1 3 5 6]
        --          used_map:  [0 1 -1 2 -1 3 4]
        used_map
         = U.backpermuteDft total (const (-1 :: Int))
         $ U.zip used_set 
                 (U.enumFromN 0 (U.length used_set))

