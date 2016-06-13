
-- | PR instance for nested arrays.
module Data.Array.PArray.Nested
where
import Data.Array.PArray.Base
import Data.Array.PArray.Segds
import Data.Array.PArray.Int            ()
import Data.Array.PArray.Pretty         ()      
import qualified Data.Array.Unboxed     as U
import qualified Data.Array.Vector      as V


instance PR a => PR (PArray a) where

 -- O(1).
 emptyPR 
  = PNested  emptyVSegd emptydPR

 -- O(1).
 lengthPR (PNested vsegd _)
  = lengthVSegd vsegd

 -- O(length result) = O(c)
 replicatePR c (PArray n pdata)
  = PNested (replicatedVSegd n c) 
            (singletondPR pdata)

 -- O(max(length segmap', length segmap))
 -- Complexity depends on the cull operation performed by updateVSegidsOfVSegd
 -- being linear in the length of the argument and return vsegd.
 -- The initial vsegd must also obey the invariant that all physical segments
 -- are reachable from some virtual segment.
 replicatesPR ns (PNested (VSegd segmap ssegd) pdatas)
  = PNested (VSegd segmap' ssegd'') pdatas'
  where (segmap', ssegd') 
         = cullOnSegmap (U.replicates ns segmap) ssegd

        (ssegd'',  pdatas')
         = cullOnSSegd  ssegd' pdatas


 -- O(length result) = O(length vsegd1 + length vsegd2)
 appendPR (PNested vsegd1 pdatas1) (PNested vsegd2 pdatas2)
  = PNested (appendVSegd (lengthdPR pdatas1) vsegd1 vsegd2)
            (appenddPR pdatas1 pdatas2)

 -- index -----------------------------
 -- O(1) for scalar return values, O(length result) otherwise.
 -- Constant time indexPR for scalar return values depends on O(1) slice being
 -- provided by the Data.Vector library. 
 -- See the comments for the (PData Int) instance.
 indexPR (PNested (VSegd segmap
                  (SSegd sources starts
                  (Segd  lengths _))) pdatas) 
         ix
  = let psegid  = segmap  U.! ix
        source  = sources U.! psegid
        start   = starts  U.! psegid
        len     = lengths U.! psegid
        pdata   = indexdPR pdatas source
    in  PArray len (extractPR pdata start len)


 -- O(length result) = O(length segixs)
 indexvsPR (PNesteds pdatas) vsegd1 srcixs
  = let
        -- O(length segixs)
        (lengths, starts, flats)
         = V.unzip3
         $ V.map (\(ix1, ix2)
                -> let  
                        -- Index into the outer array.
                        ssegd1  = vsegdSSegd   vsegd1
                        psegid1 = vsegdSegmap  vsegd1 U.! ix1
                        source1 = ssegdSources ssegd1 U.! psegid1
                        start1  = ssegdStarts  ssegd1 U.! psegid1

                        -- Index into the inner arrays.
                        arr2    = pdatas  V.! source1
                        vsegd2  = pnestedVSegd arr2
                        ssegd2  = vsegdSSegd   vsegd2
                        segd2   = ssegdSegd    ssegd2
                        psegid2 = vsegdSegmap  vsegd2 U.! (start1 + ix2)
                        source2 = ssegdSources ssegd2 U.! psegid2
                        start2  = ssegdStarts  ssegd2 U.! psegid2
                        length2 = segdLengths  segd2  U.! psegid2
                        flat2   = pnestedPData   arr2 `indexdPR` source2
                
                   in   (length2, start2, flat2))
                $ V.convert srcixs
        
        -- O(length segixs)
        vsegd'  = promoteSSegd
                $ SSegd (U.enumFromN 0 (U.length srcixs)) 
                        (U.convert starts) 
                $ segdOfLengths (U.convert lengths)

        -- O(length flats) = O(length segixs)
        pdatas' = concatdPR
                $ V.map singletondPR flats

   in   PNested vsegd' pdatas'
                            

 -- extract ---------------------------
 extractPR (PNested (VSegd segmap ssegd) pdatas) start len
  = PNested (VSegd segmap' ssegd'') pdatas'
  where (segmap', ssegd')
         = cullOnSegmap (U.slice start len segmap) ssegd

        (ssegd'', pdatas')
         = cullOnSSegd ssegd' pdatas


 extractvsPR (PNesteds pdatas) vsegd 
  = let ssegd       = demoteVSegd vsegd
        lengths     = segdLengths $ ssegdSegd ssegd
        sources     = ssegdSources ssegd

        -- Get the array id for each segment in the result.
        src_sources = U.replicates lengths sources
        
        -- Gather up the segmaps from each source array
        segmaps     = PInts $ V.map (vsegdSegmap . pnestedVSegd) pdatas
        sourcess_v  = V.map (ssegdSources . vsegdSSegd . pnestedVSegd) pdatas
        startss_v   = V.map (ssegdStarts  . vsegdSSegd . pnestedVSegd) pdatas
        lengthss_v  = V.map (segdLengths  . ssegdSegd  . vsegdSSegd . pnestedVSegd) pdatas

        -- Get the pseg id to use for each segment in the result, 
        -- relative to the source arrays.
        PInt src_psegids = extractvsPR segmaps vsegd

        -- Because all the flat arrays go into the result, 
        -- we need to adjust the source ids from the original arrays.
        psrcoffset  = V.convert $ V.prescanl (+) 0 
                    $ V.map (lengthdPR . pnestedPData) pdatas

        -- Get the flat array id for each segment in the result.
        dst_sources = U.zipWith (\src pseg -> (sourcess_v V.! src) U.! pseg 
                                           +  psrcoffset U.! src)
                                src_sources src_psegids
        
        -- Get the starting index for each segment in its flat array.
        dst_starts  = U.zipWith (\src pseg -> (startss_v V.! src) U.! pseg)
                                src_sources src_psegids

        -- Get the length of each segment in the result.
        dst_lengths = U.zipWith (\src pseg -> (lengthss_v V.! src) U.! pseg)
                                src_sources src_psegids

        -- Build the SSegd for the result.
        -- This references all data chunks in the source.
        ssegd_all   = SSegd dst_sources dst_starts
                    $ segdOfLengths dst_lengths

        pdatas_all  = concatdPR $ V.map pnestedPData pdatas

        -- Cull the chunks from the source array so we the SSegd only 
        -- references the ones needed in the result.
        (ssegd_culled, pdatas_culled)
                    = cullOnSSegd ssegd_all pdatas_all

        -- Build the final VSegd
        vsegd'      = promoteSSegd ssegd_culled

   in   PNested vsegd' pdatas_culled


 -- pack ------------------------------
 -- O(length segmap', length segmap).
 packPR (PNested (VSegd segmap ssegd) pdatas) flags
  = PNested (VSegd segmap' ssegd'') pdatas'
  where (segmap', ssegd') 
         = cullOnSegmap (U.pack segmap flags) ssegd

        (ssegd'',  pdatas')
         = cullOnSSegd  ssegd' pdatas

 -- combine ---------------------------
 combinePR flags (PNested vsegd1 pdatas1) (PNested vsegd2 pdatas2)
  = let vsegd'  = combineVSegd flags vsegd1 (lengthdPR pdatas1) vsegd2
        pdatas' = appenddPR pdatas1 pdatas2
    in  PNested vsegd' pdatas'

 -- conversion ------------------------
 fromListPR xss
  = let vsegd   = promoteSSegd $ promoteSegd 
                $ segdOfLengths
                $ U.fromList 
                $ map parrayLength xss

        pdatas  = concatdPR 
                $ V.map (singletondPR . parrayData) 
                $ V.fromList xss
 
    in  PNested  vsegd pdatas


 -- PDatas operators ------------------
 -- O(1).
 emptydPR
  = PNesteds $ V.empty

 -- O(1).
 singletondPR pdata
  = PNesteds $ V.singleton pdata

 -- O(length result) = O(length pdatas1 + length pdatas2).
 appenddPR (PNesteds pdatas1) (PNesteds pdatas2)
  = PNesteds (pdatas1 V.++ pdatas2)
 
 -- O(1).
 lengthdPR (PNesteds pdatas)
  = V.length pdatas

 -- O(1).
 indexdPR  (PNesteds pdatas) ix
  = pdatas V.! ix

 -- O(max (length result, length pdatass))
 concatdPR pdatass
  = PNesteds
        $ V.concat $ V.toList
        $ V.map (\(PNesteds pdatas) -> pdatas) pdatass

 packdPR (PNesteds pdatas) flags
  = PNesteds $ V.pack pdatas flags

