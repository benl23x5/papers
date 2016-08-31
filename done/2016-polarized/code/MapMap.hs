
-------------------------------------------------------------------------------
-- These are definitions from the paper:

data Sources i m e
   = Sources 
   { arity :: i
   , pull  :: i -> (e -> m ()) -> m () -> m () }


data Sinks   i m e
   = Sinks
   { arity :: i
   , push  :: i -> e -> m ()
   , eject :: i -> m () }


-- | Apply a function to every element in a bundle of sources.
map_i :: (a -> b) -> Source i m a -> Sources i m b
map_i f (Source n pullsA)
 = Source n pullsB
 where  pullsB i eatB ejectB
         = pullsA i eatA ejectA
         where  eatA v = eatB (f v)
                ejectA = ejectB


-------------------------------------------------------------------------------
-- Rewrite definition using lambda abstractions instead of intermediate
-- bindings, and also desugar the function parameters.

map_i :: (a -> b) -> Source i m a -> Sources i m b
map_i 
 = λf (Sources n pullsA)
   .   Sources n (λi eatB ejectB
                  . pullsA i (λv. eatB (f v)) ejectB)


-------------------------------------------------------------------------------

-- An expression that applies two map operators in turn to the same bundle
-- of streams.
 
   map_i f (map_i g ss)


-- Value 'ss' is a Sources, so we'll name the arity 'n' and pull function 'pulls'.

   map_i f (map_i g (Sources n pulls))


-- Expand the inner use of of map_i
--   We freshen the variables from the definition, 
--   renaming them n1, pullA1 etc

=  map_i f ((λf1 (Sources n1 pullsA1)
             .    Sources n1 (λi eatB1 ejectB1
                              .  pullsA1 i1 (λv1. eatB1 (f1 v1)) ejectB1))
             g  (Sources  n pulls))


-- Substitute for 'g' and (Sources n pulls)

=  map_i f (Sources n (λi1 eatB1 ejectB1
                       . pulls i1 (λv1. eatB1 (g v1)) ejectB1))


-- Expand the remaining use of map_i
--   We freshen the variables from the definition,
--   renaming them n2, pullA2 etc

=  (λf2 (Sources n2 pullsA2)
    .    Sources n2 (λi2 eatB2 ejectB2
                   . pullsA2 i2 (λv2. eatB2 (f2 v2)) ejectB2)) 
    f   (Sources n  (λi1 eatB1 ejectB1
                   . pulls   i1 (λv1. eatB1 (g v1))  ejectB1))


-- Substitute for 'f' and '(Sources n1 (λi1 ...))'

=  Sources n (λi2 eatB2 ejectB2
             . (λi1 eatB1 ejectB1
                . pulls i1 (λv1. eatB1 (g v1)) ejectB1)
               i2 (λv2. eatB2 (f v2)) ejectB2)


-- Substitute for 'i2', 'eatB2' and 'ejectB2'

=  Sources n (λi2 eatB2 ejectB2
             . pulls i2 (λv1. (λv2. eatB2 (f v2)) (g v1)) ejectB2)


-- Substitute for 'v2'

=  Sources n (λi2 eatB2 ejectB2
             . pulls i2 (λv1. eatB2 (f (g v1))) ejectB2)


-------------------------------------------------------------------------------
-- Here the definition of 'sourceFs' from the paper. 

sourceFs :: [FilePath] -> IO (Sources Int IO Char)
sourceFs names
 = do hs <- mapM (\n -> openFile n ReadMode) names
      let pulls i ieat ieject
          = do let h = hs !! i
               eof <- hIsEOF h
               if eof then hClose    h >>  ieject
                      else hGetChar  h >>= ieat
      return (Sources (length names) pulls)


-- We'll reduce the following:

  sourceFs files >>= λss. return (map_i f (map_i g ss))


-- Splitting 'ss' to name the arity 'n' and pull function 'pulls' as before.

= sourceFs files 
  >>= λ(Sources n pulls)
      . return (map_i f (map_i g (Sources n pulls)))


-- Substitute in the expression we derived for map_i / map_i.

= sourceFs files 
  >>= λ(Sources n pulls)
      . return (Sources n (λi2 eatB2 ejectB2
                           . pulls i2 (λv1. eatB2 (f (g v1))) ejectB2))


-- Expand the definition of 'sourceFs'

= (do hs <- mapM (\n -> openFile n ReadMode) files
      let pulls i ieat ieject
          = do let h = hs !! i
               eof <- hIsEOF h
               if eof then hClose    h >>  ieject
                      else hGetChar  h >>= ieat
      return (Sources (length files) pulls))
  >>= (λ(Sources n pulls)
       . return (Sources n (λi2 eatB2 ejectB2
                            . pulls i2 (λv1. eatB2 (f (g v1))) ejectB2)))


-- Inline the local binding of 'pulls'.

= (do hs <- mapM (\n -> openFile n ReadMode) files
      return (Sources (length files) 
                      (λi ieat ieject
                       .  do let h = hs !! i
                             eof <- hIsEOF h pulls
                             if eof then hClose    h >>  ieject
                                    else hGetChar  h >>= ieat)))
  >>= (λ(Sources n pulls)
       . return (Sources n (λi2 eatB2 ejectB2
                            . pulls i2 (λv1. eatB2 (f (g v1))) ejectB2)))


-- Reduce the application of (>>=)

= (do hs <- mapM (\n -> openFile n ReadMode) files
      return (Sources 
                (length files)
                (λi2 eatB2 ejectB2
                 . (λi ieat ieject
                    .  do let h = hs !! i
                          eof <- hIsEOF h pulls
                          if eof then hClose    h >>  ieject
                                 else hGetChar  h >>= ieat) 
                   i2 
                   (λv1. eatB2 (f (g v1))) 
                   ejectB2)))


-- Substitute for i, ieat and ieject

= (do hs <- mapM (\n -> openFile n ReadMode) files
      return (Sources 
                (length files)
                (λi2 eatB2 ejectB2
                 . do let h = hs !! i2
                      eof <- hIsEOF h pulls
                      if eof then hClose    h >>  ejectB2
                             else hGetChar  h >>= (λv1. eatB2 (f (g v1))))))
                    

-- Here we see that the source will get a character, then pass it immediately
-- to a function that applies 'g', then 'f', then the callers 'eatB2' function.
-- There is no overhead due to the definition of the Sources type, and no need
-- to construct and destruct values of Either type.

