{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}

module Flow where
import System.IO
import Control.Concurrent

data Sources i m e 
   = Sources
   { iarity :: i
   , pull   :: i -> (e -> m ()) -> m () -> m () }

data Sinks   i m e 
   = Sinks   
   { oarity :: i
   , push   :: i -> e -> m ()
   , eject  :: i -> m () }


-------------------------------------------------------------------------------
-- | Create sources from some named files.
sourceFs :: [FilePath] -> IO (Sources Int IO Char)
sourceFs names = do
 hs <- mapM (\n -> openFile n ReadMode) names
 let pulls i ieat ieject
      = do let h = hs !! i
           eof <- hIsEOF h
           if eof then hClose   h >> ieject
                  else hGetChar h >>= ieat
 return (Sources (length names) pulls)


-- | Create sinks to some named files.
sinkFs  :: [FilePath] -> IO (Sinks Int IO Char)
sinkFs names = do
 hs <- mapM (\n -> openFile n WriteMode) names
 let pushs  i e = hPutChar (hs !! i) e
 let ejects i   = hClose   (hs !! i)
 return (Sinks (length names) pushs ejects)


-------------------------------------------------------------------------------
-- | Drain elements from a source to a sink.
drainP :: Sources Int IO a -> Sinks Int IO a -> IO ()
drainP (Sources i1 ipull) (Sinks i2 opush oeject) = do
 let drainStream i
      = ipull i eats ejects 
      where eats   v = opush  i v >> drainStream i
            ejects   = oeject i

 let makeDrainer i = do
        mv <- newEmptyMVar 
        forkFinally (drainStream i) 
                    (\_ -> putMVar mv ())
        return mv 

 mvs <- mapM makeDrainer [0 .. min i1 i2]
 mapM_ readMVar mvs


-------------------------------------------------------------------------------
dup_ooo :: (Ord i, Monad m)
        => Sinks i m a -> Sinks i m a -> Sinks i m a
dup_ooo (Sinks n1 push1 eject1) 
       (Sinks n2 push2 eject2)
 = let pushs  i x = push1 i x >> push2 i x
       ejects i   = eject1 i  >> eject2 i
   in  Sinks (min n1 n2) pushs ejects


dup_ioi :: (Ord i, Monad m)
        => Sources i m a -> Sinks i m a -> Sources i m a
dup_ioi (Sources n1 pull1) 
       (Sinks   n2 push2 eject2)
 = let pull3 i eat3 eject3
        = pull1 i eat1 eject1
        where eat1   x = eat3 x >> push2  i x
              eject1   = eject3 >> eject2 i
   in  Sources (min n1 n2) pull3


-------------------------------------------------------------------------------
-- | Like `map_i`, but the worker function is also given the stream index.
map_i   :: (a -> b) -> Sources i m a -> Sources i m b
map_i f (Sources n pullsA)
 = Sources n pullsB
 where  pullsB i eatB ejectB
         = pullsA i eatA ejectA
         where  eatA v = eatB (f v)
                ejectA = ejectB


-- | Like `map_o`, but the worker function is also given the stream index.
map_o  :: (a -> b) -> Sinks i m b -> Sinks i m a
map_o f (Sinks n pushB ejectB)
 = Sinks n pushA_map ejectA_map
 where  pushA_map  i a  = pushB  i (f a)
        ejectA_map i    = ejectB i

≡

-------------------------------------------------------------------------------
folds_iii
        :: (Ord i, Monad m)
        => (b -> a -> b) -> b
        -> Sources i m Int 
        -> Sources i m a 
        -> Sources i m b

folds_iii f z (Sources nL pullLen) (Sources nX pullX)
 = Sources (min nL nX) pull_folds
 where  
        pull_folds i eat eject
         = pullLen i eat_len eject_len
         where 
               eat_len len = loop_folds len z
               eject_len   = eject
                   
               loop_folds !c !acc
                | c == 0    = eat acc
                | otherwise = pullX i eat_x eject_x
                where eat_x x = loop_folds (c - 1) (f acc x)
                      eject_x = eject


-------------------------------------------------------------------------------
copyMultipleP 
 :: [FilePath] -> [FilePath] -> [FilePath] -> IO ()
copyMultipleP srcs dsts1 dsts2
 = do  ss  <- sourceFs srcs
       sk1 <- sinkFs   dsts1
       sk2 <- sinkFs   dsts2 
       drainP ss (dup_ooo sk1 sk2)


copySetP :: [FilePath] -> [FilePath] -> IO ()
copySetP srcs dsts
 = do  ss <- sourceFs srcs
       sk <- sinkFs  dsts
       drainP ss sk

-----------------------------------------------------------


example1 = do
 ss <- sourceFs ["i-file1", "i-file2", "i-file3", "i-file4"]
 sk <- sinkFs   ["o-file1", "o-file2", "o-file3", "o-file4"]
 drainP ss sk


example2 
 = copyMultipleP
        ["i-file1",  "i-file2",  "i-file3",  "i-file4"]
        ["o-file1a", "o-file2a", "o-file3a", "o-file4a"]
        ["o-file1b", "o-file2b", "o-file3b", "o-file4b"]




