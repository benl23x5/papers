
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



sourceFs :: [FilePath] -> IO (Sources Int IO Char)
sourceFs names = do
 hs <- mapM (\n -> openFile n ReadMode) names
 let pulls i ieat ieject
      = do let h = hs !! i
           eof <- hIsEOF h
           if eof then hClose   h >> ieject
                  else hGetChar h >>= ieat
 return $ Sources (length names) pulls


sinkFs  :: [FilePath] -> IO (Sinks Int IO Char)
sinkFs names = do
 hs <- mapM (\n -> openFile n WriteMode) names
 let pushs  i e = hPutChar (hs !! i) e
 let ejects i   = hClose   (hs !! i)
 return $ Sinks (length names) pushs ejects


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



