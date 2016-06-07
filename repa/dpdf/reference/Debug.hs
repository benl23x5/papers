{-# LANGUAGE     BangPatterns, NoMonomorphismRestriction #-}
module Debug where
import qualified Generic as G
import Control.Concurrent
import Control.Exception (bracket_)


-- I will delete this when I am sure the folds and things are correct
debug_loud_sink :: (Show i, Show a) => G.Sinks i IO a -> IO (G.Sinks i IO a)
debug_loud_sink (G.Sinks arity1 push1 eject1)
 = do lock <- newQSem 1
      let printL str
           = bracket_ (waitQSem lock) (signalQSem lock)
           $ putStrLn str

      let push' i e
            = do printL ("Push start: sink=" ++ show i ++ " value=" ++ show e)
                 push1 i e
                 printL ("Push end:   sink=" ++ show i)

      let eject' i
            = do printL ("Eject:      sink=" ++ show i)
                 eject1 i

      return (G.Sinks arity1 push' eject')

