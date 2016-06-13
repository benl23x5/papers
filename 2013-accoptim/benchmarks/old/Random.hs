
module Random where

import System.IO.Unsafe
import System.Random.MWC
import Data.Array.Accelerate                    as A

sequence' :: [IO a] -> IO [a]
sequence' = foldr k (return [])
  where k m ms = do { x <- m; xs <- unsafeInterleaveIO ms; return (x:xs) }

randomArray
    :: (Shape sh, Elt e, Variate e)
    => GenIO
    -> sh
    -> IO (Array sh e)
randomArray gen sh
  = fromList sh `fmap` sequence' (repeat (uniform gen))

randomArrayR
    :: (Shape sh, Elt e, Variate e)
    => GenIO
    -> (e, e)
    -> sh
    -> IO (Array sh e)
randomArrayR gen range sh
  = fromList sh `fmap` sequence' (repeat (uniformR range gen))

