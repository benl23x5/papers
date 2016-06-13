

import Data.IORef

fun 
 = do	r	<- newIORef (\x -> x * 2)
	f	<- readIORef r
	putStr	$ show (f 5) ++ "\n"

