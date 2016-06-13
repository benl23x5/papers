
module RefList where

data IORef a
	= IORef a
	
data RefList a 
 	= Nil
	| Cons a (IORef (RefList a))
	
refMap :: (a -> IO b) -> RefList a -> IO (RefList b)
refMap f list
 = case list of
	Nil		-> return Nil
	Cons x xs	
	 -> do	x'	<- f x
		xs'	<- refMap f list
		return	$ Cons x' (IORef xs')