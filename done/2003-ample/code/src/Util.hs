

module	Util (
	chomp,
	mJust,
	must,
	indexOf,
	dropElemAtIx,
	glob,
	toTuple2,
	splitWhen,
	takeRange,
	replace, replaces,

	mapA,
	mapAL,
	mapASubset,
	findAIx,
	findAIxFrom,
	findAIxFromWrap,

	chain, chainR, chainF

)
where

import Array


mJust ::	Maybe a 	-> a
mJust		(Just a)	= a
mJust 		_		= error "mJust failed";	

must ::		Maybe a 	-> a
must		(Just a)	= a
must		_		= error "must failed";


indexOf :: Eq a => [a] -> a	-> Maybe Int
indexOf		(x:xs)	m 	= indexOf' (x:xs) m  0

indexOf'	[]	m n	= Nothing
indexOf'	(x:xs)	m n
	| x == m		= Just n
	| otherwise		= indexOf' xs m (n+1)


dropElemAtIx ::		Int -> 	[a]		-> [a]
dropElemAtIx		ix	xx		=
 let
	(front, back)	= splitAt ix xx
 in
	front ++ tail back

glob ::		Int ->		[a]	-> [[a]]
glob		runLength	[]	= []
glob		runLength	xx	= globbed : glob runLength rest
 where
 	(globbed, rest)		= splitAt runLength xx

toTuple2 ::	[a] 		->	(a, a)
toTuple2	(x1 : x2 : [])	=	(x1, x2)


splitWhen ::	(a -> Bool) ->	[a] 	-> ([a], [a])
splitWhen 	f		xx	= splitWhen' f xx []

splitWhen' 	f	(x:xs) 	acc 
	| f x				= (acc, x:xs)
	| otherwise			= splitWhen' f xs (acc ++ [x])

takeRange :: 	(Int, Int) -> 	[a] -> [a]
takeRange	(first, last)	xx	= take (last - first) $ snd $ splitAt first xx


replace	:: Eq a => a -> a -> a -> a 
replace		old	new	x	= if x == old then new else x

replaces:: Eq a => a -> a -> [a] -> [a]
replaces 	old 	new 	xs	= map (replace old new) xs


chomp	str			= chomp' str []

chomp'	[]	 	acc	= [acc]
chomp'  (' ':' ':xs)	acc	= 	chomp' (' ':xs) acc
chomp'	(' ':xs)	acc	= acc : chomp' xs 	[]
chomp'  (x:xs)		acc	= 	chomp' xs 	(acc ++ [x])



------------------------------------------------------------------------------------------------------------------------------- mapA
-- mapA	f array	= newarray
--
--	Apply a function to all elements of an array, returning the new array.
--
--
mapA ::		(a -> b) -> 	Array Int a	-> Array Int b
mapA		f		a		=
 let
	(low, high)	= bounds a
 in
	array (bounds a) [ (i, f (a ! i)) 	| i <- [low .. high] ]


mapAL :: 	(a -> b) ->	Array Int a	-> [b]
mapAL		f		a		=
 let
	(low, high)	= bounds a
 in
	[ f (a ! i) | i <- [low .. high]]



------------------------------------------------------------------------------------------------------------------------- mapASubset
-- mapASubset f [indicies] array	= newarray
--
--	Apply a function to a subset of an array, returning the new array.
--	The new array is the same size as the old one.
--	Elements not listed in the subset retain their original values.
--
--
mapASubset ::	(a -> a) -> 	[Int]		-> Array Int a	-> Array Int a
mapASubset	f		[]		a		= a
mapASubset	f		(x:xs)		a		= mapASubset f xs	(a // [(x, f (a ! x))]	)



---------------------------------------------------------------------------------------------------------------------------- findAIx
-- findAIx f array = mindex
--
--	Look for elements in an array which make f True, maybe return the 
--	index of the first maching element
--
--
findAIx	::		(a -> Bool) ->	Array Int a ->	Maybe Int
findAIx			f		a		
	= findAIxFrom f a low
	where
		(low, high)	= bounds a



------------------------------------------------------------------------------------------------------------------------ findAIxFrom
-- findAIxFrom f array start = mindex
--
--	Start from index given by start, look for elements in array which make f
--	True, maybe return the index of the first matching element.
--
--
findAIxFrom ::		(a -> Bool) ->	Array Int a ->	Int	-> Maybe Int
findAIxFrom		f		a		i
	| f (a!i) == True	= Just i
	| i < high		= findAIxFrom	f a (i+1)	
	| otherwise		= Nothing
	where
		(low, high)	= bounds a



---------------------------------------------------------------------------------------------------------------------- findAFromWrap
-- findAFromWrap f array start = mindex
--
--	Like findAIxFrom, but will wrap around the end of the array while looking
--	for a match. Elements are not checked more than once
--
--
findAIxFromWrap ::	(a -> Bool) ->	Array Int a ->	Int ->	Maybe Int
findAIxFromWrap		f		a		i
	= findAIxFromWrap' f		a		i	False


findAIxFromWrap'		f		a		i	around	
	| i > high		= findAIxFromWrap' f a low	True
	| f (a!i) == True 	= Just i
	| i < high		= findAIxFromWrap' f a (i + 1) 	around
	| around == False	= findAIxFromWrap' f a low	True
	| otherwise		= Nothing	
	where
		(low, high)	= bounds a



chain  :: (s -> a -> s)	->
		s ->	[a]	-> s

chain	f	state		[]	= state
chain	f	state		(x:xs)	= chain f (f state x) xs


chainR :: (s -> a -> (s, b)) -> 
		s ->	[a]	-> (s, [b])

chainR	f	state		xx	= chainR' f (state, []) xx

chainR' 	f	(state,	rs)	xx
	= case xx of
		[]			-> (state, rs)
		(x:xs)			-> chainR' f (nState, rs ++ [r]) xs
		 where
			(nState, r)	= f state x



chainF ::	[(s -> s)] -> s -> s

chainF	 	[]		s	= s
chainF		(f:fs)		s	= chainF fs (f s)









