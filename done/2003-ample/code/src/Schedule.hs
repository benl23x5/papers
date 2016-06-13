
module Schedule (
	threadIsRunnable,
	threadIsBlocked,
	threadIsInactive,

	nextRunnableThread,
	firstInactiveThread,

	scheduleImmediate,
	scheduleSequential
)

where

import	Array

import	Type
import	Util
import	List

import	Debug.Trace

--------------------------------------------------------------------------------------------------------------------------- thread
threadIsRunnable :: 	Thread		-> Bool
threadIsRunnable 	thread	
	| (tControl thread) >= 0	= True
	| otherwise			= False

threadIsBlocked	 :: 	Thread		-> Bool
threadIsBlocked 	thread
	| (tControl thread) == -1	= True
	| otherwise			= False

threadIsInactive ::	Thread		-> Bool
threadIsInactive	thread
	| (tControl thread) == -2	= True
	| otherwise			= False

nextRunnableThread ::	Array Int Thread  -> Int	-> Maybe Int
nextRunnableThread	thread		 curThread	
	= findAIxFromWrap threadIsRunnable thread (curThread + 1)

firstInactiveThread ::	Array Int Thread 		-> Int
firstInactiveThread	thread 
 = case findAIx threadIsInactive thread of
	Nothing		-> error "firstInactiveThread: out of threads"
	Just t		-> t


----------------------------------------------------------
scheduleImmediate :: 	Int ->	[a] ->	[(Int, a)] -> 	[(Int, a)]
scheduleImmediate	delay	items	scheds		=

	scheds	++ (zip (repeat delay) items)


scheduleSequential ::	Int ->	[a] -> 	[(Int, a)] ->	[(Int, a)]
scheduleSequential	delay	items	scheds		=
 let
	startDelay	= 
		if length scheds == 0 
			then delay
			else (fst $ last scheds) + delay

	itemDelay	= [startDelay, startDelay + delay ..]
 in
	scheds	++ (zip itemDelay items)


