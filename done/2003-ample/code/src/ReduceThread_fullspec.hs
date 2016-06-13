
module ReduceThread_fullspec (
	reduceThread
)

where

import	Array

import	Type
import	Heap
import	ReduceThread
import	qualified ReduceThread_parMods
import	qualified ReduceThread_single

-------------------------------------------------------------------------------------------------------------------------------
reduceThread :: State ->	Int 		-> (State, ([ReduceMod], String))
reduceThread	state		threadNum	= 
 let
	thread	= (sThread state) ! threadNum
	
	(nHeap, nControl, nEnv, nStack, mods, nRule)
		= reduceThread' (sProg state)
				((sHeap state), (tControl thread), (tEnv thread), (tStack thread))
				threadNum

	nThread	= thread { 
			tControl 	= nControl,	
			tEnv 		= nEnv,	
			tStack 		= nStack } 

	nState	= state {
			sHeap		= nHeap,
			sThread		= (sThread state) // [(threadNum, nThread)],
			sReductions	= (sReductions state) + 1 }
 in
	(nState, (mods, nRule))


reduceThread' 
	:: Prog -> (Heap Cell, Eix, Env, Stack) -> Int
		-> (Heap Cell, Eix, Env, Stack, [ReduceMod], String)

reduceThread' prog snacks threadNum	=
 let
	(heap, eix, env, stack)	= snacks

	result_parMods		= ReduceThread_parMods.reduceThread_parMods 	prog snacks threadNum
	result_fullspecMods	= reduceThread_fullspecMods			prog snacks threadNum
	result_single		= ReduceThread_single.reduceThread_single	prog snacks threadNum
 in
	if 	getRule result_parMods 		/= [] then result_parMods
	else if getRule result_fullspecMods	/= [] then result_fullspecMods
	else if getRule result_single		/= [] then result_single
	else
		(heap, -2, env, stack, [], "stop")


-- ------------------------------------------------------------------------------------------------------------------------------
reduceThread_fullspecMods
	:: Prog -> (Heap Cell, Eix, Env, Stack) -> Int
		-> (Heap Cell, Eix, Env, Stack, [ReduceMod], String)
	
reduceThread_fullspecMods prog (heap, eix, env, stack)	threadNum	=
 let
	exp	= pcExp (heapGet prog eix)
 in
	rt	(heap,	exp,			env,			stack)
  where
	blankCell	= Cell { cMark = False, cEix = 0,	cEnv = [], cBlocks = [] }
	blockedCell	= Cell { cMark = False, cEix = -1, 	cEnv = [], cBlocks = [] }

-- app1

	rt	(heap,	(ExpVar e v),		env,			stack)
	 = let
		p	= env !! v
		oCell	= heapGet heap p
	  in
	   case oCell of 
	    
	    Cell { cEix	= e2,	cEnv	= env2 }
	     -> (heap,	e,			env,	(SEPointer p) : stack,		mods,	"app1")
	      where
		mods	= [Spark p e2 env2 ]

-- default
	rt	(heap,	exp,			env,			  stack)
	 =	(heap,  eix,			env,			  stack,	[],	[])






