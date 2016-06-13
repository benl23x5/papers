
module ReduceThread_parMods (
	reduceThread_parMods
)

where

import	Array

import	Type
import	Heap
import	ReduceThread


-- ------------------------------------------------------------------------------------------------------------------------------
reduceThread_parMods
	:: Prog -> (Heap Cell, Eix, Env, Stack) -> Int
		-> (Heap Cell, Eix, Env, Stack, [ReduceMod], String)

reduceThread_parMods prog (heap, eix, env, stack)	threadNum	=
 let
	exp	= pcExp (heapGet prog eix)
 in
	rt	(heap,	exp,			env,			stack)
  where
	blankCell	= Cell { cMark = False, cEix = 0,	cEnv = [], cBlocks = [] }
	blockedCell	= Cell { cMark = False, cEix = -1, 	cEnv = [], cBlocks = [] }


-- var1, var2

	rt	(heap,	(Var	v),		env,			stack)
	 = let
		p	= env !! v
		oCell	= heapGet heap p
	  in
	   case	oCell of 

	    Cell { cEix = -1,	cBlocks	= blocks } 
	     ->	(heap,	-1,			[],			stack,		mods,	"var2")		
	      where
		mods	= [BlockOn p threadNum]

	    Cell { cEix = eix,	cEnv	= nEnv	}
	     -> (heap, eix,			nEnv,	(SEUpdate p)  : stack,		mods,	"var1")
	      where
		mods	= [Update p blockedCell]

-- var 3

	rt	(heap,	(Lambda e),		env,	(SEUpdate p)	: stack)
	 =	(heap,	eix,			env,			  stack, 	mods,	"var3")
	 where
		mods	= [Unblock   p eix env ]


	rt	(heap,	(Construct name a),	env,	(SEUpdate p)	: stack)
	 =	(heap,	eix,			env,			  stack, 	mods,	"var3")
	 where
		mods	= [Unblock   p eix (take a env)]

-- app3
	rt	(heap,	(ExpVar e v),		env,			stack)
	 = let
		p	= env !! v
		oCell	= heapGet heap p
	  in
	   case oCell of 
	    
	    Cell { cEix = -1,	cBlocks = blocks }
	     -> (heap,	e,			env,	(SEPointer p) : stack,		[],	"app3")

	    _
	     -> (heap, eix,			env,			  stack,	[],	[])

-- default
	rt	(heap,	exp,			env,			  stack)
	 =	(heap,  eix,			env,			  stack,	[],	[])

