
module ReduceThread (
	ReduceMod(..),

	getRule,
	applyMod,

	reduceModIsUpdate,
	reduceModIsBlockOn,
	reduceModIsUnblock,
	reduceModIsSpark,

	trim,
	primFunc,
	rtPrint
)

where

import Type
import Heap
import Array
import Char
import Util
import Schedule

---------------------------------------------------------------------------------------------------------------------------------
getRule (heap, eix, env, stack, mods, rule)	= rule

------------------------------------------------------------------------------------------------------------------------- applyMod

applyMod :: 	State ->	ReduceMod 	-> 	State
applyMod	state		mod 		=
 case mod of
	Error eix str		-> 
		state { sRun		= False,	
			sError	= Just (eix, str) }

	Stop  			-> 
		state { sRun		= False }


	Print str		-> 
		state { sIOBite	= str,
			sIOAccum	= (sIOAccum state) ++ str }
	
	Update  p nCell		->
		state { sHeap	= heapUpdate (sHeap state) (p, nCell) }

	Remove  p		->
		state { sHeap	= fst $ heapRemove (sHeap state) p }


	BlockOn	p threadNum	-> 
	 let
		oCell 	= heapGet (sHeap state) p
		nCell	= oCell { cBlocks = threadNum : (cBlocks oCell) }
	 in
		state { sHeap	= heapUpdate (sHeap state) (p, nCell) }

	Unblock p eix env	->
	 let
		blockedThreads	= cBlocks (heapGet (sHeap state) p)

--		state1		= chain (unblockThread eix env) state blockedThreads

		nCell		= Cell { cMark = False, cEix = eix, cEnv = env, cBlocks = [] }
	 in
		state { sHeap		= heapUpdate (sHeap state) (p, nCell),
			sUnblockQueue	= (sUnblockSchedFunc state) 
						(zip blockedThreads (repeat (eix, env))) 
						(sUnblockQueue state) 
		}


	Spark 	 p eix env	-> 
	 let
		nCell		= Cell { cMark = False, cEix = -1, cEnv = [], cBlocks = [] }

--		threadNum	= firstInactiveThread (sThread state)
--		nThread		= Thread { tControl = eix, tEnv	 = env, tStack = [SEUpdate p], tRule = "spawn" }
	 in
		state {	sHeap		= heapUpdate (sHeap state) (p, nCell),
			sSpawnQueue	= (sSpawnSchedFunc state)
						[(eix, env, p)]
						(sSpawnQueue state)
		}


--			sThread		= (sThread state) // [(threadNum, nThread)] }








--------------------------------------------------------------------------------------------------------------------------------
reduceModIsUpdate	(Update p nCell)		= True
reduceModIsUpdate	_				= False

reduceModIsBlockOn	(BlockOn p threadNum)		= True
reduceModIsBlockOn	_				= False

reduceModIsUnblock	(Unblock p eix env)		= True
reduceModIsUnblock	_				= False

reduceModIsSpark	(Spark p eix env)		= True
reduceModIsSpark	_				= False





----------------------------------------------------------------------------------------------------------------------------- trim
trim	env	[]	= []
trim	env	(t:ts)	= env !! t	: trim env ts


------------------------------------------------------------------------------------------------------------------------- primFunc
primFunc :: 	String	-> 	Env	-> (Eix, Env)

primFunc "intAdd" (0 :n2 :0 :n1 :env)	= (0, 0 :(n1 + n2)	:env)
primFunc "intSub" (0 :n2 :0 :n1 :env)	= (0, 0 :(n1 - n2)	:env)
primFunc "intMul" (0 :n2 :0 :n1 :env)	= (0, 0	:(n1 * n2)	:env)
primFunc "intDiv" (0 :n2 :0 :n1 :env)	= (0, 0 :(div n1 n2)	:env)
primFunc "intMod" (0 :n2 :0 :n1 :env)	= (0, 0 :(mod n1 n2)	:env)

primFunc "intEq"  		(0 :n2 :0 :n1 :env)	
	| n1 == n2			= (3, env)
	| otherwise			= (4, env)

primFunc "intMoreThan"		(0 :n2 :0 :n1 :env)
	| n1 > n2			= (3, env)
	| otherwise			= (4, env)	

primFunc "intMoreThanEq" 	(0 :n2 :0 :n1 :env)
	| n1 >= n2			= (3, env)
	| otherwise			= (4, env)

primFunc "intLessThan"		(0 :n2 :0 :n1 :env)
	| n1 < n2			= (3, env)
	| otherwise			= (4, env)

primFunc "intLessThanEq" 	(0 :n2 :0 :n1 :env)
	| n1 <= n2			= (3, env)
	| otherwise			= (4, env)


rtPrint	(0 :n :env)	= Just 		(show n, env)
rtPrint _		= Nothing




