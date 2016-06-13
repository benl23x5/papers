
module ReduceThread_single (
	reduceThread,
	reduceThread_single
)

where

import Array

import Type
import Heap
import Char
import Util
import ReduceThread

-- ------------------------------------------------------------------------------------------------------------------------------
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

	result_single		= reduceThread_single prog snacks threadNum
 in
	if	getRule result_single		/= [] then result_single
	else
		(heap, -2, env, stack, [], "stop")


-- ------------------------------------------------------------------------------------------------------------------------------
reduceThread_single
	:: Prog	-> (Heap Cell, Eix, Env, Stack) -> Int
		-> (Heap Cell, Eix, Env, Stack, [ReduceMod], String)

reduceThread_single  prog (heap, eix, env, stack)	threadNum 	=
 let 
	exp	= pcExp (heapGet prog eix)
 in
 	rt 	(heap,	exp, 			env,			stack)

  where
	blankCell	= Cell { cMark = False, cEix = 0,	cEnv = [], cBlocks = [] }
	blockedCell	= Cell { cMark = False, cEix = -1, 	cEnv = [], cBlocks = [] }


-- var1

	rt	(heap,	(Var	x),		env,			stack)
	 = 	(heap,	nEix,			nEnv,   (SEUpdate p)  : stack,	        [],	"var1")
	 where
		p			= env !! x

		Cell { cEix = nEix, 
		       cEnv = nEnv }	= heapGet heap p

-- var2

	rt	(heap,	(Lambda e),		env,    (SEUpdate p)  : stack)
	 = 	(heap,	eix,			env,			stack,	      mods,	"var2")
	 where
		nCell		= blankCell { cEix = eix, cEnv = env }
		mods		= [Update p nCell]

-- var3

	rt	(heap,	(Construct name a),	env,	(SEUpdate p)  : stack)
	 = 	(heap, eix,			env,			stack,	      mods,	"var3")	
	 where
		nCell		= blankCell { cEix = eix, cEnv = take a env }
		mods		= [Update p nCell]


-- app1

	rt 	(heap, 	(ExpVar e x), 		env,			stack)
	 =	(heap,	e,			env,	(SEPointer p) : stack,		[],	"app1")
	 where
		p		= env !! x


-- app2

	rt 	(heap,	(Lambda e),		env,	(SEPointer p) : stack)
	 = 	(heap,	e,		    p : env,			stack,		[],	"app2")


-- let

	rt	(heap,	(Let  binds (e0, t0)),	env,			stack)
	 = 	(nHeap,	e0,			nEnv,			stack,	      mods,	"let")
	 where
		(nHeap, ps)	= chainR heapAdd heap (take (length binds) (repeat blankCell))

		nEnv0		= reverse ps ++ env
		nEnv		= trim nEnv0 t0

		trimToCell (e, t) = blankCell { cEix = e, cEnv = trim nEnv0 t }

		cells		= map trimToCell binds
		pCells		= zip ps cells

		mods		= map (\x -> Update (fst x) (snd x)) pCells


-- case1

	rt	(heap,	(Case (e, t) alts),	env,			stack)
	 = 	(heap,	e,			env, (SEAlts (alts, trim env t) : stack),[],	"case1")


-- case2

	rt	(heap,	(Construct name a),	env, (SEAlts (alts, aEnv) : stack) )
	 =  case lookup (name, a) alts of

	  Just (nEix, trimmer)	-> 
	   (	heap,  nEix,			trim nEnv trimmer,	stack,		[],	"case2/match")
	    where
		nEnv		= (take a env) ++ aEnv

	  Nothing		-> 
	   case lookup ("Default", 0) alts of 

	    Just (nEix, trimmer) -> 
	     (  heap,  nEix,			trim nEnv trimmer,	stack,		[],	"case2/default")
	      where
		nEnv		= aEnv	

	    Nothing	-> 
	     (	heap,	eix,		 	env, (SEAlts (alts, aEnv) : stack),	mods,	"case2/fail_nomatch")
	      where
		mods		= [errorMod]
		errorEix	= fst (snd (head alts))
		
		altNames	= map (fst.fst) alts
		altAirities	= map (snd.fst) alts

		errorString	= "no match for (\"" ++ name ++ "\"," ++ show a ++ "). "
				  ++ "Alternatives were " ++ show (zip altNames altAirities)

		errorMod	= Error errorEix errorString


-- seq
-- these must come before the 'constant' rules, because their patterns overlap.

	rt	(heap,	(Seq e1 e2),		env,			stack)
	 = 	(heap,	e1,			env,	(SESeq (e2, env) : stack),	[],	"push seq")

	rt	(heap,  (Lambda e),		env,	(SESeq (e2, sEnv) : stack))
	 = 	(heap,	e2,			sEnv,			stack,		[],	"eval seq/Lambda")

	rt	(heap,	(Construct name a),	env,	(SESeq (e2, sEnv) : stack))
	 = 	(heap,	e2,			sEnv,			stack,		[],	"eval seq/Construct")

	rt	(heap,	(Constant c),		env,	(SESeq (e2, sEnv) : stack))
	 = 	(heap,	e2,			sEnv,			stack,		[],	"eval seq/Constant")


-- constant

	rt	(heap, 	Constant (CInt n),	env,			stack)
	 = 	(heap,	0,		 0 : n: env,			stack,		[],	"const int")

	
	rt	(heap, 	Constant (CChar c),	env,			stack)
	 = 	(heap,	1,		 1 : ord c: env,		stack,		[],	"const char")


-- print

	rt	(heap,	PrimFunc "print",	(0 :n :env),		stack)
	 = 	(heap,	5,			env,			stack,		mods,	"print int")
	  where
		mods	= [Print (show n)]

	rt	(heap,  PrimFunc "print",	(1 :n :env),		stack)
	 = 	(heap,  5,			env,			stack,		mods,	"print char")
	  where
		mods	= [Print [chr n]]


-- primFunc

	rt	(heap,	(PrimFunc name),	env,			stack)
	 = 	(heap,	eix,			nEnv,			stack,		[],	("primFunc:" ++ name))
	 where
		(eix, nEnv)	= primFunc name env

-- default

	rt	(heap,	exp,			env,			stack)
	 = 	(heap,	eix,			env,			stack,		[],	[])


