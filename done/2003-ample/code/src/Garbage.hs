module Garbage (
	clearAllMarks,
	clearMarks,	clearMark,
	markDeps,	markDep,
	sweepCells,	sweepCell,

	stateDeps,
	slurpStackDeps,
	garbageCollect	
)
where

import	Util
import	Type
import	Heap
import	Array


clearAllMarks	heap				= clearMarks heap [0..  (hCells heap) -1]


clearMarks ::	Heap Cell ->	[Int]		-> Heap Cell
clearMarks	heap		ds		= chain clearMark heap ds
clearMark	heap		d		=
 let
	cell	= heapGet heap d
	nCell	= cell { cMark = False }
	nHeap	= heapUpdate heap (d, nCell)
 in
	if (heapCellIsFree heap d) 
		then	heap
		else	nHeap


markDeps ::	Heap Cell ->	[Int]		-> Heap Cell
markDeps	heap		ds		= chain markDep heap ds
markDep 	heap		d		= 
 let
	cell	= heapGet heap d
	nCell	= cell { cMark = True }
	heap1	= heapUpdate heap (d, nCell)

	deps	= slurpEnvDeps (cEnv nCell)
		
	heap2	= chain markDep heap1 deps
 in
 	if (cMark cell) 
		then	heap
		else	heap2


sweepCells ::	Heap Cell			-> Heap Cell
sweepCells	heap				= chain sweepCell heap [0.. (hCells heap)-1] 
sweepCell	heap		ix 		=
 let
	cell		= heapGet heap ix
	nCell		= cell { cMark		= False }
	nHeapClear	= heapUpdate heap (ix, nCell)

	nhCell		= (hCell heap) // [(ix, Free (hFreeHead heap))]
	nHeapFree	= heap { hCell 		= nhCell,
				 hFreeHead	= ix,
				 hFreeCells	= (hFreeCells heap) + 1 }

 in
	if (heapCellIsFree heap ix) 
	 then	heap
	 else	
		if (cMark cell == True) || (cEix cell == (-1))
		 then	nHeapClear
		 else	nHeapFree


stateDeps ::	State		-> [Int]
stateDeps	state	= envDeps ++ stackDeps
 where
	env		= concat $ mapAL tEnv	(sThread state)
	envDeps		= slurpEnvDeps	env

	stack		= concat $ mapAL tStack (sThread state)
	stackDeps	= slurpStackDeps stack


slurpStackDeps ::	Stack			-> [Int]
slurpStackDeps		[]		= []
slurpStackDeps		(x:xs)		=
 case x of 
	SEAlts		(alts, env)	-> (slurpEnvDeps env) ++	slurpStackDeps xs
	SEPointer	p		-> p :				slurpStackDeps xs			 
	SEUpdate	p		-> p :				slurpStackDeps xs
	SESeq		(e1,   env)	-> (slurpEnvDeps env) ++	slurpStackDeps xs


slurpEnvDeps ::		Env			-> [Int]
slurpEnvDeps		[]			= []
slurpEnvDeps		(0:n:xs)		= 	slurpEnvDeps xs
slurpEnvDeps		(1:n:xs)		= 	slurpEnvDeps xs
slurpEnvDeps		(x:xs)			= x : 	slurpEnvDeps xs


garbageCollect ::	State			-> State
garbageCollect		state			= nState
 where
	deps		= stateDeps state

	heap0		= sHeap state
	heap1		= markDeps heap0 deps
	heap2		= sweepCells heap1
	
	nState		= state { sHeap = heap2 }


