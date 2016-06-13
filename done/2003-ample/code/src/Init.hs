
module Init (
	emptyProg,
	emptyHeap,

	threadStart,
	threadInactive,
	threadBlocked,
	threadRunnable,
	
	cellInactive,
	cellBlocked
)
where

import	Type
import	Heap
import	Array

emptyProg maxProgCells 	= nProg	
 where
	prog		= heapNew maxProgCells
	cell		= ProgCell { pcExp = Var 0, pcSourceLine = 0, pcDepth = 0 }

	(prog0, _)	= heapAdd prog  cell { pcExp = Construct "Int"		2 }
	(prog1, _)	= heapAdd prog0 cell { pcExp = Construct "Char"		2 }
	(prog2, _)	= heapAdd prog1 cell { pcExp = Construct "Float"	2 }
	(prog3, _)	= heapAdd prog2 cell { pcExp = Construct "True"		0 }
	(prog4, _)	= heapAdd prog3 cell { pcExp = Construct "False"	0 }
	(prog5, _)	= heapAdd prog4	cell { pcExp = Construct "Done"		0 } 
		
	nProg		= prog5


emptyHeap maxHeapCells	= nHeap
 where
	deadCell	= Cell { cMark = False, cEix = -1, cEnv = [], cBlocks	= [] }

	heap		= heapNew maxHeapCells

	(heap0,	_)	= heapAdd heap  deadCell
	(heap1,	_)	= heapAdd heap0 deadCell
	(heap2,	_)	= heapAdd heap1 deadCell
		
	nHeap		= heap2


threadRunnable control env stack
	= Thread {
		tControl	= control,
		tEnv		= env,
		tStack		= stack
	}


threadInactive
	 = Thread {
		tControl	= -2,
		tEnv		= [],
		tStack		= []
 	}

threadBlocked
	= Thread {
		tControl	= -1,
		tEnv		= [],
		tStack		= []
	}

threadStart eix
	= Thread {
		tControl	= eix,
		tEnv		= [],
		tStack		= []
	}


cellInactive
	= Cell {
		cMark		= False,
		cEix		= 0,
		cEnv		= [],
		cBlocks		= []
	}


cellBlocked
	= Cell {
		cMark		= False,
		cEix		= -1,
		cEnv		= [],
		cBlocks		= []
	}

