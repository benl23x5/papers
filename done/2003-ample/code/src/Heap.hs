
------------------------------------------------------------------------------------------------------------------------------- Heap
module Heap (
	HeapCell(..),	Heap(..),
	heapNew,
	heapAdd,
	heapRemove,
	heapGet,
	heapUpdate,
	heapMod,

	heapShow,

	heapCellIsFree,
	heapCellIsActive,
)
where

import	Array
import	Util
import	Debug.Trace

import 	qualified Data.IntMap	as Map
import 	Data.IntMap		(IntMap)

data HeapCell a
	= Active 	a
	| Free		Int	
 deriving Show 

data Heap a = 
	Heap { 
		hCell		:: IntMap (HeapCell a),
		hCells		:: Int,

		hFreeCells	:: Int,
		hFreeHead	:: Int
	}
 deriving Show
		
-- | create a new, empty heap.
heapNew :: Int	-> Heap a
heapNew	size	= heap
 where
	heap	= Heap {
			hCell 		= cell,
			hCells		= size,

			hFreeCells	= size,
			hFreeHead	= 0
		} 

	-- an empty heap contains Free cells, which are joined together as
	--	a linked list. The last one points outside the heap, signifying
	--	the end of the list.
	cell	= Map.fromList 	
			$  [(i,		Free (i+1)) | i <- [0.. size-2] ]
			++ [(size - 1,	Free (-1)]


-- Add a cell to the heap					
heapAdd :: Show a 
	=> Heap a 		-- old heap.
	-> a 			-- cell to add.
	-> (Heap a, Int)	-- new heap, and ix of added element.

heapAdd	heap elem	=
  	| (hFreeHead heap) == -1
	= error "heapAdd: out of space"
 
	| otherwise
	= case Map.lookup (hFreeHead heap) (hCell heap) of
		(Free	next)	-> (nHeap,  (hFreeHead heap))
		 where
			nHeap = heap { 
				hFreeHead	= next,
				hFreeCells	= (hFreeCells heap) - 1,

				hCell		= Map.insert (hFreeHead heap) (Active elem) (hCell heap)
			}


heapRemove ::	Heap a ->	Int	-> (Heap a, a)
heapRemove 	heap		ix	= 
 let
	cell	= heapGet heap ix
	
	nhCell	= (hCell heap) // [(ix, Free (hFreeHead heap))]
	nHeap 	= heap {
			hCell		= nhCell,
			hFreeHead	= ix,
			hFreeCells	= (hFreeCells heap) + 1 }
 in
	(nHeap, cell)




heapGet :: 	Heap a ->	Int	-> a
heapGet		heap		ix	
	= case (hCell heap) ! ix of 
		Active	a		-> a
		Free	_		-> error ("heapGet - element at " ++ (show ix) ++ " is free")


heapUpdate :: 	Heap a -> 	(Int, a)-> Heap a
heapUpdate	heap		(p, x)	= heap { hCell = (hCell heap) // [(p, Active x)] }


heapMod	::	Heap a ->	(a -> a) ->	Int ->	Heap a
heapMod 	heap		f		ix =
	let
		cell0	= heapGet heap ix
		cell1	= f cell0
	in
	heapUpdate heap (ix, cell1)


heapShow :: Show a => 	Heap a		-> String
heapShow		heap		
	=  "freeCells   = " ++ show (hFreeCells heap)	++ "\n"
	++ "freeHead    = " ++ show (hFreeHead heap)	++ "\n"
	++ heapShow' heap 0

heapShow'	heap	ix	= 
	let
		(low, high)	= bounds (hCell heap)
	in
	if ix > high
		then	[]
		else 	case (hCell heap) ! ix of 
--				Free cell	-> (show ix) ++ ": " ++ (show cell) ++ "\n" ++ heapShow' heap (ix + 1)
				Free _		-> heapShow' heap (ix + 1)
				Active cell	-> (show ix) ++ ": " ++ (show cell) ++ "\n" ++ heapShow' heap (ix+1)



heapCellIsFree ::	Heap a -> 	Int	-> Bool
heapCellIsFree		heap		ix 
	= case (hCell heap) ! ix of
		Active	_		-> False
		Free	_		-> True

heapCellIsActive heap ix	= not (heapCellIsFree heap ix)




heapMap	::	Heap a ->	(a -> a) ->	Heap a
heapMap		heap		f		=
 let	
	heapMod' heap ix	= heapMod heap f ix 
	(low, high)		= (0, hCells heap)
 in	
	chain heapMod' heap [low..high]




