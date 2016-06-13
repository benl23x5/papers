
module ProgUtil (
	feedVar,	slurpEixs,	slurpVars
)
where

import	Type
import	Heap
import	Util


------------------------------------------------------------------------------------------------------------------------ feedVar
-- feedVar
--	Decends a program tree, keeping track of the source line number for
--	the visited cells. If a cell doesn't have an associated source line
--	line number then it is set to be the same as last cell visited.
--
feedVar  ::	Prog ->	Int		-> Prog
feedVar		prog	top		= feedVar' 0 prog top

feedVar'	line	prog	top =
 let
	cell	= heapGet prog top
	nTops	= slurpEixs	(pcExp cell)

	nLine	= pcSourceLine	cell
 in
 if nLine == 0
	then	
		let
			nCell	= cell 		{ pcSourceLine = line }
			nProg	= heapUpdate prog (top, nCell)
		in
		chain (feedVar' line)  nProg nTops

	else	chain (feedVar' nLine) prog  nTops



----------------------------------------------------------------------------------------------------------------------- slurpEixps
-- slurpEixs
--	returns a list of all the Eixs present in the given expression.
--
slurpEixs :: 	Exp				-> [Int]
slurpEixs	exp				= 
 case exp of
	Lambda		e1			-> [e1]
	ExpVar		e1 v			-> [e1]
	Var		v			-> []
	Let		ets	(e1, t1)	-> (map fst ets) ++ [e1]
	Case		(eix, t)	alts	-> eix : map fst (map snd alts)
	Construct	name	a		-> []
	Constant	c			-> []
	PrimFunc	name			-> []
	Seq		e1	e2		-> [e1, e2]
	Par		v	e2		-> [e2]


------------------------------------------------------------------------------------------------------------------------ slurpVars
-- slurpVars
--	returns the list of Vars used by an Exp
--
slurpVars :: 	Exp				-> [Int]
slurpVars	exp				= 
 case exp of
 	ExpVar		e1 v			-> [v]
 	Var		v			-> [v]
	Construct	name a			-> if a == 0 then [] else [0 .. (a-1)]

	PrimFunc	"intAdd"		-> [0, 1, 2, 3]
	PrimFunc	"intSub"		-> [0, 1, 2, 3]
	PrimFunc	"intMul"		-> [0, 1, 2, 3]
	PrimFunc	"intDiv"		-> [0, 1, 2, 3]
	PrimFunc	"intMod"		-> [0, 1, 2, 3]
	PrimFunc	"intEq"			-> [0, 1, 2, 3]

	PrimFunc	"intMoreThan"		-> [0, 1, 2, 3]
	PrimFunc	"intMoreThanEq"		-> [0, 1, 2, 3]
	PrimFunc	"intLessThan"		-> [0, 1, 2, 3]
	PrimFunc	"intLessThanEq"		-> [0, 1, 2, 3]

	_					-> []


