
module Trimmers (
	depthTag,
	gatherFreeVars,
	buildTrimmers,	buildTrimmer,
	rewriteVars
)

where

import ProgUtil
import Type
import Util
import Heap
import List


------------------------------------------------------------------------------------------------------------------------- depthTag
-- depthTag
--
--
depthTag :: Prog ->	Eix	-> Prog
depthTag    prog	eix	= 
 let
 	cell		= heapGet prog eix

 	nCell		= cell { pcDepth	= 0 }
 	prog1		= heapUpdate prog (eix, nCell)
	eixs		= slurpEixs (pcExp cell)
 in
	case pcExp cell of 
		Let	lets (e1, t1)	-> chain (depthTag' 0) prog1 eixs
		_			-> depthTag' 0 prog eix


depthTag' ::	Int -> Prog -> Eix	-> Prog
depthTag'	depth prog0	eix	=
 let
 	cell	= heapGet prog0 eix
	nCell	= cell { pcDepth	= depth }
	prog1	= heapUpdate prog0 (eix, nCell)

	exp	= pcExp cell

 in
 	case exp of
 		Lambda		e		-> depthTag' (depth + 1)	prog1 e
		ExpVar		e v		-> depthTag' depth		prog1 e

		Let 		lets (e1, t1)	-> progE
		 where
			nDepth			= depth + length lets
			eixs			= map fst lets
			progLs			= chain 	(depthTag' nDepth) prog1 eixs
			progE			= depthTag' nDepth progLs e1
	
		Case		(e1, t1) alts	-> progA
		 where
	 		progC			= depthTag' depth	prog1 e1

			depthTagAlt prog ((name, a), (eix, trimmer))	= depthTag' (depth + a) prog eix
			progA			= chain	depthTagAlt progC alts

		Seq		e1 e2		-> progE2
		 where
	 		progE1			= depthTag' depth prog1  e1
			progE2			= depthTag' depth progE1 e2

		Par		v  e2		-> depthTag' depth		prog1 e2
	
		_				-> prog1
 		

------------------------------------------------------------------------------------------------------------------- gatherFreeVars
--
--
--
gatherFreeVars :: Prog -> Eix		-> [Int]
gatherFreeVars  prog eix	= sort $ filter (\x -> x >= 0) $ nub $ gatherFreeVars' prog eix

gatherFreeVars'	prog eix	=
 let
 	cell	= heapGet prog eix
 	exp	= pcExp cell
	depth	= pcDepth cell
 
	vars	= slurpVars exp
	varsR	= map (\x -> x - depth) vars
	
	eixs	= slurpEixs exp
 in
 	varsR ++ concat (map (gatherFreeVars' prog) eixs)
  

-------------------------------------------------------------------------------------------------------------------- buildTrimmers
--
--
--
buildTrimmers :: Prog -> Eix				-> Prog
buildTrimmers	 prog	 eix				= nProg
 where
 	prog1	= buildTrimmer prog eix 

 	eixs	= slurpEixs (pcExp (heapGet prog1 eix))
 	nProg	= chain	buildTrimmers prog1 eixs

--------------------------------------------------------------------------------------------------------------------- buildTrimmer
--
--
--
buildTrimmer :: Prog -> Eix		-> Prog
buildTrimmer	prog	eix		= nProg
 where
 	exp	= pcExp $ heapGet prog eix
 	prog1	= depthTag prog eix
 
 	nProg 	= 
 	 case exp of 
		Let	_	_		-> buildTrimmer_Let	prog1	eix exp
		Case	_	_		-> buildTrimmer_Case 	prog1	eix exp
		_				-> prog

buildTrimmer_Let ::	Prog ->	Eix ->	Exp			-> Prog
buildTrimmer_Let	prog	eix	(Let lets (e1, t1)) 	= progR
 where
-- attach
	nLet (eix, _)		= (eix, gatherFreeVars prog eix)
	nLets			= map nLet lets

	nt1			= gatherFreeVars prog e1

	progA			= heapUpdate prog (eix, (heapGet prog eix) { pcExp = Let nLets (e1, nt1) })

-- rewrite
	mapping1		= zip nt1 [0..]
	prog1			= rewriteVars mapping1 progA e1
	
	rewriteLet prog (eix, tn) =
	 let
		mapping		= zip tn [0..]
	 in
		rewriteVars mapping prog eix
	
	progR			= chain rewriteLet prog1 nLets


buildTrimmer_Case ::	Prog ->	Eix ->	Exp			-> Prog
buildTrimmer_Case	prog	eix	(Case (e1, t1) alts)	= progR
 where
-- attach
 	altEixs			= map (fst . snd) alts
 	nt1			= sort $ nub $ concat $ map (gatherFreeVars prog) altEixs
 
 	attachAlt ((name, a), (eix, _))
 				= ((name, a), (eix, trimmer))
 	 where
 		vars		= gatherFreeVars prog eix
 		trimBaseIx	= map (mJust . (indexOf nt1)) vars
 		trimmer		= [0 .. a-1] ++ map (+ a) trimBaseIx

	nAlts			= map attachAlt alts
 	
	progA			= heapUpdate prog (eix, (heapGet prog eix) { pcExp = Case (e1, nt1) nAlts })

-- rewrite
--	mapping1		= zip nt1 [0..]
--	prog1			= rewriteVars mapping1 progA e1

	rewriteAlt prog ((name, a), (eix, tn)) =
	 let
		vars		= gatherFreeVars prog eix
		mapping		= zip vars [0..]
	 in
		rewriteVars mapping prog eix
	
	progR			= chain rewriteAlt progA nAlts	


---------------------------------------------------------------------------------------------------------------------- rewriteVars
--
--
--
rewriteVars ::	[(Int, Int)] ->	Prog -> Eix		-> Prog
rewriteVars	mapping		prog	eix		= nProg
 where
 	cell	= heapGet prog eix
 	exp	= pcExp cell

	depth	= pcDepth cell

	rewrite	[]		v	= v
	rewrite ((x,y):xs)	v	= if v == (x+depth) then (y+depth) else rewrite xs v

	nExp	= 
	 case exp of
		ExpVar	e 	v 	-> ExpVar e	(rewrite mapping v)
		Var		v	-> Var		(rewrite mapping v)
		x			-> x
		
	nCell	= cell { pcExp = nExp }
	
	prog1	= heapUpdate prog (eix, nCell)

	eixs	= slurpEixs nExp	
	nProg	= chain (rewriteVars mapping) prog1 eixs

