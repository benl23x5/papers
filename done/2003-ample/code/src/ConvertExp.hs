
module ConvertExp (
	convertExp
)

where

import	qualified Parser	as S
import	Type
import	ProgUtil
import	Heap
import	Util
import	List
import	Init


----------------------------------------------------------------------------------------------------------------------- convertExp
convertExp  ::  S.VarTable ->	Prog -> S.Exp ->	(Prog, Int)
convertExp	varTable	prog    exp		= convertExp' varTable prog [] exp


convertExp' :: 	S.VarTable -> 	Prog ->	[String] -> 	S.Exp -> (Prog, Int)
convertExp' 	varTable 	prog 	env 		exp =
 let
	cE		= convertExp' varTable
	cEs		= convertExps varTable
	varName	i 	= fst (varTable !! i)
	varLine i	= snd (varTable !! i)

	newCell	= ProgCell {
		pcExp		= Var 0,
		pcSourceLine	= 0,
		pcDepth		= 0
	}

	getVar var	= 
	 case indexOf env (varName var) of
		Just x	-> x
		Nothing -> error ("lineNumber! : convertExp - unbound variable '" ++ (varName var) ++ "\n")

 in
 case exp of 
	(S.Lambda	var	exp)		-> (progL, eixL)
	 where
		env1			= (varName var) : env
		(prog1, eix1)		= cE prog env1 exp
			
		cellL			= newCell { 
						pcExp		= Lambda eix1, 
						pcSourceLine	= varLine var
					}
		(progL, eixL)		= heapAdd prog1 cellL


	(S.ExpExp	exp1	(S.Var	var))	-> (progEV, eixEV)
	 where
		(prog1, eix1)		= cE prog env exp1			
		cellEV			= newCell 	{ pcExp	= ExpVar eix1 (getVar var) }
		(progEV, eixEV)		= heapAdd prog1 cellEV


	(S.ExpExp	exp1	exp2)		-> (progL, eixL)
	 where
		env1			= ("": env)			-- make space for extra Let				

		(prog1, eix1)		= cE prog  env1 exp1
		(prog2, eix2)		= cE prog1 env1 exp2
				
		cellEV			= newCell 	{ pcExp	= ExpVar eix1 0 }
		(progEV, eixEV)		= heapAdd prog2 cellEV

		cellL			= newCell 	{ pcExp	= Let [(eix2,[])] (eixEV,[]) }
		(progL,	eixL)		= heapAdd progEV cellL


	(S.Let		binds	exp)		-> (progL, eixL)
	 where
		(bindVars, bindExps)	= unzip binds
		letBindNames		= map varName bindVars
		env1			= (reverse letBindNames) ++ env

		(progLs, eixLs)		= cEs prog   env1 bindExps
		(progE, eixE)		= cE  progLs env1 exp

		cellL 			= newCell 	{ 
						pcExp		= Let (zip eixLs (repeat [])) (eixE, []),
						pcSourceLine	= varLine (head bindVars)
					}
		(progL, eixL)		= heapAdd progE cellL 


	(S.Var		var)			-> (progV, eixV)
	 where
		cellV			= newCell {
						pcExp		= Var (getVar var),
						pcSourceLine	= varLine var
					}
		(progV, eixV)		= heapAdd prog cellV


	(S.Construct	name	[])		-> (progC, eixC)
	 where
		cellC			= newCell 	{ pcExp	= Construct name 0}
		(progC, eixC)		= heapAdd prog cellC


	(S.Construct	name	exps)		-> (progL, eixL)
	 where
		len			= length exps
		env1			= ((take len (repeat "")) ++ env)

		cellC			= newCell 	{ pcExp	= Construct name len }
		(progC, eixC)		= heapAdd prog cellC

		(progLs, eixLs)		= cEs progC env1 (reverse exps)

		cellL			= newCell 	{ pcExp	= Let (zip eixLs (repeat [])) (eixC, []) }				
		(progL, eixL)		= heapAdd progLs cellL


	(S.Case		exp	alts)		-> (progC, eixC)
	 where
		(progE, eixE)		= cE prog env exp
		(progA, cAlts)		= convertAlts varTable progE env alts

		cellC			= newCell 	{ pcExp	= Case (eixE, []) cAlts }
		(progC, eixC)		= heapAdd progA cellC


	(S.PrimFunc	var)			-> (nProg, nIxp)
	 where
		cellPF			= newCell	{ pcExp	= PrimFunc (varName var) }
		(nProg, nIxp)		= heapAdd prog cellPF


	(S.Constant	(S.CString ss))		-> (nProg, eix)
	 where
		sourceString []		= (S.Construct "Nil"	[])
		sourceString (x:xs)	= (S.Construct "Cons"	[S.Constant (S.CChar x), sourceString xs])
	
		sourceSS		= sourceString ss
	
		(nProg, eix)		= cE prog env sourceSS
	

	(S.Constant	c	)		-> (progC, eixC)
	 where
		cellC			= newCell	{ pcExp = Constant (convertConst c) }
		(progC, eixC)		= heapAdd prog	cellC


	(S.Par		exp1	exp2)		-> (progL, eixL)
	 where
		env1			= ("": env)			-- make space for extra let

		(prog1, eix1)		= cE prog  env1 exp1
		(prog2, eix2)		= cE prog1 env1 exp2

		cellEV			= newCell	{ pcExp = Par 0 eix2 }
		(progEV, eixEV)		= heapAdd prog2 cellEV

		cellL			= newCell	{ pcExp = Let [(eix1,[])] (eixEV,[]) }
		(progL, eixL)		= heapAdd progEV cellL


	(S.Seq		e1 e2)			-> (progS, eixS)
	 where
		(prog1, eix1)		= cE prog  env e1
		(prog2, eix2)		= cE prog1 env e2
	
		cellS			= newCell	{ pcExp = Seq eix1 eix2 }
		(progS, eixS)		= heapAdd prog2 cellS




convertExps :: S.VarTable ->	Prog ->	[String] ->	[S.Exp]	-> (Prog, [Int])
convertExps  varTable prog env exps		= convertExps' varTable env (prog, []) exps

convertExps' varTable env (prog, eixs) []	= (prog, eixs)
convertExps' varTable env (prog, eixs) (e:es)	= convertExps' varTable env (progN, eixs ++ [eixN]) es
 where
	(progN, eixN)	= convertExp' varTable prog env e



-- this convertAlt stuff is a bit messy
--	the old Parser parses general patterns, but we don't translate them to the machine yet.

getPatVars :: [S.Pattern]	-> [S.Var]
getPatVars []			= []
getPatVars (S.PVar v : xs)	= v : getPatVars xs
getPatVars x			= error (":getPatVars - don't handle general patterns yet, died on " ++ (show x))



convertAlts :: S.VarTable ->	Prog ->	[String] -> 	[(S.Pattern, S.Exp)]	
	-> (Prog, [((String, Int), (Eix, Trimmer))])

convertAlts	varTable	prog	env		alts
	= convertAlts' varTable env (prog, []) alts


convertAlts'	varTable	env	(prog, eixs)	alts	=
 case alts of
	[]			-> (prog, eixs)
	(a:as)			-> convertAlts' varTable env (progN, eixs ++ [eixN] ) as
	 where
		(progN, eixN)	= convertAlt varTable prog env a


convertAlt ::	S.VarTable ->	Prog ->	[String] ->	(S.Pattern, S.Exp)
	-> (Prog, ((String, Int), (Eix, Trimmer)))

convertAlt	varTable	prog	env		(pattern, exp)		=
 let
	varName i	= fst (varTable !! i)
	
 in
	case pattern of
	(S.PConstruct name pVars)	-> ( progA, ((name, varNum), (eixA, [])) )
	 where
		rVars			= getPatVars pVars
		rVarNames		= map varName rVars

		varNum			= length pVars

		(progA, eixA)		= convertExp' varTable prog (rVarNames ++ env) exp


	(S.PVar var)			-> ( progA, (("Default", 0), (eixA, [])) )
	 where
		rVarName		= varName var
		(progA, eixA)		= convertExp' varTable prog (rVarName : env) exp
								

convertConst :: S.Constant -> 	Constant
convertConst 	c	= 
	case c of 
		S.CInt		i	-> CInt 	i
		S.CFloat 	f	-> CFloat 	f
		S.CChar 	h	-> CChar 	h		



