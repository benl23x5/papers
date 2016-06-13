

module Pretty (
	prettyExp,
	prettyProgCell
) 
where


import	Type
import	Heap
import	Array

prettyExp prog ix	= "\n" ++ indent (prettyExp' prog ix) 0 ++ "\n"

indent	xx		i	=
 let
 	pad 	= replicate i '\t'
 in
 case xx of 
 	[]			-> []
 	('@':'>':xs)		-> indent xs (i+1)
 	('@':'<':xs)		-> indent xs (i-1)
	('\n':xs)		-> "\n" ++ pad		++ indent xs i
 	(x:xs)			-> x : (indent xs i)
 


prettyExp' :: Prog -> Int -> String
prettyExp' prog ix = 
 let
	f ix 		= prettyExp' prog ix

	progCell 	= 
		case ((hCell prog) ! ix) of 
			Active x	-> x

	exp		= pcExp progCell
	
	prettyLetTrim	(l, t)			= "|" ++ show t ++ "\n" ++ (f l) ++ ";\n\n"
	prettyAlt	((name, a), (ix, t))	= name	  ++ "," ++ show a ++ " ->@>\n|" ++ show t ++ "\n" ++ (f ix) ++ "@<;" ++ "\n\n"
 in
 case exp of 
	Lambda 		ix			-> "/(" ++  (f ix) ++ ")"

	ExpVar		ix	vix		-> "("	++  (f ix) ++ " $" ++ (show vix) ++ ")"

	Var		vix			-> "$" ++ (show vix)

	Let		lts	(ix, trim)	-> "let {@>\n" ++ pLets ++ "@<\n} in @>\n|" ++ show trim ++ "\n" ++ (f ix) ++ "@<\n"  ++ "\n"
	 where
		pLets			= concat (map prettyLetTrim lts)

	Construct	name 	a		-> (name ++ "," ++ show a)

	Case		(ix, trim) alts		-> "case " ++ pExp ++ " of { @>\n|" ++ show trim ++ "\n" ++ pAlts ++ "@<\n}"
	 where
		pExp			= f ix
		pAlts			= concat (map prettyAlt alts)


	Constant	c			-> "C" ++ prettyConst c

	PrimFunc	name			-> "#" ++ name

	Seq		e1 e2			-> "(" ++ prettyExp prog e1 ++ ") seq (" ++ prettyExp prog e2 ++ ")"

	Par		vix  e2			-> "$" ++ (show vix) ++ " par (" ++ prettyExp prog e2 ++ ")"


prettyConst	c	=
 case c of 
	CInt	i			-> show i
	CFloat	f			-> show f
	CChar	h			-> show h



prettyProgCell ProgCell {
			pcExp		= exp,
			pcSourceLine	= sourceLine
		}
	= show exp ++ ", " ++ show sourceLine
		


