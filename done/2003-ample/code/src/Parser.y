----------------------------------------------------------------------------------------------------------------------------- SParse
--
-- Happy source for the Parser.
--
--	2/2002	Ben Lippmeier	ben.lippmeier@cs.anu.edu.au
--
--	Returns a nifty algebraic type representing the parsed expression. 
--
--
--	WOOP WOOP WOOP:
--		If the generated Haskell code is to be compiled with GHC generate, it with
--			happy -a -g -c SParse.y -oSParse.hs -iSParse.info
--
--		These flags are mentioned in the Happy documentation.
--		
--		Failure to include the -a -g -c options will cause GHC to use huge amounts
--		of heap space for no aparent reason. (circa 270Megs)
--
--
--	WOOP WOOP WOOP WOOP:
--		If you then try to load this generated source into ghci, all you will get is
--		parse errors. (And no warning / error messages emitted by happy or ghci). 
--		
--		You must omit the -g -c flags to generate source which will work in ghci. 
--
--		I don't know why this is, I was using
--			Happy	v1.11
--			GHC	5.02.1		
--			on Linux
--
--	Things to do:
--		Parsing of complex patterns is untested because the StoM module 
--		can't translate them to the core language yet.  
--
--
{
module Parser (
	parse,
	Exp(..), Var, Type, Constant(..), Pattern(..),
	coreVars,
	VarTable

) where

import Lexer
import Util
}

%name 		parse
%tokentype	{ Token }

%token
	tag		{ TokenTag		$$	}

	lexer_error	{ TokenError		$$	}

	var		{ TokenVarI 		$$	}

	type		{ TokenType		$$	}

	char		{ TokenCChar		$$	}
	float		{ TokenCFloat		$$	}
	int		{ TokenCInt		$$	}
	string		{ TokenCString		$$	}	

	case		{ TokenKCase			}
	else		{ TokenKElse			}
	if		{ TokenKIf			}
	in		{ TokenKIn			}
	let		{ TokenKLet			}
	of		{ TokenKOf			}
	par		{ TokenKPar			}
	seq		{ TokenKSeq			}
	then		{ TokenKThen			}

	aand		{ TokenSAAnd			}
	bbang		{ TokenSBBang			}
	eequals		{ TokenSEEquals			}
	nequals		{ TokenSNEquals			}
	leq		{ TokenSLessThanEqual		}
	meq		{ TokenSMoreThanEqual		}
	oor		{ TokenSOOr			}
	pplus		{ TokenSPPlus			}
	rightarrow	{ TokenSRightArrow		}



	'&'		{ TokenSAnd			}
	'!'		{ TokenSBang			}
	'('		{ TokenSBracketOpen		}
	')'		{ TokenSBracketClose		}
	':'		{ TokenSColon			}
	','		{ TokenSComma			}
	'{'		{ TokenSCurlyBracketOpen	}
	'}'		{ TokenSCurlyBracketClose	}
	'$'		{ TokenSDollar			}
	'.'		{ TokenSDot			}
	'='		{ TokenSEquals			}
	'#'		{ TokenSHash			}
	'^'		{ TokenSHat			}
	'/'		{ TokenSLambda 			}
	'<'		{ TokenSLessThan		}
	'-'		{ TokenSMinus			}
	'>'		{ TokenSMoreThan		}
	'|'		{ TokenSOr			}
	'%'		{ TokenSPercent			}
	'+'		{ TokenSPlus			}
	';'		{ TokenSSemiColon		} 
	'['		{ TokenSSquareBracketOpen	}
	']'		{ TokenSSquareBracketClose	}
	'*'		{ TokenSStar			}

%%


------------------------------------------------------------------------------------------------------------------------------------ 


Exp		: Exp1						{ $1							}
		| Exp1 '$'	Exp				{ ExpExp	$1 $3					}
		| '/' 	var '.' Exp				{ Lambda	$2 $4					}
		| let 	'{' Lets '}' 	in Exp 			{ Let		$3 $6					}
		| case 	Exp		of  '{' CaseAlts '}'  	{ Case		$2 $5					}
		| if Exp then Exp else Exp	{ Case $2 [(PConstruct "True" [], $4), (PConstruct "False" [], $6)]	}

Exp1		: Exp2						{ $1							}
		| Exp1 seq	Exp2				{ Seq		$1 $3					}
		| Exp1 par	Exp2				{ Par		$1 $3					}

Exp2		: Exp3						{ $1							}
		| Exp2 ':' 	Exp3				{ Construct	"Cons"	[$1, $3]			}
		| Exp2 oor	Exp3				{ ExpExp	(ExpExp (cVar "or")		$1) $3	}
		| Exp2 aand	Exp3				{ ExpExp	(ExpExp (cVar "and")		$1) $3	}
		| Exp2 pplus	Exp3				{ ExpExp	(ExpExp (cVar "append")		$1) $3	}

Exp3		: Exp4						{ $1							}
		| Exp3 eequals	Exp4				{ ExpExp	(ExpExp (cVar "eq")	$1) $3		}
		| Exp3 nequals  Exp4				{ ExpExp	(ExpExp (cVar "noteq")	$1) $3		}
		| Exp3	'<'	Exp4				{ ExpExp	(ExpExp (cVar "lessThan")	$1) $3	}
		| Exp3	'>'	Exp4				{ ExpExp	(ExpExp (cVar "moreThan")	$1) $3	}
		| Exp3	meq	Exp4				{ ExpExp	(ExpExp (cVar "moreThanEqual")	$1) $3	}
		| Exp3	leq	Exp4				{ ExpExp	(ExpExp (cVar "lessThanEqual")	$1) $3	}
	
Exp4		: Exp5						{ $1							}
		| Exp4 	'+' 	Exp5				{ ExpExp	(ExpExp (cVar "add") 	$1) $3		}
		| Exp4 	'-' 	Exp5				{ ExpExp	(ExpExp (cVar "sub") 	$1) $3		}

Exp5		: Exp6						{ $1							}
		| Exp5	'*' 	Exp6				{ ExpExp 	(ExpExp (cVar "mul")	$1) $3		}
		| Exp5	'%' 	Exp6				{ ExpExp 	(ExpExp (cVar "mod")	$1) $3		}
		| Exp5	'/' 	Exp6				{ ExpExp	(ExpExp (cVar "div")	$1) $3		}

Exp6		: Exp7						{ $1							}
		| Exp6	'^'	Exp7				{ ExpExp	(ExpExp (cVar "exp")	$1) $3		}
		| '!'	Exp7					{ ExpExp	(cVar "not")		$2		}
		| '-'	Exp7					{ ExpExp	(cVar "neg")		$2		}

Exp7		: Exp8						{ $1							}
		| Exp7	bbang	Exp8				{ ExpExp	(ExpExp (cVar "index") 	$1) $3		}	
		| Exp7	'!'	Exp8				{ ExpExp	(ExpExp (cVar "select")	$1) $3		}

Exp8		: Exp9						{ $1							}
		| type	ExpAsN					{ Construct	$1 $2					}
		| '#' 	var					{ PrimFunc	$2					}
	
Exp9		: ExpE						{ $1							}
		| Exp9	ExpE					{ ExpExp	$1 $2					}	
		
ExpA		: ExpE						{ $1							}
		| type						{ Construct	$1 []					}

ExpE		: var						{ Var 		$1					}
		| Constant					{ Constant	$1					} 	
		| List						{ $1							}
		| Tuple						{ $1							}
		| '(' Exp ')'					{ $2							}

Constant	: int						{ CInt 		$1					}
		| char						{ CChar		$1					}	
		| string					{ CString	$1					}

ExpAsN		: 						{ []							}
		| ExpA ExpAsN					{ $1 : $2						}

ExpEs		: ExpE						{ [$1]							}
		| ExpE ExpEs					{ $1 : $2						}

Lets		: Let ';'					{ [$1] 							}
		| Let ';' Lets					{ $1 : $3 						}
	
Let		: var LetArgs	'=' Exp				{ ($1, $2 $4)						}

LetArgs		: 						{ id				}
		| var LetArgs					{ (\x -> Lambda $1 x) . $2	} -- ooh, tricky

Pat0		: Pat1				{ $1									}
		| type Pats			{ PConstruct	$1 $2							}

Pat1		: var				{ PVar		$1							}
		| Constant			{ PConstant	$1							}
		| '[' ']'			{ PConstruct	"Nil"					[]		}
		| '(' Pat0 ':' Pat0 ')'		{ PConstruct 	"Cons"					($2:[$4])	}
		| '(' Pat0 ',' PatC ')'		{ PConstruct	("Tuple" ++ (show $ length ($2:$4) ))	($2:$4)		}
		| '(' Pat0 ')'			{ $2									}

Pats		: 						{ []							}
		| Pat1 Pats					{ $1 : $2						}

PatC		: Pat0						{ [$1]							}
		| Pat0 ',' PatC					{ $1 : $3						}


CaseAlts	: CaseAlt ';'					{ [$1]							}
		| CaseAlt ';' CaseAlts				{ $1 : $3						}
	
CaseAlt		: Pat0			rightarrow Exp		{ ($1, $3)						}


List		: '[' ListElems ']'				{ $2							}
ListElems	:						{ Construct "Nil"	[]				}				 
		| Exp						{ Construct "Cons"	[$1, Construct "Nil" []]	}
		| Exp ',' ListElems				{ Construct "Cons"	[$1, $3]			}	

Tuple		: '(' Exp TupleElems ')'			{ Construct ("Tuple" ++ show (length ($2:$3)))	($2:$3)	}
TupleElems	: ',' Exp					{ [$2]							}
		| ',' Exp TupleElems				{ $2 : $3						}


{
-------------------------------------------------------------------------------------------------------------------------------- Exp
-- Here is the type which represents the expression.
--
--
-- type	Dxp		= (Int, Exp)

data	ExpMeta
	= ExpMeta {
		emLineNumber	:: Int
	}

type	Var		= Int
type	Type		= String

data	Exp
	= Lambda	Var		Exp
	| ExpExp	Exp		Exp
	| Let		[(Var, Exp)]	Exp 
	| Construct	Type		[Exp]
	| Case		Exp		[(Pattern, Exp)]
	| PrimFunc	Var
	| Seq		Exp		Exp
	| Par		Exp		Exp
	| Var		Var
	| Constant	Constant
 deriving Show

data	Pattern
	= PConstruct	String		[Pattern]
	| PConstant	Constant
	| PVar		Var
 deriving Show

data	Constant
	= CInt 		Int
	| CFloat	Double
	| CChar		Char
	| CString	String
 deriving (Show, Eq)



cVar :: String	-> Exp
cVar	var	= 
	let	
		mNum	= indexOf coreVars var
	in
		case mNum of
			Nothing		-> error ("cVar: variable " ++ var ++ " not in builtin list")
			Just num	-> Var num

-- after adding to this list, must update 
-- 	Lexer.hs stripTableFirstIx to its length
coreVars = [
	"add",
	"and",
	"append",
	"div",
	"eq",
	"exp",
	"index",
	"lessThan",
	"lessThanEqual",
	"mul",
	"mod",
	"moreThan",
	"moreThanEqual",
	"not",
	"noteq",
	"neg",
	"or",
	"select",
	"sub"
	]




type 	VarTable	= [(String, Int)]




happyError ::	[Token] -> a
happyError tok 	= error "Parse Error\n"

}







