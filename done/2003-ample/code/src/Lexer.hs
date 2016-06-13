
module Lexer (

	Token(..),
	lexer,
	stripTable
)
where

---------------------------------------------------------------------------------------------------------- lexer
import Util

data Token
	= TokenTag		Int
	| TokenError		String
	| TokenEOF		
	
	| TokenVarS		String
	| TokenVarI		Int
	| TokenType		String

	| TokenCChar		Char
	| TokenCFloat		Double
	| TokenCInt		Int
	| TokenCString		String

	| TokenKCase
	| TokenKElse
	| TokenKIf
	| TokenKIn
	| TokenKLet	
	| TokenKOf
	| TokenKPar
	| TokenKSeq
	| TokenKThen

	| TokenSAnd
	| TokenSAAnd
	| TokenSBang
	| TokenSBBang
	| TokenSBracketOpen
	| TokenSBracketClose
	| TokenSColon
	| TokenSComma
	| TokenSCurlyBracketOpen
	| TokenSCurlyBracketClose
	| TokenSDollar
	| TokenSDot
	| TokenSEquals
	| TokenSEEquals
	| TokenSHash
	| TokenSHat
	| TokenSMinus
	| TokenSMoreThan
	| TokenSMoreThanEqual
	| TokenSNewLine
	| TokenSNEquals
	| TokenSLambda
	| TokenSLessThan
	| TokenSLessThanEqual
	| TokenSOr
	| TokenSOOr
	| TokenSPercent
	| TokenSPPlus
	| TokenSPlus
	| TokenSRightArrow
	| TokenSSemiColon
	| TokenSSquareBracketOpen
	| TokenSSquareBracketClose
	| TokenSStar
	
 deriving Show

lexKeywords 	=
	[ ("case",	TokenKCase)
	, ("else",	TokenKElse)
	, ("if",	TokenKIf)
	, ("in",	TokenKIn)
	, ("let",	TokenKLet)
	, ("of",	TokenKOf)
	, ("par",	TokenKPar)
	, ("seq",	TokenKSeq)
	, ("then",	TokenKThen)
	]


-- must be in decending order of length
lexSymbols	=
	[ ("&&",	TokenSAAnd)
	, ("!!",	TokenSBBang)
	, ("==",	TokenSEEquals)
	, ("!=",	TokenSNEquals)
	, ("++",	TokenSPPlus)
	, ("<=",	TokenSLessThanEqual)
	, (">=",	TokenSMoreThanEqual)
	, ("||",	TokenSOOr)
	, ("->",	TokenSRightArrow)

	, ("&",		TokenSAnd)
	, ("!",		TokenSBang)
	, ("(",		TokenSBracketOpen)
	, (")",		TokenSBracketClose)
	, (":",		TokenSColon)
	, (",",		TokenSComma)
	, ("{",		TokenSCurlyBracketOpen)
	, ("}",		TokenSCurlyBracketClose)
	, ("$",		TokenSDollar)
	, (".", 	TokenSDot)
	, ("=",		TokenSEquals)
	, ("#",		TokenSHash)
	, ("^",		TokenSHat)
	, ("-",		TokenSMinus)
	, (">",		TokenSMoreThan)
	, ("/", 	TokenSLambda)
	, ("<",		TokenSLessThan)
	, ("\n",	TokenSNewLine)
	, ("|",		TokenSOr)
	, ("%",		TokenSPercent)
	, ("+",		TokenSPlus)
	, (";", 	TokenSSemiColon)
	, ("[",		TokenSSquareBracketOpen)
	, ("]",		TokenSSquareBracketClose)
	, ("*",		TokenSStar)
	]


isAlphaLower	c	= (c >= 'a') && (c <= 'z')
isAlphaUpper	c	= (c >= 'A') && (c <= 'Z')
isAlpha		c	= (isAlphaLower c) || (isAlphaUpper c)
isDigit		c	= (c >= '0') && (c <= '9')
isSpace		c	= (c == ' ') || (c == '\t')
isSymbol	c	= not (isAlpha c || isDigit c || isSpace c)
isIdentifier	c	= isAlpha c || isDigit c || c == '_' || c == '\''

------------------------------------------------------------------------------------------------------------------------------------ lexer

stripTableFirstIx	= 19				-- where to start counting user variables
							-- variables for inbuilt functions must all fit below this


stripTable :: [Token]	-> ([(String, Int)], [Token])
stripTable tokens					= st' 1 stripTableFirstIx [] [] tokens

st' line tableIx table otok tok =
 case tok of 
	[]			-> (table, otok)
	(TokenSNewLine : ts) 	-> st' (line + 1) 	tableIx 	table 				otok 				ts
	(TokenVarS var : ts)	-> st' line		(tableIx + 1)   (table ++ [(var, line)])	(otok ++ [TokenVarI tableIx])	ts
	(TokenError str : ts)	-> error ("lexer:" ++ (show line) ++ ":" ++ str)

	(t : ts) 		-> st' line		tableIx		table				(otok ++ [t])			ts



lexer 	:: 	String -> 			[Token]
lexer 		[]				= []
lexer		('\'': '\\': '\'': '\'': cs)	= TokenCChar	'\''	: lexer cs
lexer		('\'': '\\': '\"': '\'': cs)	= TokenCChar	'\"'	: lexer cs
lexer		('\'': c : '\'':cs)		= TokenCChar	c	: lexer cs
lexer		('\"': cs)			= TokenSNewLine 	: lexString ('\"':cs)
lexer		('-' :  '-': cs)		= TokenSNewLine		: lexer (tail rest)
 where
	(comment, rest)		= splitWhen (== '\n') cs

lexer 		(c:cs) 
	| isAlphaUpper c			= lexType	(c:cs)
	| isAlpha c				= lexWord	(c:cs)
	| isSymbol c				= lexSymbol	(c:cs)
	| isDigit c				= lexNum	(c:cs)
	| isSpace c				= lexer	cs
	| c == '\n'				= TokenSNewLine	: lexer cs


lexString	('\"': cs) =
 let
	(str, rest)	= span (\x -> x /= '\"') cs
 in
 	TokenCString str : (lexer $ tail rest)
 

lexType		cs =
 let
	(str, rest)	= span isIdentifier cs
 in
	TokenType str	: lexer rest


lexWord	cs = 
 let
	(str, rest)	= span isIdentifier cs
	mtoken		= lookup str lexKeywords
 in
	case mtoken of
		Just token	-> token 		: lexer rest
		Nothing		-> TokenVarS str 	: lexer rest


lexSymbol	cs = 
 let
	(sym, rest1)	= span isSymbol cs
	(mtoken, rest0)	= lookupMunch sym lexSymbols
 in
	case mtoken of
		Just token	-> token : lexer (rest0 ++ rest1)
		Nothing		-> [TokenError ("unknown symbol '" ++ sym ++ "'")]


lexNum		cs = 
 let
	(numc, rest)	= span isDigit cs
	int		= read numc
 in
	TokenCInt int	: lexer rest





lookupMunch	a	[]		= (Nothing, a)
lookupMunch	a	((am, b):xs)	=
 let
	(match, rest)		= splitAt (length am) a
 in
	if match == am
		then	(Just b, rest)
		else	lookupMunch a xs


