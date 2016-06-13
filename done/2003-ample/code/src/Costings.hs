
module Costings (
	uniform,
	distributed
)

where

uniform	:: 	String	-> Int
uniform   	_	=  1

distributed ::	String	-> Int
distributed	rule	=
	if elem rule ["var2", "var3", "app1"]	then 10
 
	else
	if elem rule ["var1", "app3"]		then 5

	else	1

