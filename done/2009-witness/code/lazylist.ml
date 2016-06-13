open Lazy

type 'a list
	= Nil 
	| Cons of 'a * (('a lazylist) Lazy.t);;


let rec llength xx
 = match xx with 
	Nil 		-> 0
      | Cons (x,xs)	-> 1 + llength (force xs);;


print_int (llength (Cons (0, lazy Nil)));;
print_newline ();;