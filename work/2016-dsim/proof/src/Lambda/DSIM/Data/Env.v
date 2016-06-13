
Require Export Coq.Strings.String.
Require Export Lambda.DSIM.Tactics.Chargueraud.
Require Export Lambda.DSIM.Data.Lists.


(* Variables want to be named. *)
Definition name 
  := string.


(* Boolean equality on names. *)
Definition eqName (n1 n2: name): bool
  := match string_dec n1 n2 with
     | left  _ => true
     | right _ => false
     end.


(* Bindings. *)
Definition bind (A: Type) 
  := prod string A.


(* Get the name of a binding. *)
Definition nameOfBind {X} (b: bind X): name
  := fst b.


(* Get the expression of a binding. *)
Definition expOfBind  {X} (b: bind X): X
  := snd b.


(* Apply a function to the expression component of a binding. *)
Definition mapExpOfBind {X} (f: X -> X) (b: bind X): bind X :=
  match b with
  | (n, x) => (n, f x)
 end.


(* An environment is a list of named bindings. *)
Definition env (A: Type) 
 := list (name * A).


