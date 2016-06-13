
Require Export Lambda.DSIM.Tactics.Chargueraud.
Require Export Lambda.DSIM.Tactics.Case.
Require Export Lambda.DSIM.Tactics.Rip.
Require Export Lambda.DSIM.Tactics.Nope.

Require Export Lambda.DSIM.Data.Lists.
Require Export Lambda.DSIM.Data.Env.


(********************************************************************)
(* Override the default notation for lists to be right biased.
   We're only using lists for environments and substitutions, 
   where the right biased order is more natural. *)

Notation "xs :> x"  := (x :: xs)   (at level 61, left associativity).
Notation "xs >< ys" := (app ys xs) (at level 60, right associativity).

