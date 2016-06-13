
(* Simply Typed Lambda Calculus (STLC) 
   Using named binders and delayed substitution. *)

(* Types, expressions, normal forms, values, lifting and substitution *)
Require Export Lambda.DSIM.Ty.
Require Export Lambda.DSIM.Exp.

(* Typing judgement and environment weakening. *)
Require Export Lambda.DSIM.TypeX.

(* Substitution of exps in exps preserves typing. *)
Require Export Lambda.DSIM.SubstXX.

(* Small step evaluation. *)
Require Export Lambda.DSIM.Step.

(* A well typed expression is either a value or can take a step. *)
Require Export Lambda.DSIM.Progress.

(* When an expression takes a step then the result has the same type. *)
Require Export Lambda.DSIM.Preservation.