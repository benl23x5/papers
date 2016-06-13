
Require Export Lambda.DSIM.TypeX.
Require Export Lambda.DSIM.SubstXX.


(*******************************************************************)
(* Small Step evaluation *)
Inductive Step : exp -> exp -> Prop :=

 (* Evaluation in a context. *)
 | EsAppLeft 
   :  forall x1 x1' x2
   ,  Step   x1 x1'
   -> Step  (XApp x1 x2) (XApp x1' x2)

 | EsAppRight
   :  forall x1 x2 x2'
   ,  Value  x1
   -> Step   x2 x2'
   -> Step  (XApp x1 x2) (XApp x1 x2')

 (* Function application. *)
 | EsAbsApp 
   :  forall sx n1 t1 x1 x2
   ,  Done  x2
   -> Step  (XApp (XAbs sx n1 t1 x1) x2)
            (substXX (sx :> (n1, x2)) x1).

Hint Constructors Step.


(********************************************************************)
(* Multi-step evaluation
   A sequence of small step transitions.
   As opposed to StepsL, this version has an append constructor
   EsAppend that makes it easy to join two evaluations together.
   We use this when converting big-step evaluations to small-step. *)
Inductive Steps : exp -> exp -> Prop :=

 (* After no steps, we get the same exp.
    We need this constructor to match the EVDone constructor
    in the big-step evaluation, so we can convert between big-step
    and multi-step evaluations. *)
 | EsNone
   :  forall x1
   ,  Steps  x1 x1

 (* Take a single step. *)
 | EsStep
   :  forall x1 x2
   ,  Step   x1 x2
   -> Steps  x1 x2

 (* Combine two evaluations into a third. *)
 | EsAppend
   :  forall x1 x2 x3
   ,  Step   x1 x2 -> Steps x2 x3
   -> Steps  x1 x3.

Hint Constructors Steps.


(* Multi-step evaluation on the left of an application. *)
Lemma steps_context_left
 :  forall x1 x1' x2
 ,  Steps  x1 x1'
 -> Steps (XApp x1 x2) (XApp x1' x2).
Proof.
 intros x1 x1' x2 HS.
 induction HS; eauto.
Qed.


(* Multi-step evaluation on the right of an application. *)
Lemma steps_context_right
 :  forall sx n t x1 x2 x2'
 ,  Steps  x2 x2'
 -> Steps  (XApp (XAbs sx n t x1) x2)
           (XApp (XAbs sx n t x1) x2').
Proof.
 intros sx n t x1 x2 x2' HS.
 induction HS; eauto.
Qed.

