
Require Export Lambda.DSIM.Base.
Require Export Lambda.DSIM.Ty.


(********************************************************************)
(* Value Expressions *)
Inductive exp : Type :=
 (* Variables want to be named. *)
 | XVar    (v: name) : exp

 (* Function abstraction. *)
 | XAbs    (ss: env exp) (v: name) (t: ty) (x: exp) : exp

 (* Function application. *)
 | XApp    (x1: exp) (x2: exp) : exp.

Hint Constructors exp.


(* A substitution is an environment of expression bindings. *)
Definition substx := env exp.


(* Indirect induction principle for expressions.

   In the definition of 'exp', abstractions contain a list of 
   subexpressions, so 'exp' is indirectly recursive via the list 
   data type.

   In the following induction principle, when proving a property
   of abstractions we know the property being proved applies to 
   subexpressions contained in those lists. The automatically
   generated induction principle 'exp_ind' doesn't give us this.
*)
Theorem exp_iind
 : forall
    (PX : exp -> Prop)

 ,  (  forall n
    ,  PX (XVar n))

 -> (  forall ss n t x
    ,  Forall (fun b => PX (expOfBind b)) ss
    -> PX x
    -> PX (XAbs ss n t x))

 -> (  forall x1 x2
    ,  PX x1  -> PX x2
    -> PX (XApp x1 x2))

 -> forall  x, PX x.
Proof.
 intros PX.
 intros Hvar Habs Happ.
 refine (fix IHX x: PX x := _).

 case x; intros.
 - apply Hvar.
 - eapply Habs.
   induction ss as [| b].
   + apply Forall_nil.
   + apply Forall_cons.
     * destruct b. simpl. eapply IHX.
     * assumption.
   + apply IHX.
 - apply Happ.
   + apply IHX.
   + apply IHX.
Qed.


(********************************************************************)
(* Values *)
Inductive Value : exp -> Prop :=
 | ValueAbs 
   :  forall ss v t x
   ,  Value (XAbs ss v t x).

Hint Constructors Value.


Lemma isValue_not_var
 : forall v
 , ~Value (XVar v).
Proof.
 intros. intuition. inverts H.
Qed.
Hint Resolve isValue_not_var.


Lemma isValue_not_app
 : forall x1 x2
 , ~Value (XApp x1 x2).
Proof.
 intros. intuition. inverts H.
Qed.
Hint Resolve isValue_not_app.


(* Done expressions have finished evaluating. *)
Inductive Done : exp -> Prop :=
 | DoneVar 
   :  forall v
   ,  Done (XVar v)

 | DoneValue
   :  forall x
   ,  Value  x
   -> Done   x

 | DoneApp 
   :  forall x1 x2
   ,  Done x1 /\ ~Value x1
   -> Done (XApp x1 x2).

Hint Constructors Done.


(* Invert all hypothesis that are compound done statements. *)
Ltac inverts_done :=
 repeat 
  (match goal with 
   | [ H: Done (XApp _ _) |- _ ] => inverts H
   end).

