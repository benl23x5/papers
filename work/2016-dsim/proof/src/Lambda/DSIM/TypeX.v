
Require Export Lambda.DSIM.Exp.


(*******************************************************************)
(* Typing judgement assigns a type to an expression. *)
Inductive TypeX : tyenv -> exp -> ty -> Prop :=
 | TxVar 
   :  forall te n t
   ,  lookup (eqName n) te = Some t
   -> TypeX te (XVar n) t

 | TxLam
   :  forall te sx st n1 t1 x2 t2
   ,  TypeS  te sx st
   -> TypeX (te >< st :> (n1, t1)) x2 t2
   -> TypeX  te (XAbs sx n1 t1 x2) (TFun t1 t2)

 | TxApp
   :  forall te x1 x2 t1 t2
   ,  TypeX  te x1 (TFun t1 t2)
   -> TypeX  te x2 t1
   -> TypeX  te (XApp x1 x2) t2

with     TypeS : tyenv -> env exp -> env ty -> Prop :=
 | TsNil 
   :  forall te
   ,  TypeS  te nil nil

 | TsCons
   :  forall n x t te sx st
   ,  TypeS  te sx st
   -> TypeX  te x  t
   -> TypeS  te (sx :> (n, x)) (st :> (n, t)).

Hint Constructors TypeX.
Hint Constructors TypeS.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TypeX _ (XVar _) _       |- _ ] => inverts H
   | [ H: TypeX _ (XAbs _ _ _ _) _ |- _ ] => inverts H
   | [ H: TypeX _ (XApp _ _) _     |- _ ] => inverts H
   end).


(* Closed expressions are well typed under an empty environment. *)
Definition Closed (xx: exp) : Prop := 
 exists t, TypeX nil xx t.


Lemma TypeS_EnvZip
 :  forall te sx st
 ,  TypeS  te sx st
 -> exists sxt, Zipped sx st sxt.
Proof.
 intros te sx st HTS.
 induction HTS.
 - exists (@nil (bind (exp * ty))).
   eapply ZippedNil.

 - destruct IHHTS as [sxt].
   exists (sxt :> (n, (x, t))).
   eapply ZippedCons.
   assumption.
Qed.


Lemma TypeS_app
 :  forall te sx1 sx2 st1 st2
 ,  TypeS  te sx1 st1
 -> TypeS  te sx2 st2
 -> TypeS  te (sx1 >< sx2) (st1 >< st2).
Proof.
 intros te sx1 sx2 st1 st2 HT1 HT2.
 induction HT2; simpl; rip.
Qed.


Lemma TypeS_lookup_TypeX
 :  forall te sx st w x t
 ,  TypeS  te sx st
 -> lookup w  sx = Some x
 -> lookup w  st = Some t
 -> TypeX  te x t.
Proof.
 intros te sx st w x t HTS HLX HLT.
 induction HTS; intros.
 - nope.
 - simpl in *.
   remember (w n) as W.
   destruct W.
   + subst. inverts HLX. inverts HLT.
     assumption.
   + firstorder.
Qed.


Lemma TypeS_Forall 
 :  forall te1 te2 sx st f
 ,  Forall (  fun x => forall t
           ,  TypeX te1  x    t
           -> TypeX te2 (f x) t)
           (map expOfBind sx)
 -> TypeS te1 sx st
 -> TypeS te2 (map (mapExpOfBind f) sx) st.
Proof.
 intros.
 induction H0.
 - eapply TsNil.
 - simpl. eapply TsCons.
   + eapply IHTypeS.
     inverts H. auto.
   + inverts H. auto.
Qed.

