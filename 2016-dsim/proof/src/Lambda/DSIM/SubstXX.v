
Require Export Lambda.DSIM.TypeX.


(* Apply an expression substitution to an expression. *)
Fixpoint substXX (sx: env exp) (xx: exp) {struct xx} : exp  :=
 match xx with
 | XVar n
 => match lookup (eqName n) sx with
    | None   => xx
    | Some x => x
    end

 |  XAbs sx2 n t x
 => XAbs (sx >< map (mapExpOfBind (substXX sx)) sx2) n t x

 |  XApp x1 x2
 => XApp (substXX sx x1) (substXX sx x2)
 end.


(* Substitution of expressions in expressions preserves typing. *)
Lemma subst_exp_exp
 :  forall te sx st x t
 ,  TypeS  te sx st
 -> TypeX (te >< st) x       t
 -> TypeX  te (substXX sx x) t.
Proof.
 intros. gen te sx st t.
 induction x using exp_iind;
  intros; inverts_type.

 - Case "XVar".
   assert (exists sxt, Zipped sx st sxt) as HZ.
    eapply TypeS_EnvZip; eauto.
   destruct HZ as [sxt].

   simpl in *.
   remember (lookup (eqName n) sx) as o.
   symmetry in Heqo.
   destruct o.

   + SCase "variable matches".
     assert (exists t2, lookup (eqName n) st = Some t2) as HL2.
      eapply EnvZip_some_2of1; eauto.
     destruct HL2 as [t2].

     assert (t = t2).
      eapply lookup_app_infront; eauto. 
     subst.

     eapply TypeS_lookup_TypeX; eauto.

   + SCase "variable does not match.".
     eapply TxVar.
     assert (lookup (eqName n) st = None)   as HL1.
      eapply EnvZip_none_1of2; eauto.

     assert (lookup (eqName n) te = Some t) as HL2.
      eapply lookup_app_inback; eauto.
     assumption.

 - Case "XLam".
   eapply TxLam.
   + eapply (Forall_inst te)  in H.
     eapply (Forall_inst sx)  in H.
     eapply (Forall_inst st)  in H.
     eapply (Forall_mp_const) in H; eauto.

     eapply (Forall_map 
              (  fun x => forall t
              ,  TypeX (te >< st) x t
              -> TypeX te (substXX sx x) t)) in H.

     eapply TypeS_app; eauto.
     eapply TypeS_Forall. eauto. eauto.

   + rewrite <- app_assoc.
     assumption.

 - Case "XApp".
   eapply TxApp; eauto.
Qed.

