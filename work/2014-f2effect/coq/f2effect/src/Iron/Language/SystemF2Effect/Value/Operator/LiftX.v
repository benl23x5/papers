
Require Export Iron.Language.SystemF2Effect.Type.
Require Export Iron.Language.SystemF2Effect.Value.Exp.


(********************************************************************)
(* Lift type indices in expressions. *)
Fixpoint liftTV (d: nat) (vv: val) : val :=
  match vv with
  |  VVar _            => vv
  |  VLoc _            => vv
  |  VLam t x          => VLam     (liftTT 1 d t) (liftTX d x)
  |  VLAM k x          => VLAM k   (liftTX (S d) x)
  |  VConst c          => vv
  end
 with liftTX (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVal v            => XVal     (liftTV d v)
  |  XLet t x1 x2      => XLet     (liftTT 1 d t)  (liftTX d x1) (liftTX d x2)
  |  XApp x1 x2        => XApp     (liftTV d x1)   (liftTV d x2)
  |  XAPP x t          => XAPP     (liftTV d x)    (liftTT 1 d t)

  |  XOp1   op1 v      => XOp1     op1 (liftTV d v)

  |  XPrivate x        => XPrivate (liftTX (S d) x)
  |  XExtend  tR x     => XExtend  (liftTT 1 d tR) (liftTX (S d) x)
  |  XAlloc   tR v     => XAlloc   (liftTT 1 d tR) (liftTV d v)
  |  XRead    tR v     => XRead    (liftTT 1 d tR) (liftTV d v)
  |  XWrite   tR v1 v2 => XWrite   (liftTT 1 d tR) (liftTV d v1) (liftTV d v2)
 end.


(********************************************************************)
(* Lift expression indices in expressions *)
Fixpoint liftXV (n: nat) (d: nat) (vv: val) {struct vv} : val :=
  match vv with
  | VVar ix    
  => if le_gt_dec d ix
        (* index was pointing into env, lift it across new elems *)
        then VVar (ix + n)
        (* index was locally bound, leave it be *)
        else vv

  | VLoc l            => VLoc   l
  | VLam t1 x1        => VLam   t1 (liftXX n (S d) x1)
  | VLAM k x          => VLAM   k (liftXX n d x)
  | VConst c          => VConst c
  end
 with   liftXX (n: nat) (d: nat) (xx: exp) {struct xx} : exp :=
  match xx with 
  | XVal v            => XVal     (liftXV n d v)
  | XLet t x1 x2      => XLet t   (liftXX n d x1) (liftXX n (S d) x2)
  | XApp v1 v2        => XApp     (liftXV n d v1) (liftXV n d v2)
  | XAPP v1 t2        => XAPP     (liftXV n d v1) t2

  | XOp1   op1 v      => XOp1     op1 (liftXV n d v)

  | XPrivate x        => XPrivate    (liftXX n d x)
  | XExtend  tR x     => XExtend  tR (liftXX n d x)
  | XAlloc   tR v     => XAlloc   tR (liftXV n d v)
  | XRead    tR v     => XRead    tR (liftXV n d v)
  | XWrite   tR v1 v2 => XWrite   tR (liftXV n d v1) (liftXV n d v2)
  end.


(********************************************************************)
(* When we lift an expression by zero places,
   then the expression is unchanged. *)
Lemma liftXX_zero
 : forall d x
 , liftXX 0 d x = x.
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PV := fun v => forall d
      ,  liftXV 0 d v = v);
  snorm; f_equal; try omega; burn.
Qed.
Hint Rewrite liftXX_zero : global.


(* Commutivity of lifting. *)
Lemma liftXX_comm
 : forall n m x d
 , liftXX n d (liftXX m d x)
 = liftXX m d (liftXX n d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PV := fun v => forall d
      ,  liftXV n d (liftXV m d v)
      =  liftXV m d (liftXV n d v)); 
  snorm; f_equal; try omega; burn.
Qed.


(* When consecutively lifting an expression, we can lift by one
   more place in the first lifting and but one less in the second. *)
Lemma liftXX_succ
 : forall n m d x
 , liftXX (S n) d (liftXX m     d x)
 = liftXX n     d (liftXX (S m) d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PV := fun v => forall d
      ,  liftXV (S n) d (liftXV  m    d v)
      =  liftXV n     d (liftXV (S m) d v));
  snorm; f_equal; try omega; burn.
Qed.
Hint Rewrite liftXX_succ : global.


(* We can collapse two consecutive lifting expressions by lifting 
   just once by the sum of the places, provided the lifting
   occurs at depth zero. *) 
Lemma liftXX_plus 
 : forall n m x 
 , liftXX n 0 (liftXX m 0 x) = liftXX (n + m) 0 x.
Proof.
 intros. gen n.
 induction m; intros.
  burn.

  intros.
  rrwrite (n + S m = S n + m). 
  rewrite liftXX_comm.
  rewrite <- IHm.
  rewrite liftXX_comm.
  burn.
Qed.

