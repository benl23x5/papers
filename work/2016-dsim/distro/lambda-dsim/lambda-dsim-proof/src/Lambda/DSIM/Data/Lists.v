Require Export Coq.Lists.List.
Require Export Lambda.DSIM.Tactics.Chargueraud.


(********************************************************************)
(* Given a list of pairs of keys and values, lookup the value
   that corresponds to the given key. *)
Fixpoint lookup {A B: Type} 
  (want: A -> bool) (xx: list (A * B)) : option B :=
 match xx with
 |  nil                
 => None

 |  (a, b) :: xs
 => match want a with
    | true  => Some b
    | false => lookup want xs
    end
 end.


Lemma lookup_some_app
 :  forall A B (env1 env2: list (A * B)) w b
 ,  lookup w (env1 ++ env2) = Some b
 -> lookup w  env1          = Some b
 \/ lookup w  env2          = Some b.
Proof.
 intros. gen b.
 induction env1; intros.
 - simpl in *. firstorder.
 - simpl in *.
   destruct a.
   remember (w a) as X.
   destruct X.
   + firstorder.
   + firstorder.
Qed.


Lemma lookup_app_infront
 :  forall A B (env1 env2: list (A * B)) w x1 x2
 ,  lookup w env1          = Some x2
 -> lookup w(env1 ++ env2) = Some x1
 -> x1 = x2.
Proof.
 intros A B env1 env2 w x1 x2 HL1 HL2.
 induction env1.
 - simpl in *. congruence.
 - simpl in *. destruct a.
   remember (w a) as X.
   destruct X.
   + inverts HL1. congruence.
   + firstorder. 
Qed.


Lemma lookup_app_inback
 :  forall A B (env1 env2: list (A * B)) w x1
 ,  lookup w  env1          = None
 -> lookup w (env1 ++ env2) = Some x1
 -> lookup w  env2          = Some x1.
Proof.
 intros A B env1 env2 w x1 HL1 HL12.
 induction env1.
 - simpl in *. assumption.
 - simpl in *. destruct a.
   remember (w a) as X.
   destruct X.
   + congruence.
   + firstorder.
Qed.



(********************************************************************)
(* Given two lists of pairs where the elements of each list aggree
   on the first component of each pair, we can zip the lists 
   where the result contains all the elements from both lists. *)

Inductive Zipped {X A B} 
  : list (X * A) -> list (X * B) -> list (X * (A * B))
  -> Prop :=

 | ZippedNil 
   :  Zipped nil nil nil

 | ZippedCons
   :  forall x a (aa : list (X * A)) b (bb : list (X * B)) ab
   ,  Zipped aa bb ab
   -> Zipped ((x, a) :: aa) ((x, b) :: bb) ((x, (a, b)) :: ab).


(* If we have two zipped lists, 
   and can lookup and element from the first list,
   then there is a corresponding element in the second list. *)
Lemma Zipped_some_1of2
 :  forall {X A B} 
      (aa: list (X * A)) 
      (bb: list (X * B))
      (ab: list (X * (A * B)))
      w b
 ,  Zipped aa bb ab
 -> lookup w bb = Some b
 -> exists a, 
    lookup w aa = Some a.
Proof.
 intros.
 induction H.
 - simpl in H0.
   congruence.
 - simpl in *.
   remember (w x) as W.
   destruct W.
   + inverts H0.
     exists a. trivial.
   + specializes IHZipped H0.
     trivial.
Qed.


(* If we have two zipped lists,
   and can lookup and element from the second list,
   then there is a corresponding element in the second list. *)
Lemma EnvZip_some_2of1
 :  forall {X A B}
      (aa: list (X * A))
      (bb: list (X * B))
      (ab: list (X * (A * B)))
      w a
 ,  Zipped aa bb ab
 -> lookup w aa = Some a
 -> exists b, 
    lookup w bb = Some b.
Proof.
 intros.
 induction H.
 - simpl in H0.
   congruence.
 - simpl in *.
   remember (w x) as W.
   destruct W.
   + inverts H0.
     exists b. trivial.
   + specializes IHZipped H0.
     trivial.
Qed.


Lemma EnvZip_none_1of2
 :  forall {X A B} 
      (aa: list (X * A))
      (bb: list (X * B))
      ab w
 ,  Zipped aa bb ab
 -> lookup w aa = None
 -> lookup w bb = None.
Proof.
 intros X A B aa bb ab w HZ HL1.
 induction HZ.
 - simpl in *.
   congruence.
 - simpl in *.
   remember (w x) as W.
   destruct W.
   + congruence.
   + firstorder.
Qed.


Lemma EnvZip_none_2of1
 :  forall {X A B}
      (aa: list (X * A))
      (bb: list (X * B))
      ab w
 ,  Zipped aa bb ab
 -> lookup w aa = None
 -> lookup w bb = None.
Proof.
 intros X A B aa bb ab w HZ HL2.
 induction HZ.
 - simpl in *.
   congruence.
 - simpl in *.
   remember (w x) as W.
   destruct W.
   + congruence.
   + firstorder.
Qed.


(********************************************************************)
(* Forall Lemmas *)

Lemma Forall_inst
 :  forall {A B} a' xs (P: A -> B -> Prop)
 ,  Forall (fun x => forall a, P a x) xs
 -> Forall (fun x => P a' x) xs.
Proof.
 intros.
 induction xs.
 - auto.
 - eapply Forall_cons.
   + inverts H. eapply H2.
   + inverts H. eapply IHxs. assumption.
Qed.


Lemma Forall_mp
 :  forall {A} (P Q: A -> Prop)  xs
 ,  Forall (fun x => P x -> Q x) xs
 -> Forall (fun x => P x)        xs
 -> Forall (fun x => Q x)        xs.
Proof.
 intros.
 induction xs.
 - auto.
 - eapply Forall_cons.
   + inverts H. inverts H0. auto.
   + inverts H. inverts H0. auto.
Qed. 


Lemma Forall_mp_const 
 :  forall {A} P (Q: A -> Prop)  xs
 ,  P
 -> Forall (fun x => P -> Q x) xs
 -> Forall (fun x => Q x)      xs.
Proof.
 intros.
 induction H.
 - auto.
 - apply Forall_cons.
   + auto.
   + auto.
Qed.


Lemma Forall_map
 :  forall {A B}
    (P: B -> Prop) (f: A -> B) (xs: list A)
 ,  Forall (fun x => P (f x)) xs
 -> Forall P (map f xs).
Proof.
 intros. induction xs.
  apply Forall_nil.
  inverts H. simpl. intuition.
Qed.

