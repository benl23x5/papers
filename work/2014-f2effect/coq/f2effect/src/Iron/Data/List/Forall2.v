
Require Import Iron.Data.List.Base.
Require Import Iron.Data.List.Replace.
Require Import Iron.Tactics.


(********************************************************************)
(* Lemmas: Forall2 *)

Lemma Forall2_impl
 : forall {A B}
          (R1: A -> B -> Prop)
          (R2: A -> B -> Prop)
          xs ys
 , (forall x y, R1 x y -> R2 x y)
 -> Forall2 R1 xs ys 
 -> Forall2 R2 xs ys.
Proof.
 intros. induction H0; auto. 
Qed.


Lemma Forall2_impl_in
 : forall {A B}
          (R1: A -> B -> Prop)
          (R2: A -> B -> Prop)
          xs ys
 ,  (forall x y, In x xs -> In y ys -> R1 x y -> R2 x y)
 -> Forall2 R1 xs ys
 -> Forall2 R2 xs ys.
Proof.
 intros.
 induction H0.
  apply Forall2_nil.
  intuition.
Qed.


Lemma Forall2_eq
 :  forall {A} xs ys
 ,  Forall2 (@eq A) xs ys
 -> xs = ys.
Proof.
 intros.
 induction H.
 auto. 
 rewrite IHForall2.
 rewrite H. auto.
Qed.


Lemma Forall2_length
 : forall {A B} (R: A -> B -> Prop) 
          (xs : list A) 
          (ys : list B)
 ,  Forall2 R xs ys
 -> length xs = length ys.
Proof.
 intros.
 induction H.
  auto.
  simpl. auto.
Qed.
Hint Resolve Forall2_length. 


Lemma Forall2_get_get_left
 :  forall {A B} (R: A -> B -> Prop) x xs ys ix
 ,  Forall2 R xs ys
 -> get ix xs = Some x
 -> (exists y, get ix ys = Some y).
Proof.
 intros. gen ix x.
 induction H; intros. 
  false.
  destruct ix.
   simpl in H1. simpl. eauto.
   simpl in H1. simpl. eauto.
Qed.


Lemma Forall2_get_get_right
 :  forall {A B} (R: A -> B -> Prop) y xs ys ix
 ,  Forall2 R xs ys
 -> get ix ys = Some y
 -> (exists x, get ix xs = Some x).
Proof.
 intros. gen ix y.
 induction H; intros. 
  false.
  destruct ix.
   simpl in H1. simpl. eauto.
   simpl in H1. simpl. eauto.
Qed.


Lemma Forall2_exists_in_left
 :  forall {A B} (R: A -> B -> Prop) x xs ys
 ,  In x xs
 -> Forall2 R xs ys
 -> (exists y, In y ys).
Proof.
 intros.
 induction H0.
  false.
  exists y. simpl. auto.
Qed.


Lemma Forall2_exists_in_right
 :  forall {A B} (R: A -> B -> Prop) y xs ys
 ,  In y ys
 -> Forall2 R xs ys
 -> (exists x, In x xs).
Proof.
 intros.
 induction H0.
  false.
  exists x. simpl. auto.
Qed.


Lemma Forall2_exists_left
 : forall {A B} (R: A -> B -> Prop) x xs ys
 ,  In x xs 
 -> Forall2 R xs ys 
 -> (exists y, R x y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst. eauto.
   eapply IHForall2. eauto.
Qed.
Hint Resolve Forall2_exists_left.


Lemma Forall2_exists_left_in
 : forall {A B} (R: A -> B -> Prop) x xs ys
 ,             In x xs  -> Forall2 R xs ys 
 -> (exists y, In y ys  /\         R x  y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst.
   exists y. split. simpl. auto. auto.
   lets D: IHForall2 H.
   destruct D.
   exists x1.
    inverts H2.
    split. simpl. auto. auto.
Qed.


Lemma Forall2_exists_right
 : forall {A B} (R: A -> B -> Prop)
     y xs ys
 ,  In y ys 
 -> Forall2 R xs ys 
 -> (exists x, R x y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst. eauto.
   eapply IHForall2. eauto.
Qed.
Hint Resolve Forall2_exists_right.


Lemma Forall2_exists_left_all
 :  forall {A B}
      (P: B -> Prop)
      (R: A -> B -> Prop)
      ys
 ,  (forall y, P y -> (exists x, R x y))
 -> Forall  P ys
 -> (exists xs, Forall2 R xs ys).
Proof.
 intros.
 induction ys.
  exists (@nil A). auto.
  inverts H0.
   spec IHys H4.
   dest xs.
   have (exists x, R x a).
   dest x.
   exists (xs :> x). eauto.
Qed.
Hint Resolve Forall2_exists_left_all.


Lemma Forall2_exists_right_all
 :  forall {A B}
     (P: A -> Prop)
     (R: A -> B -> Prop)
     xs
 ,  (forall x, P x -> (exists y, R x y))
 -> Forall  P xs
 -> (exists ys, Forall2 R xs ys).
Proof.
 intros.
 induction xs.
  exists (@nil B). auto.
  inverts H0.
   spec IHxs H4.
   dest ys.
   have (exists y, R a y).
   dest y.
   exists (ys :> y). eauto.
Qed.
Hint Resolve Forall2_exists_right_all.


Lemma Forall2_exists_left_from_right
 :  forall {A B} (R: A -> B-> Prop)
 ,  (forall y,  exists x,  R x y)
 -> (forall ys, exists xs, Forall2 R xs ys).
Proof.
 intros.
 induction ys. 
  eauto.
  dest xs.
  spec H a.
  dest x.
  exists (xs :> x).
  eauto.
Qed.
Hint Resolve Forall2_exists_left_from_right.



Lemma Forall2_map
 : forall {A B C D}
     (R1: B -> D -> Prop)
     (f:  A -> B) (g:  C -> D)
     xs ys
 ,  Forall2 (fun x y => R1 (f x) (g y)) xs ys
 -> Forall2 R1 (map f xs) (map g ys).
Proof.
 intros.
 induction H.
  apply Forall2_nil.
  burn.
Qed.


Lemma Forall2_map'
 : forall {A B C D}
     (R1: B -> D -> Prop)
     (f:  A -> B) (g:  C -> D)
     xs ys
 ,  Forall2 R1 (map f xs) (map g ys)
 -> Forall2 (fun x y => R1 (f x) (g y)) xs ys.
Proof.
 intros. gen ys.
 induction xs; intros.
  induction ys; intros.
   auto.
   nope.
   destruct ys.
    nope.
    simpl in H.
    inverts H. eauto.
Qed.


Lemma Forall2_map_left
 : forall {A B C}
     (R1: B -> C -> Prop)
     (f:  A -> B)
     xs ys
 ,  Forall2 (fun x y => R1 (f x) y) xs ys
 -> Forall2 R1 (map f xs) ys.
Proof.
 intros.
 induction H.
  apply Forall2_nil.
  burn.
Qed.


Lemma Forall2_map_left'
 : forall {A B C}
     (R1: B -> C -> Prop)
     (f:  A -> B)
     xs ys
 ,  Forall2 R1 (map f xs) ys
 -> Forall2 (fun x y => R1 (f x) y) xs ys.
Proof.
 intros. gen ys.
 induction xs; intros.
  destruct ys.
   auto.
   simpl in H. nope.
  destruct ys.
   simpl in H. nope.
   simpl in H. inverts H.
   apply Forall2_cons. auto. auto.
Qed.


Lemma Forall2_map_right
 : forall {A B C}
     (R1: A -> C -> Prop)
     (f:  B -> C)
     xs ys
 ,  Forall2 (fun x y => R1 x (f y)) xs ys
 -> Forall2 R1 xs (map f ys).
Proof.
 intros.
 induction H.
  apply Forall2_nil.
  burn.
Qed.


Lemma Forall2_map_right'
 : forall {A B C}
     (R1: A -> C -> Prop)
     (f:  B -> C)
     xs ys
 ,  Forall2 R1 xs (map f ys)
 -> Forall2 (fun x y => R1 x (f y)) xs ys.
Proof.
 intros. gen xs.
 induction ys; intros.
  destruct xs.
   auto.
   simpl in H. nope.
  destruct xs.
   simpl in H. nope.
   simpl in H. inverts H.
   apply Forall2_cons. auto. auto.
Qed.


Lemma Forall2_Forall_left
 : forall {A B}
     (R : A -> B -> Prop)
     (P : A -> Prop)
     xs ys
 ,  Forall  (fun x => forall y, R x y -> P x) xs
 -> Forall2 R xs ys
 -> Forall  P xs.
Proof.
 intros.
 rewrite Forall_forall.
 rewrite Forall_forall in H. 
 intros.
 lets D: @Forall2_exists_left H1 H0.
 destruct D. eauto. 
Qed.
Hint Resolve Forall2_Forall_left.


Lemma Forall2_swap
 :  forall {A B}
      (R : A -> B -> Prop)
      (y : B)  (ys: list B)
      x x' xs1 xs2
 ,  (forall y, R x y -> R x' y)
 -> Forall2 R (xs1 ++ x  :: xs2) ys
 -> Forall2 R (xs1 ++ x' :: xs2) ys.
Proof.
 intros.
 lets D: Forall2_app_inv_l H0.
  destruct D  as [ys1].
  destruct H1 as [ys2].
  inverts H1. inverts H3.
  apply Forall2_app.
   auto.
  destruct ys2 as [ys2 | y'].
   inverts H1.
   inverts H1. eauto.
Qed.


Lemma Forall2_snoc
 : forall {A B}
     (R   : A -> B -> Prop)
     x xs y ys
 ,  R x y
 -> Forall2 R xs ys
 -> Forall2 R (x <: xs) (y <: ys).
Proof.
 intros. induction H0; burn.
Qed.
Hint Resolve Forall2_snoc.


Lemma Forall2_get_get_same
 : forall {A B}
     (R   : A -> B -> Prop)
     i x xs y ys
 ,  Forall2 R xs ys
 -> get i xs = Some x
 -> get i ys = Some y
 -> R x y.
Proof.
 intros. gen i.
 induction H; intros.
  false.
  destruct i.
   simpl in H1. inverts H1.
   simpl in H2. inverts H2. 
   auto.

   simpl in H1.
   simpl in H2.
   eauto.
Qed.


Lemma Forall2_get_left_for_right
 :  forall {A B}
     (R   : A -> B -> Prop)
     (f   : B -> A)
     i y ys
 ,  Forall2 R (map f ys) ys
 -> get i ys         = Some y
 -> get i (map f ys) = Some (f y).
Proof.
 intros. gen i y. 
 induction ys; intros.
  nope.
  destruct i.
   simpl in H0. inverts H0. simpl. auto.
   simpl in H0.
   simpl. inverts H. eauto.
Qed.
Hint Resolve Forall2_get_left_for_right.


Lemma Forall2_construct_left
 : forall {A B}
     (R   : A -> B -> Prop)
     (f   : B -> A)
     ys
 ,  (forall b, R (f b) b)
 -> (exists (xs : list A), Forall2 R xs ys).
Proof.
 intros.
 induction ys.
  eauto.
  destruct IHys as [xs].
  exists (xs :> f a).
  eauto.
Qed.     
Hint Resolve Forall2_construct_left.


Lemma Forall2_construct_right
 : forall {A B}
     (R : A -> B -> Prop)
     (f : A -> B)
     xs
 ,  (forall a, R a (f a))
 -> (exists (ys : list B), Forall2 R xs ys).
Proof.
 intros.
 induction xs.
  eauto.
  destruct IHxs as [ys].
  exists (ys :> f a).
  eauto.
Qed.
Hint Resolve Forall2_construct_right.


Lemma Forall2_replace
 : forall {A B} i x xs y ys
     (R : A -> B -> Prop)
     (f : A -> B)
 ,  Forall2 R xs ys
 -> R x y
 -> Forall2 R (replace i x xs) (replace i y ys).
Proof.
 intros. gen i.
 induction H; intros.
  repeat (rewrite replace_nil).
   auto.
  destruct i.
   simpl. eauto.
   simpl.
   eapply Forall2_cons.
    auto.
    spec IHForall2 i. auto.
Qed.

