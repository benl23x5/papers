
Require Export Iron.Data.Bool.
Require Export Iron.Data.List.
Require Export Iron.Data.Nat.
Require Export Iron.Norm.List.
Require Export Iron.Norm.
Require Export Iron.Tactics.Rip.
Require Export Iron.Tactics.Rewrite.
Require Export Iron.Tactics.Case.
Require Export Iron.Tactics.Nope.
Require Export Iron.Tactics.Break.
Require Export Iron.Tactics.Short.
Require Export Iron.Tactics.Have.
Require Export Iron.Tactics.Exists.
Require Export Iron.Tactics.LibTactics.
Require Export Coq.Arith.Arith.
Require Export Coq.Arith.Compare_dec.
Require Export Coq.Logic.FunctionalExtensionality.


(* Comparisons on nats are common when using deBruijn indices.
   Automatically try the omega tactic when we see one. *)
Hint Extern 4 (_ >  _) => omega.
Hint Extern 4 (_ <  _) => omega.
Hint Extern 4 (_ >= _) => omega.
Hint Extern 4 (_ <= _) => omega.


(* The norm_beq_nat tactic normalises this,
   so we never want to unfold it. *)
Global Opaque beq_nat.


(* Primitive normalisations. *)
Tactic Notation "norm1"
 := first
    [ split_dec 
    | split_if 
    | split_match
    | norm_negb
    | norm_andb
    | norm_orb
    | norm_nat
    | norm_nat_compare
    | norm_beq_nat
    | norm_lists
    | norm_inverts_option].

Tactic Notation "norm"
 := repeat (rip; try norm1);
    autorewrite with global in *.

Tactic Notation "snorm"
 := repeat (rip; simpl in *; try norm1);
    autorewrite with global in *.


Tactic Notation "burn0"
 := snorm; eauto; nope.

Tactic Notation "burn0" "using" tactic(T)
 := snorm; eauto using T; nope.


Tactic Notation "burn"
 := try (solve [ burn0
               | red; burn0
               | repeat f_equal; burn0 ]).

Tactic Notation "burn" "using" tactic(T) 
 := try (solve [ burn0 using T
               | red; burn0 using T
               | repeat f_equal; burn0 using T ]).


(* TODO: shift this to tatics lib *)
Tactic Notation "rgwrite" constr(xx)
 := let H2 := fresh
    in  assert xx as H2 by have_auto; rewrite H2; clear H2.

Tactic Notation "rgwrite" constr(xx) "by" tactic(T)
 := let H := fresh 
    in  assert xx as H  by T; rewrite H;  clear H.


Ltac have_auto ::= burn.
