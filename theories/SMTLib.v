(**************************************************************************)
(*                                                                        *)
(*     SMTCoq-Api                                                         *)
(*     Copyright (C) 2020 - 2022                                          *)
(*                                                                        *)
(*     Author: Chantal Keller - LMF, Université Paris-Saclay              *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

Require Import Lia.
Require Coq.Program.Wf.

Require Import SMTCoq.SMTCoq.
Require Import SMTCoq.bva.BVList.
Import BITVECTOR_LIST.
Require Import ZArith.

Import ListNotations.


(* A high-level, simple syntax for SMT-LIB *)
(* TO BE EXTENDED *)
Section SMTLib.

  (* Uninterpreted sorts *)
  Local Notation sort_sym := nat.

  Inductive sort : Set :=
  | Sort_Bool
  | Sort_Int
  (* SMTLIB says m > 0, but SMTCoq uses N in src/bva/BVList.v *)
  | Sort_BitVec (m : N)
  | Sort_Uninterpreted (_:sort_sym)
  .

  (* Uninterpreted functions. Remarks:
     - predicate symbols are function symbols of codomain Bool
     - variables are function symbols without arguments
   *)
  Local Notation fun_sym := (nat * ((list sort) * sort))%type.

  Variant BVUnaryOp : Set :=
    | BVNot
    | BVNeg
  .

  Variant BVBinOp : Set :=
    | BVAnd
    | BVOr
    | BVAdd
    | BVMul
    | BVUDiv
    | BVURem
    | BVShl
    | BVShr
  .

  Inductive term : Set :=
  | Term_Fun : fun_sym -> list term -> term
  | Term_Int : Z -> term
  | Term_Geq : term -> term -> term
  | Term_Eq : term -> term -> term
  | Term_And : term -> term -> term
  | Term_Or : term -> term -> term
  | Term_Not : term -> term
  | Term_ITE : term -> term -> term -> term
  | Term_True : term
  | Term_False : term
  | Term_BVLit : list bool -> term
  | Term_BVConcat : term -> term -> term
  | Term_BVExtract : nat -> nat -> term -> term
  | Term_BVUnaryOp : BVUnaryOp -> term -> term
  | Term_BVBinOp : BVBinOp -> term -> term -> term
  | Term_BVUlt : term -> term -> term
  .

  (* To be able to interpret function symbol application, we introduce
     an informative cast between sorts

     See
     https://www.lri.fr/~keller/Documents-recherche/Publications/thesis13.pdf
     Sec.3.2.2 *)
  Section Cast.
    Definition cast_result (A:Type) (n m:A) :=
      option (forall (P:A -> Type), P n -> P m).
    Definition idcast (A:Type) (n:A) : cast_result A n n :=
      Some (fun P x => x).
    Arguments idcast {A n}.

    Fixpoint nat_cast (n m:nat) : cast_result nat n m :=
      match n, m with
      | O, O => idcast
      | S n, S m =>
          match nat_cast n m with
          | Some k => Some (fun P => k (fun x => P (S x)))
          | None => None
          end
      | _, _ => None
      end.

    Lemma nat_cast_refl:
      forall n, nat_cast n n = idcast.
    Proof. induction n as [ |n IHn]; simpl; try rewrite IHn; auto. Qed.

    Fixpoint pos_cast (n m: positive) : cast_result positive n m :=
      match n, m with
      | xH, xH => idcast
      | xO n, xO m =>
          match pos_cast n m with
          | Some k => Some (fun P => k (fun x => P (xO x)))
          | None => None
          end
      | xI n, xI m =>
          match pos_cast n m with
          | Some k => Some (fun P => k (fun x => P (xI x)))
          | None => None
          end
      | _, _ => None
      end.

    Lemma pos_cast_refl:
      forall n, pos_cast n n = idcast.
    Proof. induction n; simpl; try rewrite IHn; auto. Qed.

    Fixpoint N_cast (n m: N) : cast_result N n m :=
      match n, m with
      | N0, N0 => idcast
      | Npos n, Npos m =>
          match pos_cast n m with
          | Some k => Some (fun P => k (fun x => P (Npos x)))
          | None => None
          end
      | _, _ => None
      end.

    Lemma N_cast_refl:
      forall n, N_cast n n = idcast.
    Proof. induction n; simpl; try rewrite pos_cast_refl; try rewrite IHn; auto. Qed.

    Definition cast (A B:sort) : cast_result sort A B :=
      match A, B return cast_result sort A B with
      | Sort_Bool, Sort_Bool => idcast
      | Sort_Int, Sort_Int => idcast
      | Sort_BitVec m1, Sort_BitVec m2 =>
          match N_cast m1 m2 with
          | Some k => Some (fun P => k (fun x => P (Sort_BitVec x)))
          | None => None
          end
      | Sort_Uninterpreted n1, Sort_Uninterpreted n2 =>
          match nat_cast n1 n2 with
          | Some k => Some (fun P => k (fun x => P (Sort_Uninterpreted x)))
          | None => None
          end
      | _, _ => None
      end.

    Lemma cast_refl:
      forall s, cast s s = idcast.
    Proof.
      destruct s as [ | |m|n]; simpl; auto.
      - now rewrite N_cast_refl.
      - now rewrite nat_cast_refl.
    Qed.
  End Cast.

  (* Interpretation *)
  Section Interpretation.

    (* Interpretation of sorts *)
    Variable interp_sort_sym : sort_sym -> Type.

    Definition interp_sort (s:sort) : Type :=
      match s with
      | Sort_Bool => bool
      | Sort_Int => Z
      | Sort_BitVec m => bitvector m
      | Sort_Uninterpreted sy => interp_sort_sym sy
      end.

    (* Interpretation of function types *)
    Fixpoint interp_fun_type (dom:list sort) (codom:sort) : Type :=
      match dom with
      | nil => interp_sort codom
      | s::dom => (interp_sort s) -> (interp_fun_type dom codom)
      end.

    (* Applying function symbols *)
    Fixpoint apply_fun (dom:list sort) (codom:sort) :
      (interp_fun_type dom codom) ->
      (list (option {A:sort & interp_sort A})) ->
      option (interp_sort codom) :=
      match dom return
            (interp_fun_type dom codom) ->
            (list (option {A:sort & interp_sort A})) ->
            option (interp_sort codom)
      with
      | nil => fun f _ => Some f
      | s::dom => fun f arg =>
                  match arg with
                  | (Some (existT _ s' a))::arg =>
                      match cast s' s with
                      | Some k => apply_fun dom codom (f (k _ a)) arg
                      | None => None
                      end
                  | _ => None
                  end
      end.

    (* TODO: This is probably wrong. *)
    Program Fixpoint bv2nat {m} (bv : bitvector m) {measure (nat_of_N m)} : nat :=
      match bits bv with
      | nil =>
          0
      | (b :: bs) =>
          (if b then 2 ^ (nat_of_N m) else 0) + bv2nat (of_bits bs)
      end
    .
    Next Obligation.
      destruct m.
      - destruct bv0; destruct bv0; simpl in *; discriminate.
      - destruct bv0; destruct bv0; simpl in *; try discriminate.
        unfold RAWBITVECTOR_LIST.size in *; simpl in *.
        inversion Heq_anonymous; subst.
        lia.
    Qed.

    (* TODO: This is probably wrong. *)
    Program Fixpoint nat2bv (n : nat) {measure n} : {m : N & bitvector m} :=
      match n with
      | 0 => existT _ 0%N (of_bits [])
      | _ =>
          let 'existT _ m head := nat2bv (n / 2) in
          (existT _ (m + 1)%N (bv_concat head (of_bits [n mod 2 =? 1])))
      end
    .
    Next Obligation.
      pose proof (Nat.divmod_spec n 1 0 1) as H.
      specialize (H ltac:(lia)).
      destruct (Nat.divmod n 1 0 1).
      simpl in *.
      lia.
    Qed.

    (* Interpretation of terms *)
    Variable interp_fun_sym :
      nat -> forall (dom:list sort) (codom:sort), interp_fun_type dom codom.

    Fixpoint interp_term (t:term) : option {A : sort & interp_sort A} :=
      match t with
      | Term_Fun (n, (dom, codom)) arg =>
          match apply_fun dom codom (interp_fun_sym n dom codom)
                  (List.map interp_term arg)
          with
          | Some i => Some (existT _ codom i)
          | None => None
          end
      | Term_Int z => Some (existT _ Sort_Int z)
      | Term_Geq t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ Sort_Int z1), Some (existT _ Sort_Int z2) =>
              Some (existT _ Sort_Bool (z1 >=? z2)%Z)
          | _, _ => None
          end
      | Term_Eq t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ Sort_Int z1), Some (existT _ Sort_Int z2) =>
              Some (existT _ Sort_Bool (z1 =? z2)%Z)
          | _, _ => None
          end
      | Term_And t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ Sort_Bool b1), Some (existT _ Sort_Bool b2) =>
              Some (existT _ Sort_Bool (b1 && b2)%bool)
          | _, _ => None
          end
      | Term_Or t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ Sort_Bool b1), Some (existT _ Sort_Bool b2) =>
              Some (existT _ Sort_Bool (b1 || b2)%bool)
          | _, _ => None
          end
      | Term_Not t =>
          match interp_term t with
          | Some (existT _ Sort_Bool b) =>
              Some (existT _ Sort_Bool (negb b))
          | _ => None
          end
      | Term_ITE t1 t2 t3 =>
          match interp_term t1, interp_term t2, interp_term t3 with
          | Some (existT _ Sort_Bool b1), Some v2, Some v3 =>
              if b1 then Some v2 else Some v3
          | _, _, _ => None
          end
      | Term_True => Some (existT _ Sort_Bool true)
      | Term_False => Some (existT _ Sort_Bool false)
      | Term_BVLit bits =>
          Some (existT _ (Sort_BitVec (N_of_nat (length bits))) (of_bits bits))
      | Term_BVConcat t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ (Sort_BitVec m1) bv1), Some (existT _ (Sort_BitVec m2) bv2) =>
              Some (existT _ (Sort_BitVec (m1 + m2)) (bv_concat bv1 bv2))
          | _, _ => None
          end
      | Term_BVExtract lo hi t =>
          match interp_term t with
          | Some (existT _ (Sort_BitVec m) bv) =>
              Some (existT _
                      (Sort_BitVec (N_of_nat (hi - lo + 1)))
                      (bv_extr (N_of_nat lo) _ bv))
          | _ => None
          end
      | Term_BVUnaryOp op t =>
          match interp_term t with
          | Some (existT _ (Sort_BitVec m) bv) =>
              match op with
              | BVNot => Some (existT _ (Sort_BitVec m) (bv_not bv))
              | BVNeg => Some (existT _ (Sort_BitVec m) (bv_neg bv))
              end
          | _ => None
          end
      | Term_BVBinOp binop t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ (Sort_BitVec m1) bv1),
            Some (existT _ (Sort_BitVec m2) bv2_2) =>
              match N.eq_dec m2 m1 with
              | left e =>
                  let bv2 : bitvector m1 := eq_rect m2 bitvector bv2_2 m1 e in
                  match binop with
                  | BVAnd => Some (existT _ (Sort_BitVec m1) (bv_and bv1 bv2))
                  | BVOr => Some (existT _ (Sort_BitVec m1) (bv_or bv1 bv2))
                  | BVAdd => Some (existT _ (Sort_BitVec m1) (bv_add bv1 bv2))
                  | BVMul => Some (existT _ (Sort_BitVec m1) (bv_mult bv1 bv2))
                  | BVUDiv =>
                      (* Divide does not exist in SMTCoq bitvectors *)
                      (* Some (existT _ (Sort_BitVec m1) (bv_udiv bv1 bv2)) *)
                      None
                  | BVURem =>
                      (* Divide does not exist in SMTCoq bitvectors *)
                      (* Some (existT _ (Sort_BitVec m1) (bv_rem bv1 bv2)) *)
                      None
                  | BVShl => Some (existT _ (Sort_BitVec m1) (bv_shl bv1 bv2))
                  | BVShr => Some (existT _ (Sort_BitVec m1) (bv_shr bv1 bv2))
                  end
              | right _ => None
              end
          | _, _ => None
          end
      | Term_BVUlt t1 t2 =>
          match interp_term t1, interp_term t2 with
          | Some (existT _ (Sort_BitVec m1) bv1),
            Some (existT _ (Sort_BitVec m2) bv2) =>
              if (m1 =? m2)%N
              then Some (existT _ Sort_Bool (bv2nat bv1 <=? bv2nat bv2))
              else None
          | _, _ => None
          end
      end.

    Definition interp_formula (t:term) : bool :=
      match interp_term t with
      | Some (existT _ Sort_Bool b) => b
      | _ => true
      end.

  End Interpretation.

  (* Default values for interpreted sorts *)
  Section Default.
    Variable interp_sort_sym : sort_sym -> Type.
    Variable interp_sort_sym_def : forall (sy:sort_sym), interp_sort_sym sy.

    Definition interp_sort_def (s:sort) : interp_sort interp_sort_sym s :=
      match s return interp_sort interp_sort_sym s with
      | Sort_Bool => true
      | Sort_Int => 0%Z
      | Sort_BitVec m => zeros m
      | Sort_Uninterpreted sy => interp_sort_sym_def sy
      end.

    Fixpoint interp_fun_type_def (dom:list sort) (codom:sort) :
      interp_fun_type interp_sort_sym dom codom :=
      match dom return interp_fun_type interp_sort_sym dom codom with
      | nil => interp_sort_def codom
      | _::dom => fun _ => interp_fun_type_def dom codom
      end.
  End Default.

End SMTLib.


(* Register constants for OCaml access *)
Register Sort_Bool as SMTCoqAPI.SMTLib.Sort_Bool.
Register Sort_Int as SMTCoqAPI.SMTLib.Sort_Int.
Register Sort_BitVec as SMTCoqAPI.SMTLib.Sort_BitVec.
Register Sort_Uninterpreted as SMTCoqAPI.SMTLib.Sort_Uninterpreted.

Register BVNot as SMTCoqAPI.SMTLib.BVNot.
Register BVNeg as SMTCoqAPI.SMTLib.BVNeg.

Register BVAnd as SMTCoqAPI.SMTLib.BVAnd.
Register BVOr as SMTCoqAPI.SMTLib.BVOr.
Register BVAdd as SMTCoqAPI.SMTLib.BVAdd.
Register BVMul as SMTCoqAPI.SMTLib.BVMul.
Register BVUDiv as SMTCoqAPI.SMTLib.BVUDiv.
Register BVURem as SMTCoqAPI.SMTLib.BVURem.
Register BVShl as SMTCoqAPI.SMTLib.BVShl.
Register BVShr as SMTCoqAPI.SMTLib.BVShr.

Register Term_Fun as SMTCoqAPI.SMTLib.Term_Fun.
Register Term_Int as SMTCoqAPI.SMTLib.Term_Int.
Register Term_Geq as SMTCoqAPI.SMTLib.Term_Geq.
Register Term_Eq as SMTCoqAPI.SMTLib.Term_Eq.
Register Term_And as SMTCoqAPI.SMTLib.Term_And.
Register Term_Or as SMTCoqAPI.SMTLib.Term_Or.
Register Term_Not as SMTCoqAPI.SMTLib.Term_Not.
Register Term_ITE as SMTCoqAPI.SMTLib.Term_ITE.
Register Term_True as SMTCoqAPI.SMTLib.Term_True.
Register Term_False as SMTCoqAPI.SMTLib.Term_False.
Register Term_BVLit as SMTCoqAPI.SMTLib.Term_BVLit.
Register Term_BVConcat as SMTCoqAPI.SMTLib.Term_BVConcat.
Register Term_BVExtract as SMTCoqAPI.SMTLib.Term_BVExtract.
Register Term_BVUnaryOp as SMTCoqAPI.SMTLib.Term_BVUnaryOp.
Register Term_BVBinOp as SMTCoqAPI.SMTLib.Term_BVBinOp.
Register Term_BVUlt as SMTCoqAPI.SMTLib.Term_BVUlt.
