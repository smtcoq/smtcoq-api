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


Require Import SMTCoq.SMTCoq.
Require Import ZArith.


(* A high-level, simple syntax for SMT-LIB *)
(* TO BE EXTENDED *)
Section SMTLib.

  (* Uninterpreted sorts *)
  Local Notation sort_sym := nat.

  Inductive sort : Set :=
  | Sort_Bool
  | Sort_Int
  | Sort_Uninterpreted (_:sort_sym)
  .

  (* Uninterpreted functions. Remarks:
     - predicate symbols are function symbols of codomain Bool
     - variables are function symbols with arguments
   *)
  Local Notation fun_sym := (nat * ((list sort) * sort))%type.

  Inductive term : Set :=
  | Term_Fun : fun_sym -> list term -> term
  | Term_Int : Z -> term
  | Term_Geq : term -> term -> term
  | Term_Eq : term -> term -> term
  | Term_And : term -> term -> term
  .

End SMTLib.


(* Register constants for OCaml access *)
Register Sort_Bool as SMTCoqAPI.SMTLib.Sort_Bool.
Register Sort_Int as SMTCoqAPI.SMTLib.Sort_Int.
Register Sort_Uninterpreted as SMTCoqAPI.SMTLib.Sort_Uninterpreted.
Register Term_Fun as SMTCoqAPI.SMTLib.Term_Fun.
Register Term_Int as SMTCoqAPI.SMTLib.Term_Int.
Register Term_Geq as SMTCoqAPI.SMTLib.Term_Geq.
Register Term_Eq as SMTCoqAPI.SMTLib.Term_Eq.
Register Term_And as SMTCoqAPI.SMTLib.Term_And.
