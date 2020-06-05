Require Import ZArith.


(* Import the SMTCoq-API Library *)
Add Rec LoadPath "../src" as SMTCoqApi.
Require Import SMTCoqApi.


(* Take your representation of expressions. Note that, for integers,
   only Z is currently supported. See below for a way to use natural
   numbers. *)
Definition var := nat.
Inductive ExpZ : Type -> Type :=
  ExpZ_Var     : var -> ExpZ Z
| ExpZ_Z       : Z -> ExpZ Z
| ExpZ_EqbZ    : ExpZ Z -> ExpZ Z -> ExpZ bool
| ExpZ_Andb    : ExpZ bool -> ExpZ bool -> ExpZ bool.


(* You only have to write a translation function from your expressions
   into a high-level representation of FOL. This representation is
   defined in ../src/SMTLib.v. *)
Fixpoint ExpZ2SMTLIB {A:Type} (e:ExpZ A) : term :=
  match e with
  | ExpZ_Var v => Term_Fun (v, (nil, Sort_Int)) nil
  | ExpZ_Z z => Term_Int z
  | ExpZ_EqbZ e1 e2 => Term_Eq (ExpZ2SMTLIB e1) (ExpZ2SMTLIB e2)
  | ExpZ_Andb e1 e2 => Term_And (ExpZ2SMTLIB e1) (ExpZ2SMTLIB e2)
  end.


(* Now, take your expression *)
Definition exp1 := ExpZ_EqbZ (ExpZ_Var 0) (ExpZ_Z 233).

(* You translate it and normalize the result *)
Definition smt1 := Eval compute in (ExpZ2SMTLIB exp1).
Print smt1.


(* This command outputs the satisfiability of the expression in the
   given SMT-LIB2 file *)
Generate_SMT smt1 "/tmp/ex1.smt2".


(* We can proceed similarly for the second example *)
Definition exp2 := ExpZ_Andb (ExpZ_EqbZ (ExpZ_Var 0) (ExpZ_Z 233))
                            (ExpZ_EqbZ (ExpZ_Var 0) (ExpZ_Z 2333)).

Definition smt2 := Eval compute in (ExpZ2SMTLIB exp2).
Generate_SMT smt2 "/tmp/ex2.smt2".


(* Here is a way to use natural numbers *)
Inductive Exp : Type -> Type :=
  Exp_Var     : var -> Exp nat
| Exp_Nat     : nat -> Exp nat
| Exp_EqbNat  : Exp nat -> Exp nat -> Exp bool
| Exp_Andb    : Exp bool -> Exp bool -> Exp bool.


(* It relies on the same translation function, but:
   - constants are injected into Z
   - it also adds a constraint for each variable of type nat
 *)
Definition encode_var v := Term_Fun (v, (nil, Sort_Int)) nil.

Fixpoint Exp2SMTLIB_core {A:Type} (e:Exp A) : term :=
  match e with
  | Exp_Var v => encode_var v
  | Exp_Nat n => Term_Int (Z.of_nat n)
  | Exp_EqbNat e1 e2 => Term_Eq (Exp2SMTLIB_core e1) (Exp2SMTLIB_core e2)
  | Exp_Andb e1 e2 => Term_And (Exp2SMTLIB_core e1) (Exp2SMTLIB_core e2)
  end.

Fixpoint get_nat_vars {A:Type} (e:Exp A) (acc:list nat) : list nat :=
  match e with
  | Exp_Var v => if List.existsb (Nat.eqb v) acc then acc else v::acc
  | Exp_Nat n => acc
  | Exp_EqbNat e1 e2 => get_nat_vars e2 (get_nat_vars e1 acc)
  | Exp_Andb e1 e2 => get_nat_vars e2 (get_nat_vars e1 acc)
  end.

Fixpoint nat_constraints (vars:list nat) (acc:term) : term :=
  match vars with
  | nil => acc
  | v::vars => nat_constraints vars (Term_And (Term_Geq (encode_var v) (Term_Int 0)) acc)
  end.

Definition Exp2SMTLIB {A:Type} (e:Exp A) : term :=
  let vars := get_nat_vars e nil in
  nat_constraints vars (Exp2SMTLIB_core e).


(* We can look at the two examples *)
Definition exp3 := Exp_EqbNat (Exp_Var 0) (Exp_Nat 233).
Definition smt3 := Eval compute in (Exp2SMTLIB exp3).
Generate_SMT smt3 "/tmp/ex3.smt2".

Definition exp4 := Exp_Andb (Exp_EqbNat (Exp_Var 0) (Exp_Nat 233))
                            (Exp_EqbNat (Exp_Var 0) (Exp_Nat 2333)).
Definition smt4 := Eval compute in (Exp2SMTLIB exp4).
Generate_SMT smt4 "/tmp/ex4.smt2".
