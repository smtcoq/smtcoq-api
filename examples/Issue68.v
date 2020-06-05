Require Import ZArith.


(* Import the SMTCoq-API Library *)
Add Rec LoadPath "../src" as SMTCoqApi.
Require Import SMTCoqApi.


(* Take your representation of expressions. Note that, for integers,
   only Z is currently supported. *)
Definition var := nat.
Inductive Exp : Type -> Type :=
  Exp_Var     : var -> Exp Z
| Exp_Z       : Z -> Exp Z
| Exp_EqbZ    : Exp Z -> Exp Z -> Exp bool
| Exp_Andb    : Exp bool -> Exp bool -> Exp bool.


(* You only have to write a translation function from your expressions
   into a high-level representation of FOL. This representation is
   defined in ../src/SMTLib.v. *)
Fixpoint Exp2SMTLIB {A:Type} (e:Exp A) : term :=
  match e with
  | Exp_Var v => Term_Fun (v, (nil, Sort_Int)) nil
  | Exp_Z z => Term_Int z
  | Exp_EqbZ e1 e2 => Term_Eq (Exp2SMTLIB e1) (Exp2SMTLIB e2)
  | Exp_Andb e1 e2 => Term_And (Exp2SMTLIB e1) (Exp2SMTLIB e2)
  end.


(* Now, take your expression *)
Definition exp1 := Exp_EqbZ (Exp_Var 0) (Exp_Z 233).

(* You translate it and normalize the result *)
Definition smt1 := Eval compute in (Exp2SMTLIB exp1).
Print smt1.


(* This command outputs the satisfiability of the expression in the
   given SMT-LIB2 file *)
Generate_SMT smt1 "/tmp/ex1.smt2".


(* We can proceed similarly for the second example *)
Definition exp2 := Exp_Andb (Exp_EqbZ (Exp_Var 0) (Exp_Z 233))
                            (Exp_EqbZ (Exp_Var 0) (Exp_Z 2333)).

Definition smt2 := Eval compute in (Exp2SMTLIB exp2).
Generate_SMT smt2 "/tmp/ex2.smt2".
