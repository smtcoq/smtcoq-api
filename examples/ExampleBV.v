(* Import the SMTCoq-API Library *)
Require Import SMTCoqApi.


(* Expressions that are manipulated *)
Inductive Exp : Type :=
| ExpBV0 : nat -> Exp            (* The bitvector with n zeros *)
| ExpEq : Exp -> Exp -> Exp.   (* Equality between bitvectors *)


(* Translation function *)
Fixpoint Exp2SMTLIB (e:Exp) : term :=
  match e with
  | ExpBV0 n => Term_BVLit (List.repeat false n)
  | ExpEq e1 e2 => Term_Eq (Exp2SMTLIB e1) (Exp2SMTLIB e2)
  end.


(* Now, take an expression *)
Definition exp1 := ExpEq (ExpBV0 4) (ExpBV0 4).

(* You translate it and normalize the result *)
Definition smt1 := Eval compute in (Exp2SMTLIB exp1).


(* This command outputs the satisfiability of the expression in the
   given SMT-LIB2 file *)
Generate_SMT smt1 "/tmp/ex1.smt2".
