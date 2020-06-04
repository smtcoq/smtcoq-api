Require Import SMTCoq.SMTCoq.
Require Import ZArith.


Definition var := nat.
Inductive Exp : Type -> Type :=
  Exp_Var     : var -> Exp Z
| Exp_Z       : Z -> Exp Z
| Exp_EqbZ    : Exp Z -> Exp Z -> Exp bool
| Exp_Andb    : Exp bool -> Exp bool -> Exp bool.




Definition exp1 := (Exp_EqbZ (Exp_Var 0) (Exp_Z 0)).


(* TODO: provide a Coq API to build this *)
Definition t_atom :=
  let t0 := PArray.make 3 (Acop CO_xH) in
  let t1 := PArray.set t0 0 (Aapp 0 nil) in
  let t2 := PArray.set t1 1 (Acop CO_Z0) in
  let t3 := PArray.set t2 2 (Abop (BO_eq Typ.TZ) 0 1) in
  t3.

Definition t_form :=
  let t0 := PArray.make 1 Ftrue in
  let t1 := PArray.set t0 0 (Fatom 2) in
  t1.

Definition exp1_smtcoq := (0, t_form, t_atom).


(* TODO: implement this vernacular command *)
(* Generate_SMT exp1_smtcoq "/tmp/issue68.smt2". *)




(*
Definition satisfiable : Exp bool -> bool. Admitted.
Goal satisfiable (Exp_EqbNat (Exp_Var 0) (Exp_Nat 233)) = true.
Admitted.
Goal satisfiable (Exp_Andb (Exp_EqbNat (Exp_Var 0) (Exp_Nat 233))
                           (Exp_EqbNat (Exp_Var 0) (Exp_Nat 2333))) = false.
Admitted.
*)
