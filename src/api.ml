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


module S = Smtcoq_plugin


(* Simple SMT-LIB syntax *)
type sort =
  | Sort_Bool
  | Sort_Int
  | Sort_BitVec of int
  | Sort_Uninterpreted of Constr.t

type fun_sym = Constr.t * ((sort list) * sort)

type bvUnaryOp =
  | BVNot
  | BVNeg

type bvBinOp =
  | BVAnd
  | BVOr
  | BVAdd
  | BVMul
  | BVUDiv
  | BVURem
  | BVShl
  | BVShr

type term =
  | Term_Fun of fun_sym * (term list)
  | Term_Int of Constr.t        (* of type Z *)
  | Term_Geq of term * term
  | Term_Eq of term * term
  | Term_And of term * term
  | Term_Or of term * term
  | Term_Not of term
  | Term_ITE of term * term * term
  | Term_True
  | Term_False
  | Term_BVLit of bool list
  | Term_BVConcat of term * term
  | Term_BVExtract of int * int * term
  | Term_BVUnaryOp of (bvUnaryOp * term)
  | Term_BVBinOp of (bvBinOp * term * term)
  | Term_BVUlt of (term * term)

(* Simple SMT-LIB syntax, in Coq *)
let gc prefix constant =
  lazy (UnivGen.constr_of_monomorphic_global (Global.env ()) (Coqlib.lib_ref (prefix ^ "." ^ constant)))
let smtcoq_api_prefix = "SMTCoqAPI.SMTLib"
let smtcoq_api_gc = gc smtcoq_api_prefix
let cSort_Bool = smtcoq_api_gc "Sort_Bool"
let cSort_Int = smtcoq_api_gc "Sort_Int"
let cSort_BitVec = smtcoq_api_gc "Sort_BitVec"
let cSort_Uninterpreted = smtcoq_api_gc "Sort_Uninterpreted"

let cBVNot = smtcoq_api_gc "BVNot"
let cBVNeg = smtcoq_api_gc "BVNeg"
let cBVAnd = smtcoq_api_gc "BVAnd"
let cBVOr = smtcoq_api_gc "BVOr"
let cBVAdd = smtcoq_api_gc "BVAdd"
let cBVMul = smtcoq_api_gc "BVMul"
let cBVUDiv = smtcoq_api_gc "BVUDiv"
let cBVURem = smtcoq_api_gc "BVURem"
let cBVShl = smtcoq_api_gc "BVShl"
let cBVShr = smtcoq_api_gc "BVShr"

let cTerm_Fun = smtcoq_api_gc "Term_Fun"
let cTerm_Int = smtcoq_api_gc "Term_Int"
let cTerm_Geq = smtcoq_api_gc "Term_Geq"
let cTerm_Eq = smtcoq_api_gc "Term_Eq"
let cTerm_And = smtcoq_api_gc "Term_And"
let cTerm_Or = smtcoq_api_gc "Term_Or"
let cTerm_Not = smtcoq_api_gc "Term_Not"
let cTerm_ITE = smtcoq_api_gc "Term_ITE"
let cTerm_True = smtcoq_api_gc "Term_True"
let cTerm_False = smtcoq_api_gc "Term_False"
let cTerm_BVLit = smtcoq_api_gc "Term_BVLit"
let cTerm_BVConcat = smtcoq_api_gc "Term_BVConcat"
let cTerm_BVExtract = smtcoq_api_gc "Term_BVExtract"
let cTerm_BVUnaryOp = smtcoq_api_gc "Term_BVUnaryOp"
let cTerm_BVBinOp = smtcoq_api_gc "Term_BVBinOp"
let cTerm_BVUlt = smtcoq_api_gc "Term_BVUlt"

(* Reification *)
let rec reify_list l =
  let c, args = Constr.decompose_app l in
  if c = Lazy.force S.CoqTerms.cnil then
    []
  else (
    assert (Array.length args = 3);
    let x = args.(1) in
    let xs = args.(2) in
    x::(reify_list xs)
  )

let reify_sort (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force cSort_Bool then
    Sort_Bool
  else if c = Lazy.force cSort_Int then
    Sort_Int
  else if c = Lazy.force cSort_BitVec then (
    assert (Array.length args = 1);
    let n = S.CoqTerms.mk_N args.(0) in
    Sort_BitVec n
  ) else if c = Lazy.force cSort_Uninterpreted then (
    assert (Array.length args = 1);
    let num = args.(0) in
    Sort_Uninterpreted num
  ) else
    assert false

let reify_sym (sym:Constr.t) : fun_sym =
  let _, args = Constr.decompose_app sym in
  assert (Array.length args = 4);
  let sym = args.(2) in
  let sign = args.(3) in
  let _, sign = Constr.decompose_app sign in
  assert (Array.length sign = 4);
  let dom = sign.(2) in
  let codom = sign.(3) in
  let dom = reify_list dom in
  (sym, (List.map reify_sort dom, reify_sort codom))

let reify_unop (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  assert (Array.length args = 0);
  if c = Lazy.force cBVNot then BVNot
  else if c = Lazy.force cBVNeg then BVNeg
  else assert false

let reify_binop (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  assert (Array.length args = 0);
  if c = Lazy.force cBVAnd then BVAnd
  else if c = Lazy.force cBVOr then BVOr
  else if c = Lazy.force cBVAdd then BVAdd
  else if c = Lazy.force cBVMul then BVMul
  else if c = Lazy.force cBVUDiv then BVUDiv
  else if c = Lazy.force cBVURem then BVURem
  else if c = Lazy.force cBVShl then BVShl
  else if c = Lazy.force cBVShr then BVShr
  else assert false

let rec reify (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force cTerm_Fun then (
    assert (Array.length args = 2);
    let sym = args.(0) in
    let args = args.(1) in
    let sym = reify_sym sym in
    let args = reify_list args in
    Term_Fun (sym, List.map reify args)
  ) else if c = Lazy.force cTerm_Int then (
    assert (Array.length args = 1);
    let z = args.(0) in
    Term_Int z
  ) else if c = Lazy.force cTerm_Geq then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_Geq (reify t1, reify t2)
  ) else if c = Lazy.force cTerm_Eq then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_Eq (reify t1, reify t2)
  ) else if c = Lazy.force cTerm_And then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_And (reify t1, reify t2)
  ) else if c = Lazy.force cTerm_Or then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_Or (reify t1, reify t2)
  ) else if c = Lazy.force cTerm_Not then (
    assert (Array.length args = 1);
    let t = args.(0) in
    Term_Not (reify t)
  ) else if c = Lazy.force cTerm_ITE then (
    assert (Array.length args = 3);
    let cond = args.(0) in
    let ifT = args.(1) in
    let ifF = args.(2) in
    Term_ITE (reify cond, reify ifT, reify ifF)
  ) else if c = Lazy.force cTerm_True then (
    assert (Array.length args = 0);
    Term_True
  ) else if c = Lazy.force cTerm_False then (
    assert (Array.length args = 0);
    Term_False
  ) else if c = Lazy.force cTerm_BVLit then (
    let l = reify_list args.(0) in
    let l' = List.map S.CoqTerms.mk_bool l in
    Term_BVLit l'
  ) else if c = Lazy.force cTerm_BVConcat then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_BVConcat (reify t1, reify t2)
  ) else if c = Lazy.force cTerm_BVExtract then (
    assert (Array.length args = 3);
    let lo = S.CoqTerms.mk_N args.(0) in
    let hi = S.CoqTerms.mk_N args.(1) in
    let t = args.(2) in
    Term_BVExtract (lo, hi, reify t)
  ) else if c = Lazy.force cTerm_BVUnaryOp then (
    assert (Array.length args = 2);
    let op = args.(0) in
    let t = args.(1) in
    Term_BVUnaryOp (reify_unop op, reify t)
  ) else if c = Lazy.force cTerm_BVBinOp then (
    assert (Array.length args = 2);
    let op = args.(0) in
    let t1 = args.(1) in
    let t2 = args.(2) in
    Term_BVBinOp (reify_binop op, reify t1, reify t2)
  ) else if c = Lazy.force cTerm_BVUlt then (
    assert (Array.length args = 2);
    let t1 = args.(0) in
    let t2 = args.(1) in
    Term_BVUlt (reify t1, reify t2)
  ) else assert false


(* Compilation to low-level SMTCoq syntax *)
(* let dummy_typ_compdec = Constr.mkProp *)

let compile_sort rt = function
  | Sort_Bool -> S.SmtBtype.Tbool
  | Sort_Int -> S.SmtBtype.TZ
  | Sort_BitVec w -> S.SmtBtype.TBV w (* How do I convert a Constr.t to an int whtih S.SmtBtype.TBV expects? *)
  | Sort_Uninterpreted c -> failwith "Not implemented yet" (* S.SmtBtype.declare rt c dummy_typ_compdec *)


let rec compile_positive ra (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force S.CoqTerms.cxH then (
    S.SmtAtom.Atom.get ra (S.SmtAtom.Acop S.SmtAtom.CO_xH)
  ) else if c = Lazy.force S.CoqTerms.cxO then (
    assert (Array.length args = 1);
    let arg = args.(0) in
    S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_xO, compile_positive ra arg))
  ) else if c = Lazy.force S.CoqTerms.cxI then (
    assert (Array.length args = 1);
    let arg = args.(0) in
    S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_xI, compile_positive ra arg))
  ) else assert false


let compile_Z ra (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force S.CoqTerms.cZ0 then (
    S.SmtAtom.Atom.get ra (S.SmtAtom.Acop S.SmtAtom.CO_Z0)
  ) else if c = Lazy.force S.CoqTerms.cZpos then (
    assert (Array.length args = 1);
    let arg = args.(0) in
    S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_Zpos, compile_positive ra arg))
  ) else if c = Lazy.force S.CoqTerms.cZneg then (
    assert (Array.length args = 1);
    let arg = args.(0) in
    S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_Zneg, compile_positive ra arg))
  ) else assert false


type form_atom =
  | Form of S.SmtAtom.Form.t
  | Atom of S.SmtAtom.Atom.t

let get_atom = function
  | Atom a -> a
  | _ -> assert false

let get_form rf = function
  | Form f -> f
  | Atom a -> S.SmtAtom.Form.get rf (S.SmtForm.Fatom a)

let rec compile rt ro rf ra = function
  | Term_Fun ((sym, (dom, codom)), args) ->
     let dom = List.map (compile_sort rt) dom in
     let codom = compile_sort rt codom in
     let hargs = Array.of_list (List.map (
                                    fun arg -> get_atom (compile rt ro rf ra arg)
                                  ) args) in
     let op =
       try S.SmtAtom.Op.of_coq ro sym
       with | Not_found ->
               S.SmtAtom.Op.declare ro sym (Array.of_list dom) codom None
     in
     Atom (S.SmtAtom.Atom.get ra (S.SmtAtom.Aapp (op, hargs)))
  | Term_Int z -> Atom (compile_Z ra z)
  | Term_Geq (t1, t2) ->
     let t1 = get_atom (compile rt ro rf ra t1) in
     let t2 = get_atom (compile rt ro rf ra t2) in
     Atom (S.SmtAtom.Atom.get ra (S.SmtAtom.Abop (S.SmtAtom.BO_Zge, t1, t2)))
  | Term_Eq (t1, t2) ->
     let t1 = get_atom (compile rt ro rf ra t1) in
     let t2 = get_atom (compile rt ro rf ra t2) in
     let ty = S.SmtAtom.Atom.type_of t1 in
     Atom (S.SmtAtom.Atom.get ra (S.SmtAtom.Abop (S.SmtAtom.BO_eq ty, t1, t2)))
  | Term_And (t1, t2) ->
     let t1 = get_form rf (compile rt ro rf ra t1) in
     let t2 = get_form rf (compile rt ro rf ra t2) in
     Form (S.SmtAtom.Form.get rf (S.SmtForm.Fapp (S.SmtForm.Fand, [|t1; t2|])))
  | Term_BVLit l ->
     Atom (S.SmtAtom.Atom.get ra (S.SmtAtom.Acop (S.SmtAtom.CO_BV l)))
  | Term_True -> assert false
  | Term_False -> assert false
  | Term_Or (_, _) -> assert false
  | Term_Not _ -> assert false
  | Term_ITE (_, _, _) -> assert false
  | Term_BVConcat (_, _) -> assert false
  | Term_BVExtract (_, _, _) -> assert false
  | Term_BVUnaryOp _ -> assert false
  | Term_BVBinOp _ -> assert false
  | Term_BVUlt _ -> assert false

let compile rt ro rf ra c = get_form rf (compile rt ro rf ra c)


(* SMT-LIB export *)
(* TODO: expose Verit.export in SMTCoq *)
let export out_channel rt ro lsmt =
  let fmt = Format.formatter_of_out_channel out_channel in
  Format.fprintf fmt "(set-logic UFLIA)@.";

  List.iter (fun (i,t) ->
    let s = "Tindex_"^(string_of_int i) in
    Format.fprintf fmt "(declare-sort %s 0)@." s
  ) (S.SmtBtype.to_list rt);

  List.iter (fun (i,dom,cod,op) ->
    let s = "op_"^(string_of_int i) in
    Format.fprintf fmt "(declare-fun %s (" s;
    let is_first = ref true in
    Array.iter (fun t -> if !is_first then is_first := false else Format.fprintf fmt " "; S.SmtBtype.to_smt fmt t) dom;
    Format.fprintf fmt ") ";
    S.SmtBtype.to_smt fmt cod;
    Format.fprintf fmt ")@."
  ) (S.SmtAtom.Op.to_list ro);

  List.iter (fun u -> Format.fprintf fmt "(assert ";
                      S.SmtAtom.Form.to_smt fmt u;
                      Format.fprintf fmt ")\n") lsmt;

  Format.fprintf fmt "(check-sat)\n(exit)@."


(* Main function *)
let generate_smt env sigma formula output =
  let rt = S.SmtBtype.create () in
  let ro = S.SmtAtom.Op.create () in
  let ra = S.SmtAtom.Atom.create () in
  let rf = S.SmtAtom.Form.create () in
  let formula = EConstr.to_constr sigma (Reductionops.whd_delta env sigma formula) in
  let formula = compile rt ro rf ra (reify formula) in
  let out = Stdlib.open_out output in
  export out rt ro [formula];
  close_out out
