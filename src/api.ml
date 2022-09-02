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
  | Sort_Uninterpreted of Constr.t

type fun_sym = Constr.t * ((sort list) * sort)

type term =
  | Term_Fun of fun_sym * (term list)
  | Term_Int of Constr.t        (* of type Z *)
  | Term_Geq of term * term
  | Term_Eq of term * term
  | Term_And of term * term


(* Simple SMT-LIB syntax, in Coq *)
let smtcoq_api_modules = [["SMTCoqApi";"SMTLib"]]
let cSort_Bool = S.CoqTerms.gen_constant smtcoq_api_modules "Sort_Bool"
let cSort_Int = S.CoqTerms.gen_constant smtcoq_api_modules "Sort_Int"
let cSort_Uninterpreted = S.CoqTerms.gen_constant smtcoq_api_modules "Sort_Uninterpreted"
let cTerm_Fun = S.CoqTerms.gen_constant smtcoq_api_modules "Term_Fun"
let cTerm_Int = S.CoqTerms.gen_constant smtcoq_api_modules "Term_Int"
let cTerm_Geq = S.CoqTerms.gen_constant smtcoq_api_modules "Term_Geq"
let cTerm_Eq = S.CoqTerms.gen_constant smtcoq_api_modules "Term_Eq"
let cTerm_And = S.CoqTerms.gen_constant smtcoq_api_modules "Term_And"


(* Reification *)
let rec reify_list l =
  let c, args = Constr.decompose_app l in
  if c = Lazy.force S.CoqTerms.cnil then
    []
  else
    match args with
      | [_; x; xs] -> x::(reify_list xs)
      | _ -> assert false

let reify_sort (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force cSort_Bool then
    Sort_Bool
  else if c = Lazy.force cSort_Int then
    Sort_Int
  else if c = Lazy.force cSort_Uninterpreted then
    match args with
      | [num] -> Sort_Uninterpreted num
      | _ -> assert false
  else
    assert false

let reify_sym (sym:Constr.t) : fun_sym =
  let _, args = Constr.decompose_app sym in
  match args with
    | [_; _; sym; sign] ->
       (let _, sign = Constr.decompose_app sign in
        match sign with
          | [_; _; dom; codom] ->
             let dom = reify_list dom in
             (sym, (List.map reify_sort dom, reify_sort codom))
          | _ -> assert false
       )
    | _ -> assert false

let rec reify (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force cTerm_Fun then (
    match args with
      | [sym; args] ->
         let sym = reify_sym sym in
         let args = reify_list args in
         Term_Fun (sym, List.map reify args)
      | _ -> assert false
  ) else if c = Lazy.force cTerm_Int then (
    match args with
      | [z] -> Term_Int z
      | _ -> assert false
  ) else if c = Lazy.force cTerm_Geq then (
    match args with
      | [t1; t2] -> Term_Geq (reify t1, reify t2)
      | _ -> assert false
  ) else if c = Lazy.force cTerm_Eq then (
    match args with
      | [t1; t2] -> Term_Eq (reify t1, reify t2)
      | _ -> assert false
  ) else if c = Lazy.force cTerm_And then (
    match args with
      | [t1; t2] -> Term_And (reify t1, reify t2)
      | _ -> assert false
  ) else assert false


(* Compilation to low-level SMTCoq syntax *)
let dummy_typ_compdec = Constr.mkProp

let compile_sort rt = function
  | Sort_Bool -> S.SmtBtype.Tbool
  | Sort_Int -> S.SmtBtype.TZ
  | Sort_Uninterpreted c -> S.SmtBtype.declare rt c dummy_typ_compdec


let rec compile_positive ra (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force S.CoqTerms.cxH then (
    S.SmtAtom.Atom.get ra (S.SmtAtom.Acop S.SmtAtom.CO_xH)
  ) else if c = Lazy.force S.CoqTerms.cxO then (
    match args with
      | [arg] ->
         S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_xO, compile_positive ra arg))
      | _ -> assert false
  ) else if c = Lazy.force S.CoqTerms.cxI then (
    match args with
      | [arg] ->
         S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_xI, compile_positive ra arg))
      | _ -> assert false
  ) else assert false


let compile_Z ra (c:Constr.t) =
  let c, args = Constr.decompose_app c in
  if c = Lazy.force S.CoqTerms.cZ0 then (
    S.SmtAtom.Atom.get ra (S.SmtAtom.Acop S.SmtAtom.CO_Z0)
  ) else if c = Lazy.force S.CoqTerms.cZpos then (
    match args with
      | [arg] ->
         S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_Zpos, compile_positive ra arg))
      | _ -> assert false
  ) else if c = Lazy.force S.CoqTerms.cZneg then (
    match args with
      | [arg] ->
         S.SmtAtom.Atom.get ra (S.SmtAtom.Auop (S.SmtAtom.UO_Zneg, compile_positive ra arg))
      | _ -> assert false
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
