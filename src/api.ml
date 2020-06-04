(**************************************************************************)
(*                                                                        *)
(*     SMTCoq-Api                                                         *)
(*     Copyright (C) 2020                                                 *)
(*                                                                        *)
(*     Author: Chantal Keller - LRI, Université Paris-Saclay              *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)


let generate_smt formula output =
  let module S = Smtcoq_plugin in
  let _ = S.SmtForm.Ftrue in
  print_endline "ok"
