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


val generate_smt : Environ.env -> Evd.evar_map -> EConstr.constr -> string -> unit
