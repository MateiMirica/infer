(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val init_formals :
     (Pvar.t * Typ.t) list
  -> tree_borrows:Specialization.Pulse.TreeBorrows.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val exec_load :
  id:Ident.t -> e:Exp.t -> typ:Typ.t -> loc:Location.t -> AbductiveDomain.t -> AbductiveDomain.t

val exec_store :
     lhs:Exp.t
  -> rhs:Exp.t
  -> typ:Typ.t
  -> loc:Location.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val exec_refmut :
     dst_exp:Exp.t
  -> src_exp:Exp.t
  -> mut_:bool
  -> protected:bool
  -> loc:Location.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val graft_call :
     callee_summary:AbductiveDomain.Summary.summary
  -> callee_pname:Procname.t
  -> tb_arg_exps:Exp.t list
  -> subst_map:(AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
  -> ret_id:Ident.t
  -> loc:Location.t
  -> caller:AbductiveDomain.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val compute_specialization :
  formals:(Pvar.t * Typ.t) list -> Exp.t list -> AbductiveDomain.t -> Specialization.Pulse.t option

val report_errors : Procdesc.t -> Errlog.t -> AbductiveDomain.Summary.summary -> unit
