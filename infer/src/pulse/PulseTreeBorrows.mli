(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue

module Operand : sig
  (** how a SIL operand reaches the heap-blind domain. *)
  type t = {root: Ident.t option; cells: AbstractValue.t list}

  val untracked : t

  val leaf : t -> AbstractValue.t option

  val pp : Format.formatter -> t -> unit
end

type state [@@deriving compare, equal]

val start : unit -> state

val pp_state : Format.formatter -> state -> unit

val entry_pre : state -> Specialization.Pulse.TreeBorrows.t

val init_formals :
     (Pvar.t * Typ.t) list
  -> cell_of:(Pvar.t -> AbstractValue.t option)
  -> borrowed_cell_of:(Pvar.t -> AbstractValue.t option)
  -> tree_borrows:Specialization.Pulse.TreeBorrows.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> state
  -> state

val exec_load :
     id:Ident.t
  -> typ:Typ.t
  -> src:Operand.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val exec_store :
     lhs:Operand.t
  -> rhs:Operand.t
  -> typ:Typ.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val exec_refmut :
     dst:Operand.t
  -> src:Operand.t
  -> mut_:bool
  -> protected:bool
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val canonicalize : f:(AbstractValue.t -> AbstractValue.t) -> state -> state

val exec_call :
     callee_state:state
  -> callee_pdesc:Procdesc.t
  -> subst:(AbstractValue.t -> AbstractValue.t option)
  -> callee_edges:(AbstractValue.t * AbstractValue.t) list
  -> callee_ret_cell:AbstractValue.t option
  -> args:Operand.t list
  -> ret_id:Ident.t
  -> succs:(AbstractValue.t -> AbstractValue.t list)
  -> loc:Location.t
  -> state
  -> state

val precondition_of_actuals : state -> Operand.t list -> Specialization.Pulse.TreeBorrows.t

val perm_spec_needed :
  formals:(Pvar.t * Typ.t) list -> Specialization.Pulse.TreeBorrows.t -> bool

val report_errors : Procdesc.t -> Errlog.t -> state -> unit
