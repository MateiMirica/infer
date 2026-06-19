(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseModelsImport
module DSL = PulseModelsDSL

let refmut ~mut_ ~protected (dst_exp : Exp.t) (src_arg : ValueOrigin.t FuncArg.t) : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  if not (Config.is_checker_enabled TreeBorrows) then ret ()
  else
    let* {location} = get_data in
    let src_exp = src_arg.FuncArg.exp in
    exec_command (fun astate ->
        PulseTreeBorrowsOperations.exec_refmut ~dst_exp ~src_exp ~mut_ ~protected ~loc:location
          astate )


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"__rust_refmut" <>$ capt_exp $+ capt_arg $--> refmut ~mut_:true ~protected:false
  ; -"__rust_refmut_shared" <>$ capt_exp $+ capt_arg $--> refmut ~mut_:false ~protected:false
  ; -"__rust_refmut_protected" <>$ capt_exp $+ capt_arg $--> refmut ~mut_:true ~protected:true
  ; -"__rust_refmut_shared_protected" <>$ capt_exp $+ capt_arg
    $--> refmut ~mut_:false ~protected:true ]
