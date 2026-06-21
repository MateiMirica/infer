(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module AVMap = AbstractValue.Map
module AVSet = AbstractValue.Set

module Tag = struct
  type t = int [@@deriving compare, equal]

  let pp fmt t = F.fprintf fmt "T%d" t

  module Key = struct
    type nonrec t = t [@@deriving compare]
  end

  module Set = Stdlib.Set.Make (Key)
  module Map = Stdlib.Map.Make (Key)
end

module Perm = struct
  type t = Reserved | Unique | Frozen | Disabled | ReservedConflicted
  [@@deriving compare, equal]

  let pp fmt = function
    | Reserved ->
        F.pp_print_string fmt "Reserved"
    | Unique ->
        F.pp_print_string fmt "Unique"
    | Frozen ->
        F.pp_print_string fmt "Frozen"
    | Disabled ->
        F.pp_print_string fmt "Disabled"
    | ReservedConflicted ->
        F.pp_print_string fmt "ReservedConflicted"
end

module Rel = struct
  type t = Local | Foreign | Unrelated [@@deriving compare, equal]
end

module Access = struct
  type t = Read | Write [@@deriving compare, equal]

  let pp fmt = function Read -> F.pp_print_string fmt "Rd" | Write -> F.pp_print_string fmt "Wr"
end

module Ub = struct
  type t =
    | Disabled_local_read
    | Disabled_local_read_protected
    | Frozen_local_write
    | Frozen_local_write_protected
    | Disabled_local_write
    | Disabled_local_write_protected
    | ResC_local_write
    | Unique_foreign_read_protected
    | Foreign_write_protected
  [@@deriving compare, equal]

  let pp fmt = function
    | Disabled_local_read ->
        F.pp_print_string fmt "read through a Disabled tag"
    | Disabled_local_read_protected ->
        F.pp_print_string fmt "read through a protected Disabled tag"
    | Frozen_local_write ->
        F.pp_print_string fmt "write through a Frozen tag"
    | Frozen_local_write_protected ->
        F.pp_print_string fmt "write through a protected Frozen tag"
    | Disabled_local_write ->
        F.pp_print_string fmt "write through a Disabled tag"
    | Disabled_local_write_protected ->
        F.pp_print_string fmt "write through a protected Disabled tag"
    | ResC_local_write ->
        F.pp_print_string fmt "write through a ReservedConflicted tag"
    | Unique_foreign_read_protected ->
        F.pp_print_string fmt "foreign read of a protected Unique tag"
    | Foreign_write_protected ->
        F.pp_print_string fmt "foreign write to a protected tag"
end

module Operand = struct
  type t = {root: Ident.t option; cells: AbstractValue.t list}

  let untracked = {root= None; cells= []}

  let pp fmt {root; cells} =
    match root with
    | Some id ->
        F.fprintf fmt "n%a[%a]" Ident.pp id (Pp.seq ~sep:"." AbstractValue.pp) cells
    | None ->
        F.fprintf fmt "[%a]" (Pp.seq ~sep:"." AbstractValue.pp) cells


  let leaf {root; cells} = match root with None -> List.last cells | Some _ -> None
end

module Trans = struct
  let fire (perm : Perm.t) ~(protector : bool) (access : Access.t) : (Perm.t, Ub.t) Result.t =
    match (access, perm, protector) with
    | Read, Reserved, _ ->
        Ok Perm.Reserved
    | Read, Unique, _ ->
        Ok Perm.Unique
    | Read, Frozen, _ ->
        Ok Perm.Frozen
    | Read, ReservedConflicted, _ ->
        Ok Perm.ReservedConflicted
    | Read, Disabled, false ->
        Error Ub.Disabled_local_read
    | Read, Disabled, true ->
        Error Ub.Disabled_local_read_protected
    | Write, Reserved, _ ->
        Ok Perm.Unique
    | Write, Unique, _ ->
        Ok Perm.Unique
    | Write, Frozen, false ->
        Error Ub.Frozen_local_write
    | Write, Frozen, true ->
        Error Ub.Frozen_local_write_protected
    | Write, Disabled, false ->
        Error Ub.Disabled_local_write
    | Write, Disabled, true ->
        Error Ub.Disabled_local_write_protected
    | Write, ReservedConflicted, _ ->
        Error Ub.ResC_local_write


  let fire_foreign (perm : Perm.t) ~(protector : bool) (access : Access.t) : (Perm.t, Ub.t) Result.t
      =
    match (access, perm, protector) with
    | Read, Reserved, false ->
        Ok Perm.Reserved
    | Read, Unique, false ->
        Ok Perm.Frozen
    | Read, Frozen, false ->
        Ok Perm.Frozen
    | Read, Disabled, false ->
        Ok Perm.Disabled
    | Read, ReservedConflicted, false ->
        Ok Perm.ReservedConflicted
    | Read, Reserved, true ->
        Ok Perm.ReservedConflicted
    | Read, Unique, true ->
        Error Ub.Unique_foreign_read_protected
    | Read, Frozen, true ->
        Ok Perm.Frozen
    | Read, Disabled, true ->
        Ok Perm.Disabled
    | Read, ReservedConflicted, true ->
        Ok Perm.ReservedConflicted
    | Write, _, false ->
        Ok Perm.Disabled
    | Write, Disabled, true ->
        Ok Perm.Disabled
    | Write, _, true ->
        Error Ub.Foreign_write_protected
end

module Event = struct
  type t =
    { loc: Location.t
    ; tag: Tag.t
    ; perm_before: Perm.t
    ; access: Access.t
    ; protector: bool
    ; ub: Ub.t }
  [@@deriving compare, equal]

  let pp fmt {loc; tag; perm_before; access; protector; ub} =
    F.fprintf fmt "@[<h>%a: %a (%a, %a, prot=%b) -- %a@]" Location.pp loc Tag.pp tag Perm.pp
      perm_before Access.pp access protector Ub.pp ub
end

module IdentMap = Ident.Map

module St = struct
  type tag_info = {protector: bool; borrowed_cell: AbstractValue.t option}
  [@@deriving compare, equal]

  type t =
    { tag_infos: tag_info Tag.Map.t
    ; tags_at: Perm.t Tag.Map.t AVMap.t
    ; parent: Tag.t option Tag.Map.t
    ; local_refs: Tag.Set.t Tag.Map.t
    ; pointer_tag: Tag.t AVMap.t
    ; temps: Tag.t IdentMap.t
    ; object_root: Tag.t AVMap.t
    ; formal_tags: Tag.t Pvar.Map.t
    ; entry_pre: (Specialization.Pulse.TreeBorrows.t[@ignore])
    ; access_log: (Access.t * Tag.t * AbstractValue.t * Location.t) list
    ; next_tag: int }
  [@@deriving compare, equal]

  let empty =
    { tag_infos= Tag.Map.empty
    ; tags_at= AVMap.empty
    ; parent= Tag.Map.empty
    ; local_refs= Tag.Map.empty
    ; pointer_tag= AVMap.empty
    ; temps= IdentMap.empty
    ; object_root= AVMap.empty
    ; formal_tags= Pvar.Map.empty
    ; entry_pre= Specialization.Pulse.TreeBorrows.bottom
    ; access_log= []
    ; next_tag= 0 }


  let set_entry_pre state pre = {state with entry_pre= pre}

  let entry_pre_of state = state.entry_pre

  let tag_info_of state tag =
    try Tag.Map.find tag state.tag_infos
    with Stdlib.Not_found -> {protector= false; borrowed_cell= None}


  let protector_of state tag = (tag_info_of state tag).protector

  let parent_of state tag = try Tag.Map.find tag state.parent with Stdlib.Not_found -> None

  let local_refs_of state tag =
    try Tag.Map.find tag state.local_refs with Stdlib.Not_found -> Tag.Set.empty


  let rec root_of state tag =
    match parent_of state tag with Some p -> root_of state p | None -> tag


  let rec is_ancestor state ~ancestor ~descendant =
    Tag.equal ancestor descendant
    ||
    match parent_of state descendant with
    | Some p ->
        is_ancestor state ~ancestor ~descendant:p
    | None ->
        false


  let entries_at state av = try AVMap.find av state.tags_at with Stdlib.Not_found -> Tag.Map.empty

  let perm_at state tag av = Tag.Map.find_opt tag (entries_at state av)

  let own_perm state tag =
    let m = tag_info_of state tag in
    match Option.bind m.borrowed_cell ~f:(fun av -> perm_at state tag av) with
    | Some p ->
        p
    | None ->
        Perm.Reserved


  let set_entry state av tag perm =
    {state with tags_at= AVMap.add av (Tag.Map.add tag perm (entries_at state av)) state.tags_at}


  let inherit_hop state ~parent_av ~child_av =
    let pmap = entries_at state parent_av in
    if Tag.Map.is_empty pmap then state
    else
      let cmap = entries_at state child_av in
      let cmap' = Tag.Map.union (fun _ child_perm _parent_perm -> Some child_perm) cmap pmap in
      if Tag.Map.equal Perm.equal cmap' cmap then state
      else (
        {state with tags_at= AVMap.add child_av cmap' state.tags_at} )


  let inherit_access_path state (access_path : AbstractValue.t list) =
    match access_path with
    | [] | [_] ->
        state
    | first :: rest ->
        List.fold rest ~init:(state, first) ~f:(fun (state, parent_av) child_av ->
            (inherit_hop state ~parent_av ~child_av, child_av) )
        |> fst

  let touched_subheap state ~succs av =
    let rec go state visited order frontier =
      match frontier with
      | [] ->
          (state, List.rev order)
      | a :: rest ->
            let children =
              succs a |> List.filter ~f:(fun c -> not (AVSet.mem c visited))
            in
            let state =
              List.fold children ~init:state ~f:(fun st c -> inherit_hop st ~parent_av:a ~child_av:c)
            in
            let visited = List.fold children ~init:visited ~f:(fun v c -> AVSet.add c v) in
            go state visited (List.rev_append children order) (rest @ children)
    in
    go state (AVSet.singleton av) [av] [av]


  let bind_pointer_tag state av tag =
    {state with pointer_tag= AVMap.add av tag state.pointer_tag}


  let drop_pointer_tag state av =
    if AVMap.mem av state.pointer_tag then (
      {state with pointer_tag= AVMap.remove av state.pointer_tag} )
    else state


  let bind_temp state id tag =
    {state with temps= IdentMap.add id tag state.temps}


  let drop_temp state id =
    if IdentMap.mem id state.temps then {state with temps= IdentMap.remove id state.temps}
    else state


  let tag_of_operand state (op : Operand.t) : Tag.t option =
    match op.Operand.root with
    | Some id ->
        IdentMap.find_opt id state.temps
    | None ->
        Option.bind (List.last op.Operand.cells) ~f:(fun leaf -> AVMap.find_opt leaf state.pointer_tag)


  let tag_fresh state ~protector ~borrowed_cell =
    let t = state.next_tag in
    let state =
      {state with tag_infos= Tag.Map.add t {protector; borrowed_cell} state.tag_infos; next_tag= t + 1}
    in
    (t, state)


  let set_parent state tag p =
    {state with parent= Tag.Map.add tag p state.parent}


  let add_local_ref state ~tag ~local_to =
    let cur = local_refs_of state tag in
    {state with local_refs= Tag.Map.add tag (Tag.Set.add local_to cur) state.local_refs}

  let ensure_object_root state base_av =
    match AVMap.find_opt base_av state.object_root with
    | Some owner ->
        (owner, state)
    | None ->
        let owner, state = tag_fresh state ~protector:false ~borrowed_cell:(Some base_av) in
        let state = set_entry state base_av owner Perm.Unique in
        let state = {state with object_root= AVMap.add base_av owner state.object_root} in
        (owner, state)

  let adopt_borrowed_cell state tag av =
    let m = tag_info_of state tag in
    match m.borrowed_cell with
    | Some _ ->
        state
    | None ->
        let state = {state with tag_infos= Tag.Map.add tag {m with borrowed_cell= Some av} state.tag_infos} in
        if Tag.Map.mem tag (entries_at state av) then state else set_entry state av tag Perm.Reserved

  let perm_severity (p : Perm.t) =
    match p with
    | Perm.Reserved ->
        0
    | Perm.ReservedConflicted ->
        1
    | Perm.Unique ->
        2
    | Perm.Frozen ->
        3
    | Perm.Disabled ->
        4


  let perm_join a b = if perm_severity a >= perm_severity b then a else b

  let redirect_tag state ~from ~to_ =
    let sub t = if Tag.equal t from then to_ else t in
    let parent =
      Tag.Map.map
        (function Some p when Tag.equal p from -> Some to_ | other -> other)
        (Tag.Map.remove from state.parent)
    in
    let local_refs =
      Tag.Map.fold
        (fun t refs m ->
          let t = sub t in
          let refs = Tag.Set.fold (fun r s -> Tag.Set.add (sub r) s) refs Tag.Set.empty in
          let refs = Tag.Set.remove t refs in
          if Tag.Set.is_empty refs then m
          else
            let prev = try Tag.Map.find t m with Stdlib.Not_found -> Tag.Set.empty in
            Tag.Map.add t (Tag.Set.union prev refs) m )
        state.local_refs Tag.Map.empty
    in
    let tags_at =
      AVMap.map
        (fun entries ->
          match Tag.Map.find_opt from entries with
          | None ->
              entries
          | Some p ->
              let entries = Tag.Map.remove from entries in
              Tag.Map.update to_
                (function None -> Some p | Some p0 -> Some (perm_join p0 p))
                entries )
        state.tags_at
    in
    let pointer_tag = AVMap.map sub state.pointer_tag in
    let temps = IdentMap.map sub state.temps in
    let object_root = AVMap.map sub state.object_root in
    let formal_tags = Pvar.Map.map sub state.formal_tags in
    let access_log = List.map state.access_log ~f:(fun (a, t, av, l) -> (a, sub t, av, l)) in
    {state with parent; local_refs; tags_at; pointer_tag; temps; object_root; formal_tags; access_log}

  let merge_owner_trees state ~survivor ~victim =
    let ms = tag_info_of state survivor and mv = tag_info_of state victim in
    let merged =
      { protector= ms.protector || mv.protector
      ; borrowed_cell= (match ms.borrowed_cell with Some _ -> ms.borrowed_cell | None -> mv.borrowed_cell) }
    in
    let tag_infos = Tag.Map.add survivor merged (Tag.Map.remove victim state.tag_infos) in
    redirect_tag {state with tag_infos} ~from:victim ~to_:survivor

  let canonicalize_owners state ~f =
    let tags_at =
      AVMap.fold
        (fun av entries m ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | None ->
              AVMap.add av' entries m
          | Some entries0 ->
              AVMap.add av'
                (Tag.Map.union (fun _ p0 p -> Some (perm_join p0 p)) entries0 entries)
                m )
        state.tags_at AVMap.empty
    in
    let pointer_tag =
      AVMap.fold
        (fun av tag m ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | Some tag0 when not (Tag.equal tag0 tag) ->
              AVMap.remove av' m
          | _ ->
              AVMap.add av' tag m )
        state.pointer_tag AVMap.empty
    in
    let tag_infos =
      Tag.Map.map (fun m -> {m with borrowed_cell= Option.map m.borrowed_cell ~f}) state.tag_infos
    in
    let access_log = List.map state.access_log ~f:(fun (a, t, av, l) -> (a, t, f av, l)) in
    let state = {state with tags_at; pointer_tag; tag_infos; access_log} in
    let object_root, merges =
      AVMap.fold
        (fun av tag (m, ms) ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | Some tag0 when not (Tag.equal tag0 tag) ->
              let s, v = if Tag.compare tag0 tag <= 0 then (tag0, tag) else (tag, tag0) in
              (AVMap.add av' s m, (s, v) :: ms)
          | _ ->
              (AVMap.add av' tag m, ms) )
        state.object_root (AVMap.empty, [])
    in
    let state = {state with object_root} in
    let resolve redirects t =
      let rec go t = match Tag.Map.find_opt t redirects with Some t' -> go t' | None -> t in
      go t
    in
    let state, redirects =
      List.fold (List.rev merges) ~init:(state, Tag.Map.empty) ~f:(fun (st, rd) (s, v) ->
          let s = resolve rd s and v = resolve rd v in
          if Tag.equal s v then (st, rd)
          else
            let s, v = if Tag.compare s v <= 0 then (s, v) else (v, s) in
            (merge_owner_trees st ~survivor:s ~victim:v, Tag.Map.add v s rd) )
    in
    if Tag.Map.is_empty redirects then state
    else {state with object_root= AVMap.map (resolve redirects) state.object_root}


  let log_access state access tag ~av ~loc =
    {state with access_log= (access, tag, av, loc) :: state.access_log}

  let global_log state = List.rev state.access_log

  let pp_tags fmt state =
    F.fprintf fmt "@[<v>" ;
    Tag.Map.iter
      (fun tag (m : tag_info) ->
        let parent = parent_of state tag in
        F.fprintf fmt "%a -> (prot=%b, borrowed_cell=%s, parent=%s" Tag.pp tag m.protector
          (match m.borrowed_cell with Some av -> F.asprintf "%a" AbstractValue.pp av | None -> "-")
          (match parent with Some p -> F.asprintf "%a" Tag.pp p | None -> "-") ;
        AVMap.iter
          (fun av entries ->
            match Tag.Map.find_opt tag entries with
            | Some p ->
                F.fprintf fmt " [%a=%a]" AbstractValue.pp av Perm.pp p
            | None ->
                () )
          state.tags_at ;
        ( match Tag.Map.find_opt tag state.local_refs with
        | Some refs when not (Tag.Set.is_empty refs) ->
            F.fprintf fmt " local_to={%a}" (Pp.seq ~sep:"," Tag.pp) (Tag.Set.elements refs)
        | _ ->
            () ) ;
        F.fprintf fmt ")@," )
      state.tag_infos ;
    F.fprintf fmt "@]"


  let pp_ptrs fmt state =
    F.fprintf fmt "@[<v>" ;
    AVMap.iter (fun av t -> F.fprintf fmt "[%a] -> %a@," AbstractValue.pp av Tag.pp t) state.pointer_tag ;
    IdentMap.iter (fun id t -> F.fprintf fmt "n%a -> %a@," Ident.pp id Tag.pp t) state.temps ;
    AVMap.iter
      (fun av t -> F.fprintf fmt "[%a] =obj=> %a@," AbstractValue.pp av Tag.pp t)
      state.object_root ;
    Pvar.Map.iter
      (fun pv t -> F.fprintf fmt "formal %a -> %a@," Pvar.pp_value pv Tag.pp t)
      state.formal_tags ;
    F.fprintf fmt "@]"


  let pp_log fmt state =
    let pp_evt fmt (acc, t, _av, _loc) = F.fprintf fmt "(%a,%a)" Access.pp acc Tag.pp t in
    F.fprintf fmt "@[<v>[%a]@]" (Pp.comma_seq pp_evt) (List.rev state.access_log)


  let pp fmt state =
    F.fprintf fmt "@[<v>tags:@,  %a@,ptrs:@,  %a@,log:@,  %a@]" pp_tags state pp_ptrs state pp_log
      state
end

type state = {st: St.t; events: Event.t list} [@@deriving compare, equal]

let start () = {st= St.empty; events= []}

let pp_state fmt {st; events} =
  F.fprintf fmt "@[<v>%a@,events: [%a]@]" St.pp st (Pp.comma_seq Event.pp) events


let is_errored t = not (List.is_empty t.events)

let add_event t event = {t with events= event :: t.events}

let mk (st : St.t) (events : Event.t list) : state = {st; events}

let entry_pre (d : state) = St.entry_pre_of d.st

let classify_typ (typ : Typ.t) =
  match typ.desc with
  | Tptr (_, _) ->
      let mut = not (Typ.is_const typ.quals) in
      if Typ.is_reference_on_source typ.quals then `Reference mut
      else if mut then `RawPtr true
      else `RawPtr false
  | _ ->
      `Other


let initial_perm_of_shape shape =
  match shape with
  | `Reference true ->
      Perm.Reserved
  | `Reference false ->
      Perm.Frozen
  | `RawPtr true ->
      Perm.Reserved
  | `RawPtr false ->
      Perm.Frozen

let establish_root (st : St.t) ~(base_av : AbstractValue.t) : Tag.t * St.t =
  St.ensure_object_root st base_av

let tag_at_base (st : St.t) (base_av : AbstractValue.t) : Tag.t option =
  match AVMap.find_opt base_av st.St.object_root with
  | Some t ->
      Some t
  | None ->
      AVMap.find_opt base_av st.St.pointer_tag

let local_set_of (st : St.t) (through : Tag.t) : Tag.Set.t =
  let rec chain acc t =
    let acc = t :: acc in
    match St.parent_of st t with None -> acc | Some p -> chain acc p
  in
  let parent_chain = chain [] through in
  List.fold parent_chain ~init:Tag.Set.empty ~f:(fun s t ->
      Tag.Set.union (Tag.Set.add t s) (St.local_refs_of st t) )

let fire_at_loc (d : state) (loc : Location.t) ~(local_set : Tag.Set.t) ~(through : Tag.t)
    ~(arg_tags : Tag.Set.t) (a : AbstractValue.t) (acc : Access.t) : state =
  if is_errored d then d
  else
    let through_root = St.root_of d.st through in
    Tag.Map.fold
      (fun t perm (d : state) ->
        if is_errored d then d
        else if Tag.Set.mem t arg_tags && not (Tag.equal t through) then
          d
        else
          let rel =
            if Tag.Set.mem t local_set then Rel.Local
            else if Tag.equal (St.root_of d.st t) through_root then Rel.Foreign
            else Rel.Unrelated
          in
          match rel with
          | Rel.Unrelated ->
              d
          | (Rel.Local | Rel.Foreign) as rel -> (
              let protector = St.protector_of d.st t in
              let fire = match rel with Rel.Foreign -> Trans.fire_foreign | _ -> Trans.fire in
              match fire perm ~protector acc with
              | Ok perm' ->
                  mk (St.set_entry d.st a t perm') d.events
              | Error ub ->
                  add_event d {loc; tag= t; perm_before= perm; access= acc; protector; ub}
              ) )
      (St.entries_at d.st a) d

let access_through ?(log = true) ?(arg_tags = Tag.Set.empty) ?(access_path = [])
    ~(succs : AbstractValue.t -> AbstractValue.t list) (d : state) (loc : Location.t)
    ~(through : Tag.t) ~(av : AbstractValue.t) (acc : Access.t) : state =
  if is_errored d then d
  else (
    let st = St.inherit_access_path d.st access_path in
    let st = St.adopt_borrowed_cell st through av in
    let st, touched = St.touched_subheap st ~succs av in
    let local_set = local_set_of st through in
    let d = mk st d.events in
    let d =
      List.fold touched ~init:d ~f:(fun d a -> fire_at_loc d loc ~local_set ~through ~arg_tags a acc)
    in
    if is_errored d || not log then d
    else mk (St.log_access d.st acc through ~av ~loc) d.events )

let do_reborrow ~(protector : bool) (d : state) (loc : Location.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(bind : AbstractValue.t option)
    ~(mut_ : bool) ~(src : Operand.t) ~(borrowed_cell : AbstractValue.t) : state =
  let st = d.st in
  let parent_tag, st =
    match src with
    | {Operand.root= Some id} -> (
      match IdentMap.find_opt id st.St.temps with
      | Some t ->
          (Some t, st)
      | None ->
          let owner, st = establish_root st ~base_av:borrowed_cell in
          (Some owner, st) )
    | {Operand.root= None; cells= base :: _ as access_path} -> (
        let st = St.inherit_access_path st access_path in
        match tag_at_base st base with
        | Some t ->
            (Some t, st)
        | None ->
            let owner, st = establish_root st ~base_av:base in
            (Some owner, st) )
    | _ ->
        let owner, st = establish_root st ~base_av:borrowed_cell in
        (Some owner, st)
  in
  let initial_perm = if mut_ then Perm.Reserved else Perm.Frozen in
  let tag, st = St.tag_fresh st ~protector ~borrowed_cell:(Some borrowed_cell) in
  let st = St.set_parent st tag parent_tag in
  let st, touched = St.touched_subheap st ~succs borrowed_cell in
  let st =
    List.fold touched ~init:st ~f:(fun st a ->
        if Tag.Map.mem tag (St.entries_at st a) then st else St.set_entry st a tag initial_perm )
  in
  let st = match bind with Some av -> St.bind_pointer_tag st av tag | None -> st in
  let d = mk st d.events in
  access_through ~succs d loc ~through:tag ~av:borrowed_cell Access.Read

let resolve_access_target (st : St.t) ~(target : Operand.t) : (Tag.t * AbstractValue.t) option =
  let through =
    match target with
    | {Operand.root= Some id} ->
        IdentMap.find_opt id st.St.temps
    | {Operand.root= None; cells= base :: _} ->
        tag_at_base st base
    | _ ->
        None
  in
  match (through, List.last target.Operand.cells) with
  | Some t, Some av ->
      Some (t, av)
  | _ ->
      None

let exec_access ~(acc : Access.t) ~(target : Operand.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (d : state) : state =
  match resolve_access_target d.st ~target with
  | None ->
      d
  | Some (through, av) ->
      access_through ~access_path:target.Operand.cells ~succs d loc ~through ~av acc


let exec_load ~(id : Ident.t) ~(typ : Typ.t) ~(src : Operand.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (d : state) : state =
  if is_errored d then d
  else
    match classify_typ typ with
    | `Other ->
        exec_access ~acc:Access.Read ~target:src ~succs ~loc d
    | `Reference _ | `RawPtr _ -> (
      match
        Option.bind (List.last src.Operand.cells) ~f:(fun av -> AVMap.find_opt av d.st.St.pointer_tag)
      with
      | Some tag ->
          mk (St.bind_temp d.st id tag) d.events
      | None ->
          mk (St.drop_temp d.st id) d.events )


let exec_store ~(lhs : Operand.t) ~(rhs : Operand.t) ~(typ : Typ.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (d : state) : state =
  if is_errored d then d
  else
    match classify_typ typ with
    | (`Reference _ | `RawPtr _) as sh -> (
      match List.last lhs.Operand.cells with
      | None ->
          d
      | Some cell -> (
        let is_raw = match sh with `RawPtr _ -> true | _ -> false in
        match St.tag_of_operand d.st rhs with
        | Some tag ->
            mk (St.bind_pointer_tag d.st cell tag) d.events
        | None -> (
          match rhs with
          | {Operand.root= None; cells= base :: _ as access_path} when is_raw ->
              let st = St.inherit_access_path d.st access_path in
              let owner, st = establish_root st ~base_av:base in
              mk (St.bind_pointer_tag st cell owner) d.events
          | _ ->
              mk (St.drop_pointer_tag d.st cell) d.events ) ) )
    | `Other ->
        exec_access ~acc:Access.Write ~target:lhs ~succs ~loc d

let exec_refmut ~(dst : Operand.t) ~(src : Operand.t) ~(mut_ : bool) ~(protected : bool)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (d : state) : state =
  if is_errored d then d
  else
    match List.last src.Operand.cells with
    | None ->
        d
    | Some borrowed_cell ->
        do_reborrow ~protector:protected d loc ~succs ~bind:(Operand.leaf dst) ~mut_ ~src ~borrowed_cell

let perm_to_spec : Perm.t -> Specialization.Pulse.TreeBorrows.Perm.t = function
  | Perm.Reserved ->
      Specialization.Pulse.TreeBorrows.Perm.Reserved
  | Perm.Unique ->
      Specialization.Pulse.TreeBorrows.Perm.Unique
  | Perm.Frozen ->
      Specialization.Pulse.TreeBorrows.Perm.Frozen
  | Perm.Disabled ->
      Specialization.Pulse.TreeBorrows.Perm.Disabled
  | Perm.ReservedConflicted ->
      Specialization.Pulse.TreeBorrows.Perm.ReservedConflicted


let spec_to_perm : Specialization.Pulse.TreeBorrows.Perm.t -> Perm.t = function
  | Specialization.Pulse.TreeBorrows.Perm.Reserved ->
      Perm.Reserved
  | Specialization.Pulse.TreeBorrows.Perm.Unique ->
      Perm.Unique
  | Specialization.Pulse.TreeBorrows.Perm.Frozen ->
      Perm.Frozen
  | Specialization.Pulse.TreeBorrows.Perm.Disabled ->
      Perm.Disabled
  | Specialization.Pulse.TreeBorrows.Perm.ReservedConflicted ->
      Perm.ReservedConflicted


let rel_to_spec : Rel.t -> Specialization.Pulse.TreeBorrows.Rel.t option = function
  | Rel.Local ->
      Some Specialization.Pulse.TreeBorrows.Rel.Local
  | Rel.Foreign ->
      Some Specialization.Pulse.TreeBorrows.Rel.Foreign
  | Rel.Unrelated ->
      None

let write_cover (st : St.t) ~(tag : Tag.t) ~(perm : Perm.t)
    ~(borrowed_cell : AbstractValue.t option) ~(succs : AbstractValue.t -> AbstractValue.t list) :
    St.t =
  match borrowed_cell with
  | None ->
      st
  | Some borrowed_cell ->
      let st = St.set_entry st borrowed_cell tag perm in
      let st, touched = St.touched_subheap st ~succs borrowed_cell in
      List.fold touched ~init:st ~f:(fun st a ->
          if Tag.Map.mem tag (St.entries_at st a) then st else St.set_entry st a tag perm )

let perm_of_formal (tree_borrows : Specialization.Pulse.TreeBorrows.t) i (typ : Typ.t) :
    Perm.t option =
  match classify_typ typ with
  | (`Reference _ | `RawPtr _) as shape ->
      Some
        ( match
            List.Assoc.find tree_borrows.Specialization.Pulse.TreeBorrows.perms
              (Specialization.Pulse.TreeBorrows.ArgIndex.of_int i)
              ~equal:Specialization.Pulse.TreeBorrows.ArgIndex.equal
          with
        | Some perm ->
            spec_to_perm perm
        | None ->
            initial_perm_of_shape shape )
  | `Other ->
      None


let init_formals (formals : (Pvar.t * Typ.t) list)
    ~(cell_of : Pvar.t -> AbstractValue.t option)
    ~(borrowed_cell_of : Pvar.t -> AbstractValue.t option)
    ~(tree_borrows : Specialization.Pulse.TreeBorrows.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) (d : state) : state =
  let rels = tree_borrows.Specialization.Pulse.TreeBorrows.rels in
  let idx = Specialization.Pulse.TreeBorrows.ArgIndex.to_int in
  let local_pair a b =
    List.exists rels ~f:(fun (i, j, r) ->
        Int.equal (idx i) a && Int.equal (idx j) b
        && match r with Specialization.Pulse.TreeBorrows.Rel.Local -> true | _ -> false )
  in
  let mutual_local a b = local_pair a b && local_pair b a in
  let any_rel a b =
    List.exists rels ~f:(fun (i, j, _) ->
        (Int.equal (idx i) a && Int.equal (idx j) b)
        || (Int.equal (idx i) b && Int.equal (idx j) a) )
  in
  let d, _created =
    List.foldi formals ~init:(d, []) ~f:(fun i (d, created) (pvar, typ) ->
        match classify_typ typ with
        | (`Reference _ | `RawPtr _) as shape -> (
            let is_ref = match shape with `Reference _ -> true | `RawPtr _ -> false in
            let bind_and_record (d : state) tag =
              let st = {d.st with St.formal_tags= Pvar.Map.add pvar tag d.st.St.formal_tags} in
              let st = match cell_of pvar with Some c -> St.bind_pointer_tag st c tag | None -> st in
              (mk st d.events, (i, tag) :: created)
            in
            match List.find_map created ~f:(fun (j, tj) -> if mutual_local i j then Some tj else None)
            with
            | Some tj ->
                bind_and_record d tj
            | None ->
                let ti, st = St.tag_fresh d.st ~protector:is_ref ~borrowed_cell:None in
                let st =
                  match
                    List.find_map created ~f:(fun (j, tj) -> if any_rel i j then Some tj else None)
                  with
                  | Some tj ->
                      St.set_parent st ti (Some (St.root_of st tj))
                  | None ->
                      let root, st = St.tag_fresh st ~protector:false ~borrowed_cell:None in
                      St.set_parent st ti (Some root)
                in
                bind_and_record (mk st d.events) ti )
        | `Other ->
            (d, created) )
  in
  let formal_tag pv = Pvar.Map.find_opt pv d.st.St.formal_tags in
  let pvar_of k = Option.map (List.nth formals (idx k)) ~f:fst in
  let d =
    List.fold rels ~init:d ~f:(fun d (i, j, rel) ->
        match rel with
        | Specialization.Pulse.TreeBorrows.Rel.Foreign ->
            d
        | Specialization.Pulse.TreeBorrows.Rel.Local -> (
          match (Option.bind (pvar_of i) ~f:formal_tag, Option.bind (pvar_of j) ~f:formal_tag) with
          | Some ti, Some tj when not (Tag.equal ti tj) ->
              mk (St.add_local_ref d.st ~tag:ti ~local_to:tj) d.events
          | _ ->
              d ) )
  in
  let d =
    List.foldi formals ~init:d ~f:(fun i (d : state) (pvar, typ) ->
        match perm_of_formal tree_borrows i typ with
        | None ->
            d
        | Some perm -> (
          match Pvar.Map.find_opt pvar d.st.St.formal_tags with
          | None ->
              d
          | Some tag ->
              let borrowed_cell = borrowed_cell_of pvar in
              let st =
                match borrowed_cell with Some av -> St.adopt_borrowed_cell d.st tag av | None -> d.st
              in
              mk (write_cover st ~tag ~perm ~borrowed_cell ~succs) d.events ) )
  in
  let entry_pre =
    let perms =
      List.filter_mapi formals ~f:(fun i (pvar, _) ->
          Option.map (Pvar.Map.find_opt pvar d.st.St.formal_tags) ~f:(fun tag ->
              ( Specialization.Pulse.TreeBorrows.ArgIndex.of_int i
              , perm_to_spec (St.own_perm d.st tag) ) ) )
    in
    {tree_borrows with Specialization.Pulse.TreeBorrows.perms}
  in
  mk (St.set_entry_pre d.st entry_pre) d.events

let rel_of (st : St.t) a b : Rel.t =
  if Tag.equal a b then Rel.Local
  else if not (Tag.equal (St.root_of st a) (St.root_of st b)) then Rel.Unrelated
  else if Tag.Set.mem b (local_set_of st a) then Rel.Local
  else Rel.Foreign


let precondition_of_actuals (d : state) (actuals : Operand.t list) :
    Specialization.Pulse.TreeBorrows.t =
  let indexed =
    List.filter_mapi actuals ~f:(fun i op ->
        Option.map (St.tag_of_operand d.st op) ~f:(fun tag -> (i, tag)) )
  in
  let perms =
    List.map indexed ~f:(fun (i, tag) ->
        (Specialization.Pulse.TreeBorrows.ArgIndex.of_int i, perm_to_spec (St.own_perm d.st tag)) )
  in
  let rels =
    List.concat_map indexed ~f:(fun (i, ti) ->
        List.filter_map indexed ~f:(fun (j, tj) ->
            if Int.equal i j then None
            else
              Option.map (rel_to_spec (rel_of d.st ti tj)) ~f:(fun r ->
                  ( Specialization.Pulse.TreeBorrows.ArgIndex.of_int i
                  , Specialization.Pulse.TreeBorrows.ArgIndex.of_int j
                  , r ) ) ) )
  in
  {Specialization.Pulse.TreeBorrows.perms; rels}


let perm_spec_needed ~(formals : (Pvar.t * Typ.t) list)
    (tb_pre : Specialization.Pulse.TreeBorrows.t) : bool =
  List.exists tb_pre.Specialization.Pulse.TreeBorrows.perms ~f:(fun (i, perm) ->
      match List.nth formals (Specialization.Pulse.TreeBorrows.ArgIndex.to_int i) with
      | Some (_, ftyp) -> (
        match classify_typ ftyp with
        | (`Reference _ | `RawPtr _) as shape ->
            not (Perm.equal (spec_to_perm perm) (initial_perm_of_shape shape))
        | `Other ->
            false )
      | None ->
          false )

let replay_callee_access (d : state) (loc : Location.t) (through : Tag.t) ~(arg_tags : Tag.Set.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(av : AbstractValue.t) (acc : Access.t) :
    state =
  if is_errored d then d
  else (
    access_through ~log:false ~arg_tags ~succs d loc ~through ~av acc )


let exec_call ~(callee_state : state) ~(callee_pdesc : Procdesc.t)
    ~(subst : AbstractValue.t -> AbstractValue.t option)
    ~(callee_edges : (AbstractValue.t * AbstractValue.t) list)
    ~(callee_ret_cell : AbstractValue.t option) ~(args : Operand.t list) ~(ret_id : Ident.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (d : state) : state =
  if is_errored d then d
  else
    let callee_st = callee_state.st in
    let formals = Procdesc.get_pvar_formals callee_pdesc in
    let zipped = match List.zip formals args with Ok z -> z | Unequal_lengths -> [] in
    let arg_tags =
      List.fold zipped ~init:Tag.Set.empty ~f:(fun s (_, op) ->
          match St.tag_of_operand d.st op with Some t -> Tag.Set.add t s | None -> s )
    in
    let formal_to_caller =
      List.filter_map zipped ~f:(fun ((pvar, ftyp), op) ->
          match classify_typ ftyp with
          | `Other ->
              None
          | `Reference _ | `RawPtr _ -> (
            match (Pvar.Map.find_opt pvar callee_st.St.formal_tags, St.tag_of_operand d.st op) with
            | Some ftag, Some ctag ->
                Some (ftag, ctag)
            | _ ->
                None ) )
    in
    let deepest_formal_ancestor callee_tag =
      formal_to_caller
      |> List.filter ~f:(fun (ftag, _) ->
             St.is_ancestor callee_st ~ancestor:ftag ~descendant:callee_tag )
      |> List.max_elt ~compare:(fun (t1, _) (t2, _) ->
             if Tag.equal t1 t2 then 0
             else if St.is_ancestor callee_st ~ancestor:t2 ~descendant:t1 then 1
             else -1 )
    in
    let route callee_tag = Option.map (deepest_formal_ancestor callee_tag) ~f:snd in
    let subst_or_drop av =
      match subst av with
      | Some av' ->
          Some av'
      | None ->
          None
    in
    let perm_of ct =
      match St.own_perm callee_st ct with Perm.ReservedConflicted -> Perm.Reserved | p -> p
    in
    let borrowed_cell_of ct =
      Option.bind (St.tag_info_of callee_st ct).borrowed_cell ~f:subst_or_drop
    in
    let materialize_chain st ~from_formal ~to_tag ~root =
      let rec collect ct acc =
        if Tag.equal ct from_formal then acc
        else
          match St.parent_of callee_st ct with
          | Some p ->
              collect p (ct :: acc)
          | None ->
              ct :: acc
      in
      List.fold (collect to_tag []) ~init:(st, root) ~f:(fun (st, parent) ct ->
          let borrowed_cell = borrowed_cell_of ct in
          let t_c, st = St.tag_fresh st ~protector:false ~borrowed_cell in
          let st = St.set_parent st t_c (Some parent) in
          let st =
            match borrowed_cell with Some av -> St.set_entry st av t_c (perm_of ct) | None -> st
          in
          (st, t_c) )
    in
    let d =
      let st =
        List.fold callee_edges ~init:d.st ~f:(fun st (a, b) ->
            match (subst_or_drop a, subst_or_drop b) with
            | Some a', Some b' ->
                St.inherit_hop st ~parent_av:a' ~child_av:b'
            | _ ->
                st )
      in
      mk st d.events
    in
    let callee_log = St.global_log callee_st in
    let d =
      List.fold callee_log ~init:d
        ~f:(fun (d : state) (access, callee_t, callee_av, callee_loc) ->
          if is_errored d then d
          else
            match (route callee_t, subst_or_drop callee_av) with
            | Some caller_tag, Some av ->
                let report_loc =
                  if Location.equal callee_loc Location.dummy then loc else callee_loc
                in
                let d = replay_callee_access d report_loc caller_tag ~arg_tags ~succs ~av access in
                if is_errored d then d
                else mk (St.log_access d.st access caller_tag ~av ~loc:report_loc) d.events
            | _ ->
                d )
    in
    let d =
      List.fold formal_to_caller ~init:d ~f:(fun (d : state) (ftag, ctag) ->
          if is_errored d then d
          else (
            let st = d.st in
            let releasing = St.protector_of st ctag in
            let unconflict (p : Perm.t) =
              if releasing then
                match p with Perm.ReservedConflicted -> Perm.Reserved | p -> p
              else p
            in
            let st =
              { st with
                St.tags_at= AVMap.map (fun entries -> Tag.Map.remove ctag entries) st.St.tags_at }
            in
            let st =
              AVMap.fold
                (fun callee_av entries st ->
                  match Tag.Map.find_opt ftag entries with
                  | None ->
                      st
                  | Some perm -> (
                    match subst_or_drop callee_av with
                    | Some av ->
                        St.set_entry st av ctag (unconflict perm)
                    | None ->
                        st ) )
                callee_st.St.tags_at st
            in
            let cm = St.tag_info_of st ctag in
            let st =
              { st with
                St.tag_infos=
                  Tag.Map.add ctag
                    {cm with protector= (if releasing then false else cm.protector)}
                    st.St.tag_infos }
            in
            mk st d.events ) )
    in

    let d =
      List.fold formal_to_caller ~init:d ~f:(fun (d : state) (ftag_x, _) ->
          if is_errored d then d
          else
            match (St.tag_info_of callee_st ftag_x).borrowed_cell with
            | None ->
                d
            | Some pointee_callee -> (
              match AVMap.find_opt pointee_callee callee_st.St.pointer_tag with
              | None ->
                  d
              | Some escaping_tag -> (
                match deepest_formal_ancestor escaping_tag with
                | Some (formal_anc, caller_anc) when not (Tag.equal formal_anc ftag_x) -> (
                  match subst_or_drop pointee_callee with
                  | Some caller_pointee ->
                      let st, last_tag =
                        materialize_chain d.st ~from_formal:formal_anc ~to_tag:escaping_tag
                          ~root:caller_anc
                      in
                      mk (St.bind_pointer_tag st caller_pointee last_tag) d.events
                  | None ->
                      d )
                | _ ->
                    d ) ) )
    in
    if is_errored d then d
    else
      match Option.bind callee_ret_cell ~f:(fun av -> AVMap.find_opt av callee_st.St.pointer_tag) with
      | None ->
          d
      | Some callee_ret_tag -> (
        match deepest_formal_ancestor callee_ret_tag with
        | None ->
            d
        | Some (formal_tag, caller_arg_tag) ->
            let st, last_tag =
              materialize_chain d.st ~from_formal:formal_tag ~to_tag:callee_ret_tag
                ~root:caller_arg_tag
            in
            mk (St.bind_temp st ret_id last_tag) d.events )

let canonicalize ~f (d : state) : state = {d with st= St.canonicalize_owners d.st ~f}

let report_errors proc_desc err_log (d : state) : unit =
  List.iter (List.rev d.events) ~f:(fun (e : Event.t) ->
      let message =
        F.asprintf
          "Tree Borrows Undefined Behaviour: %a via %a access on tag %a (was %a, protector=%b)."
          Ub.pp e.ub Access.pp e.access Tag.pp e.tag Perm.pp e.perm_before e.protector
      in
      Reporting.log_issue proc_desc err_log ~loc:e.loc TreeBorrows IssueType.tree_borrows_ub message )
