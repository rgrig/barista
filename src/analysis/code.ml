(*
 * This file is part of Barista.
 * Copyright (C) 2007-2011 Xavier Clerc.
 *
 * Barista is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Barista is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Utils

exception Already_present

let make_pending depth =
  if depth then
    let cont = Stack.create () in
    (fun x ->
      try
        Stack.iter
          (fun y ->
            if ControlFlow.equal_vertex x y then
              raise Already_present)
          cont;
        Stack.push x cont
      with Already_present -> ()),
    (fun () -> Stack.is_empty cont),
    (fun () -> Stack.pop cont)
  else
    let cont = Queue.create () in
    (fun x ->
      try
        Queue.iter
          (fun y ->
            if ControlFlow.equal_vertex x y then
              raise Already_present)
          cont;
        Queue.add x cont
      with Already_present -> ()),
    (fun () -> Queue.is_empty cont),
    (fun () -> Queue.take cont)

let map_graph f g =
  ControlFlow.map_graph
    (fun x y -> f x, y)
    (fun x _ -> x)
    (fun x _ _ -> x)
    g

let visit_graph ~depth ~init_mark ~init_root ~visit_node g =
  let pending_add, pending_is_empty, pending_take = make_pending depth in
  (* create a new graph with empty marks *)
  let g = map_graph init_mark g in
  (* mark the root *)
  let root = ControlFlow.root_of_graph g in
  init_root root;
  pending_add root;
  (* visit the graph *)
  while not (pending_is_empty ()) do
    let vertex = pending_take () in
    let edge =
      try
        let edge = ControlFlow.edge_of_vertex g vertex in
        Some (snd edge)
      with Not_found -> None in
    let handlers = ControlFlow.handlers_of_vertex g vertex in
    visit_node pending_add vertex edge handlers
  done;
  g

let remove_dead_code g =
  let g =
    visit_graph
      ~depth:false
      ~init_mark:(fun x -> ref false, x)
      ~init_root:(fun v ->
        let lbl, _ = ControlFlow.label_of_vertex v in
        lbl := true)
      ~visit_node:(fun pending_add _ edge handlers ->
        let mark v =
          let lbl, _ = ControlFlow.label_of_vertex v in
          if not !lbl then begin
            lbl := true;
            pending_add v
          end in
        (match edge with
        | Some (ControlFlow.Unconditional_edge dst) ->
            mark dst
        | Some (ControlFlow.Conditional_edge (_, ifso, ifno)) ->
            mark ifso;
            mark ifno
        | Some (ControlFlow.Switch_edge (_, dest, dests)) ->
            mark dest;
            List.iter mark dests
        | None -> ());
        List.iter (fun (_, _, dst) -> mark dst) handlers)
      g in
  (* remove unvisited nodes *)
  let unvisited =
    ControlFlow.fold_vertices
      (fun v acc ->
        let lbl, _ = ControlFlow.label_of_vertex v in
        if not !lbl then v :: acc else acc)
      g
      [] in
  let g = ControlFlow.remove_vertices g unvisited in
  map_graph snd g

let optimize_jumps g =
  let rec follow_link v =
    let instrs = ControlFlow.instructions_of_vertex v in
    if instrs = [] then
      match ControlFlow.edge_of_vertex g v with
      | _, ControlFlow.Unconditional_edge v' -> follow_link v'
      | _ -> v
    else
      v in
  let rec follow_jump v =
    let instrs = ControlFlow.instructions_of_vertex v in
    if instrs = [] then
      match snd (ControlFlow.edge_of_vertex g v) with
      | ControlFlow.Unconditional_edge dst ->
          follow_jump dst
      | ControlFlow.Conditional_edge (jk, dst1, dst2) ->
          ControlFlow.Conditional_edge (jk, follow_link dst1, follow_link dst2)
      | ControlFlow.Switch_edge (sk, dst, l) ->
          ControlFlow.Switch_edge (sk, follow_link dst, List.map follow_link l)
    else
      ControlFlow.Unconditional_edge v in
  let eq_edge = ControlFlow.equal_edge ControlFlow.equal_vertex in
  let changes_edges, changes_handlers =
    ControlFlow.fold_vertices
      (fun v (acc_edges, acc_handlers) ->
        let acc_edges =
          try
            let lbl, e = ControlFlow.edge_of_vertex g v in
            let e' = match e with
            | ControlFlow.Unconditional_edge dst ->
                follow_jump dst
            | ControlFlow.Conditional_edge (jk, dst1, dst2) ->
                ControlFlow.Conditional_edge (jk, follow_link dst1, follow_link dst2)
            | ControlFlow.Switch_edge (sk, dst, l) ->
                ControlFlow.Switch_edge (sk, follow_link dst, List.map follow_link l) in
            if eq_edge e e' then acc_edges else (v, e', lbl) :: acc_edges
          with Not_found -> acc_edges in
        let handlers = ControlFlow.handlers_of_vertex g v in
        let handlers =
          List.fold_left
            (fun acc (lbl, cn, dst) ->
              let dst' = follow_link dst in
              if ControlFlow.equal_vertex dst dst' then
                acc
              else
                (lbl, cn, dst') :: acc)
            []
            handlers in
        let acc_handlers =
          if handlers = [] then
            acc_handlers
          else
            (v, handlers) :: acc_handlers in
        (acc_edges, acc_handlers))
      g
      ([], []) in
  let g =
    List.fold_left
      (fun acc (v, e, lbl) -> ControlFlow.add_edge acc v e lbl)
      g
      changes_edges in
  List.fold_left
    (fun acc (v, l) ->
      List.fold_left
        (fun acc (lbl, cn, dst) ->
          ControlFlow.add_handler acc v cn dst lbl)
        acc
        l)
    g
    changes_handlers
      
type 'a block = {
    mutable id : int32;
    mutable in_line : bool;
    lines : Utils.u2 list;
    old_label : 'a;
  }

let flatten_graph_with_goto_size wide g =
  let goto_size = if wide then 5 else 3 in
  let blocks = ref [] in
  let g =
    visit_graph
      ~depth:true
      ~init_mark:(fun (x, y) -> { id = Int32.min_int; in_line = false; lines = y; old_label = x; })
      ~init_root:(fun v ->
        let lbl = ControlFlow.label_of_vertex v in
        lbl.id <- 0l;
        blocks := v :: !blocks)
      ~visit_node:(fun pending_add _ edge handlers ->
        let mark v =
          let lbl = ControlFlow.label_of_vertex v in
          if lbl.id = Int32.min_int then begin
            lbl.id <- Int32.of_int (List.length !blocks);
            blocks := v :: !blocks;
            pending_add v
          end in
        List.iter (fun (_, _, dst) -> mark dst) handlers;
        match edge with
        | Some (ControlFlow.Unconditional_edge dst) ->
            mark dst
        | Some (ControlFlow.Conditional_edge (_, ifso, ifno)) ->
            mark ifno;
            mark ifso
        | Some (ControlFlow.Switch_edge (_, dest, dests)) ->
            List.fold_right (fun x _ -> mark x) dests ();
            mark dest
        | None -> ())
      g in
  (* check which blocks are in-line *)
  List.iter
    (fun vertex ->
      let lbl = ControlFlow.label_of_vertex vertex in
      let edge = try Some (snd (ControlFlow.edge_of_vertex g vertex)) with Not_found -> None in
      match edge with
      | Some (ControlFlow.Unconditional_edge dst) ->
          lbl.in_line <- (Int32.succ lbl.id) = (ControlFlow.label_of_vertex dst).id
      | Some (ControlFlow.Conditional_edge (_, _, ifno)) ->
          lbl.in_line <- (Int32.succ lbl.id) = (ControlFlow.label_of_vertex ifno).id;
      | Some (ControlFlow.Switch_edge (_, _, _)) ->
          lbl.in_line <- false
      | None -> ())
    !blocks;
  (* compute offsets *)
  let _, blocks =
    List.fold_right
      (fun v (ofs, acc) ->
        let lbl = ControlFlow.label_of_vertex v in
        let instrs = ControlFlow.instructions_of_vertex v in
        let edge = try Some (snd (ControlFlow.edge_of_vertex g v)) with Not_found -> None in
        let size = Instruction.size_of_list ofs instrs in
        let size = size + 
          match edge with
          | Some (ControlFlow.Unconditional_edge _) ->
              if lbl.in_line then 0 else goto_size
          | Some (ControlFlow.Conditional_edge (_, _, _)) ->
              (if wide then 3 + 5 else 3) + (if lbl.in_line then 0 else goto_size)
          | Some (ControlFlow.Switch_edge (sk, _, l)) ->
              let switch_ofs = ofs + size in
              1 + (3 - (switch_ofs mod 4)) + 4 + 4 +
                (match sk with
                | ControlFlow.Table _ -> 4 + 4 * (List.length l)
                | ControlFlow.Lookup _ -> 8 * (List.length l))
          | None -> 0 in
        lbl.id <- Int32.of_int ofs;
        ofs + size, v :: acc)
      !blocks
      (0, []) in
  let blocks = List.rev blocks in
  (* add suffixes *)
  let instructions =
    List.map
      (fun v ->
        let lbl = ControlFlow.label_of_vertex v in
        let instrs = ControlFlow.instructions_of_vertex v in
        let size = Int32.of_int (Instruction.size_of_list (Int32.to_int lbl.id) instrs) in
        let ofs_of_vertex32 x = Int32.sub (ControlFlow.label_of_vertex x).id (Int32.add lbl.id size) in
        let ofs_of_vertex x =
          let x = ofs_of_vertex32 x in
          if x >= -32768l && x <= 32767l then
            Int32.to_int x
          else
            invalid_arg "BaristaLibrary.Code.flatten_graph" in
        let edge = try Some (snd (ControlFlow.edge_of_vertex g v)) with Not_found -> None in
        match edge with
        | Some (ControlFlow.Unconditional_edge dst) ->
            if lbl.in_line then
              instrs
            else if wide then
              instrs @ [ Instruction.GOTO_W (s4 (ofs_of_vertex32 dst)) ]
            else
              instrs @ [ Instruction.GOTO (s2 (ofs_of_vertex dst)) ]
        | Some (ControlFlow.Conditional_edge (jk, ifso, ifno)) ->
            if wide then begin
              let next = s2 (3 + goto_size) in
              instrs
              @ (match ControlFlow.opposite_jump_kind jk with
              | ControlFlow.References_equal -> [ Instruction.IF_ACMPEQ next ]
              | ControlFlow.References_not_equal -> [ Instruction.IF_ACMPNE next ]
              | ControlFlow.Integers_equal -> [ Instruction.IF_ICMPEQ next ]
              | ControlFlow.Integers_greater_or_equal -> [ Instruction.IF_ICMPGE next ]
              | ControlFlow.Integers_greater -> [ Instruction.IF_ICMPGT next ]
              | ControlFlow.Integers_lower_or_equal -> [ Instruction.IF_ICMPLE next ]
              | ControlFlow.Integers_lower -> [ Instruction.IF_ICMPLT next ]
              | ControlFlow.Integers_not_equal -> [ Instruction.IF_ICMPNE next ]
              | ControlFlow.Integer_equal_zero -> [ Instruction.IFEQ next ]
              | ControlFlow.Integer_greater_or_equal_zero -> [ Instruction.IFGE next ]
              | ControlFlow.Integer_greater_zero -> [ Instruction.IFGT next ]
              | ControlFlow.Integer_lower_or_equal_zero -> [ Instruction.IFLE next ]
              | ControlFlow.Integer_lower_zero -> [ Instruction.IFLT next ]
              | ControlFlow.Integer_not_equal_zero -> [ Instruction.IFNE next ]
              | ControlFlow.Reference_null -> [ Instruction.IFNULL next ]
              | ControlFlow.Reference_not_null -> [ Instruction.IFNONNULL next ])
              @ ([Instruction.GOTO_W (s4 (ofs_of_vertex32 ifso))])
              @ (if lbl.in_line then
                []
              else
                let dest = Int32.sub (ofs_of_vertex32 ifno) (Int32.of_int goto_size) in
                [ Instruction.GOTO_W (s4 dest) ])
            end else begin
              let ofs = s2 (ofs_of_vertex ifso) in
              instrs
              @ (match jk with
              | ControlFlow.References_equal -> [ Instruction.IF_ACMPEQ ofs ]
              | ControlFlow.References_not_equal -> [ Instruction.IF_ACMPNE ofs ]
              | ControlFlow.Integers_equal -> [ Instruction.IF_ICMPEQ ofs ]
              | ControlFlow.Integers_greater_or_equal -> [ Instruction.IF_ICMPGE ofs ]
              | ControlFlow.Integers_greater -> [ Instruction.IF_ICMPGT ofs ]
              | ControlFlow.Integers_lower_or_equal -> [ Instruction.IF_ICMPLE ofs ]
              | ControlFlow.Integers_lower -> [ Instruction.IF_ICMPLT ofs ]
              | ControlFlow.Integers_not_equal -> [ Instruction.IF_ICMPNE ofs ]
              | ControlFlow.Integer_equal_zero -> [ Instruction.IFEQ ofs ]
              | ControlFlow.Integer_greater_or_equal_zero -> [ Instruction.IFGE ofs ]
              | ControlFlow.Integer_greater_zero -> [ Instruction.IFGT ofs ]
              | ControlFlow.Integer_lower_or_equal_zero -> [ Instruction.IFLE ofs ]
              | ControlFlow.Integer_lower_zero -> [ Instruction.IFLT ofs ]
              | ControlFlow.Integer_not_equal_zero -> [ Instruction.IFNE ofs ]
              | ControlFlow.Reference_null -> [ Instruction.IFNULL ofs ]
              | ControlFlow.Reference_not_null -> [ Instruction.IFNONNULL ofs ])
              @ (if lbl.in_line then
                []
              else
                [ Instruction.GOTO (s2 ((ofs_of_vertex ifno) - goto_size)) ])
            end
          | Some (ControlFlow.Switch_edge (sk, default, l)) ->
              instrs
              @ (match sk with
              | ControlFlow.Table (low, high) ->
                  [ Instruction.TABLESWITCH (s4 (ofs_of_vertex32 default),
                                             low,
                                             high,
                                             List.map (fun x -> s4 (ofs_of_vertex32 x)) l) ]
              | ControlFlow.Lookup k ->
                  [ Instruction.LOOKUPSWITCH (s4 (ofs_of_vertex32 default),
                                              s4 (Int32.of_int (List.length l)),
                                              List.map2 (fun x y -> x, s4 (ofs_of_vertex32 y)) k l) ])
          | None -> instrs)
      blocks in
  (* build line number table *)
  let offset_line_couples =
    List.map
      (fun v ->
        let lbl = ControlFlow.label_of_vertex v in
        let instrs = ControlFlow.instructions_of_vertex v in
        let _, res =
          List.fold_left2
            (fun (ofs, lst) instr line ->
              let size = Instruction.size_of ofs instr in
              (ofs + size, (ofs, line) :: lst))
            (Int32.to_int lbl.id, [])
            instrs
            lbl.lines in
        List.rev res)
      blocks in
  let offset_line_couples = List.flatten offset_line_couples in
  let _, offset_line_couples =
    List.fold_left
      (fun (prev_line, acc) (ofs, line) ->
        match prev_line with
        | Some prev when line <> prev ->
            (Some line, (u2 ofs, line) :: acc)
        | _ ->
            (prev_line, acc))
      (None, [])
      offset_line_couples in
  (* build exception table *)
  let table =
    ControlFlow.fold_handlers
      (fun v _ cn v' acc ->
        let ofs = Int32.to_int (ControlFlow.label_of_vertex v).id in
        let ofs' = Int32.to_int (ControlFlow.label_of_vertex v').id in
        let i = ControlFlow.instructions_of_vertex v in
        let sz = Instruction.size_of_list ofs i in
        if sz > 0 then
          (ofs, ofs + sz, ofs', cn) :: acc
        else
          acc)
      g
      [] in
  let same_class cn1 cn2 =
    match cn1, cn2 with
    | (Some n1), (Some n2) -> Name.equal_for_class n1 n2
    | None, None -> true
    | Some _, None | None, Some _ -> false in
  let rec optimize_table = function
    | (ofs_start, ofs_end, ofs_dest, cn) :: (ofs_start', ofs_end', ofs_dest', cn') :: tl
      when ofs_end = ofs_start' && ofs_dest = ofs_dest' && (same_class cn cn') ->
        optimize_table ((ofs_start, ofs_end', ofs_dest, cn) :: tl)
    | (ofs_start, ofs_end, ofs_dest, cn) :: tl ->
        (u2 ofs_start, u2 ofs_end, u2 ofs_dest, cn) :: (optimize_table tl)
    | [] ->
        [] in
  let table = optimize_table (List.sort compare table) in
  let table =
    List.map
      (fun (ofs_start, ofs_end, ofs_dest, cn) ->
        { Attribute.try_start = ofs_start;
          Attribute.try_end = ofs_end;
          Attribute.catch = ofs_dest;
          Attribute.caught = cn; })
      table in
  (List.flatten instructions),
  (List.rev offset_line_couples),
  table,
  map_graph (fun x -> x.id, x.old_label) g

let flatten_graph g =
  try
    flatten_graph_with_goto_size false g
  with _ ->
    flatten_graph_with_goto_size true g

let optimize_graph g =
  let g = Peephole.optimize_graph g in
  let g = optimize_jumps g in
  let g = remove_dead_code g in
  g

let dummy_state = StackState.make_empty ()

type mark = {
    offset : int32;
    mutable stack_state : StackState.t;
    mutable max_stack : int;
    mutable max_locals : int;
  }

let java_lang_Throwable = Name.make_for_class_from_external @"java.lang.Throwable"

let update_state n s =
  let stack = ref s.StackState.stack in
  for i = 1 to n do
    stack := StackState.pop !stack
  done;
  { s with StackState.stack = !stack }

let equal_state x y =
  if x == dummy_state || y == dummy_state then
     false
  else
    StackState.equal x y

module IntMap = Map.Make (struct type t = int let compare = Pervasives.compare end)

let compute_stack_infos unify_fun g s =
  let unify x y =
    if x == dummy_state then
      StackState.unify unify_fun y y
    else if y == dummy_state then
      StackState.unify unify_fun x x
    else
      StackState.unify unify_fun x y in
  let g =
    visit_graph
      ~depth:false
      ~init_mark:(fun (x, _) ->
        { offset = x;
          stack_state = dummy_state;
          max_stack = 0;
          max_locals = 0; })
      ~init_root:(fun v ->
        let lbl = ControlFlow.label_of_vertex v in
        lbl.stack_state <- s)
      ~visit_node:(fun pending_add vertex edge handlers ->
        let mark v s =
          let lbl = ControlFlow.label_of_vertex v in
          let s' = unify lbl.stack_state s in
          if not (equal_state lbl.stack_state s') then begin
            lbl.stack_state <- s';
            lbl.max_locals <- 0;
            lbl.max_stack <- 0;
            pending_add v
          end in
        let label = ControlFlow.label_of_vertex vertex in
        let instructions = ControlFlow.instructions_of_vertex vertex in
        let initial_state = label.stack_state in
        let offset = Int32.to_int label.offset in
        let final_state, _ =
          List.fold_left
            (fun (state, ofs) instr ->
              let sz = Instruction.size_of ofs instr in
              let new_state = StackState.update (u2 ofs) instr state in
              label.max_stack <- max label.max_stack (StackState.stack_size new_state);
              label.max_locals <- max label.max_locals (StackState.locals_size new_state);
              (new_state, ofs + sz))
            (initial_state, offset)
            instructions in
        (match edge with
        | Some (ControlFlow.Unconditional_edge dst) ->
            mark dst final_state
        | Some (ControlFlow.Conditional_edge (jk, ifso, ifno)) ->
            let final_state = update_state (ControlFlow.nb_args_of_jump_kind jk) final_state in
            mark ifso final_state;
            mark ifno final_state
        | Some (ControlFlow.Switch_edge (sk, dest, dests)) ->
            let final_state = update_state (ControlFlow.nb_args_of_switch_kind sk) final_state in
            mark dest final_state;
            List.iter (fun x -> mark x final_state) dests
        | None -> ());
        List.iter
          (fun (_, cn, dst) ->
            let cn = match cn with Some x -> x | None -> java_lang_Throwable in
            let state = { initial_state with StackState.stack = StackState.only_exception cn } in
            mark dst state)
          handlers)
      g in
  let max_stack, max_locals, frames =
    ControlFlow.fold_vertices
      (fun v (ms, ml, sf) ->
        let lbl = ControlFlow.label_of_vertex v in
        let ofs = Int32.to_int lbl.offset in
        (max ms lbl.max_stack,
         max ml lbl.max_locals,
         (if (lbl.stack_state != dummy_state) then
           try
             let old = IntMap.find ofs sf in
             IntMap.add ofs (unify old lbl.stack_state) sf
           with Not_found ->
             IntMap.add ofs lbl.stack_state sf
         else
           sf)))
      g
      (0, 0, IntMap.empty) in
  let max_locals' =
    ControlFlow.fold_vertices
      (fun v acc ->
        let instrs = ControlFlow.instructions_of_vertex v in
        List.fold_left
          (fun acc i ->
            try
              let index =
                match i with
                | Instruction.ASTORE p -> p
                | Instruction.ASTORE_0 -> u1 0
                | Instruction.ASTORE_1 -> u1 1
                | Instruction.ASTORE_2 -> u1 2
                | Instruction.ASTORE_3 -> u1 3
                | Instruction.DSTORE p -> u1_succ p
                | Instruction.DSTORE_0 -> u1 1
                | Instruction.DSTORE_1 -> u1 2
                | Instruction.DSTORE_2 -> u1 3
                | Instruction.DSTORE_3 -> u1 4
                | Instruction.FSTORE p -> p
                | Instruction.FSTORE_0 -> u1 0
                | Instruction.FSTORE_1 -> u1 1
                | Instruction.FSTORE_2 -> u1 2
                | Instruction.FSTORE_3 -> u1 3
                | Instruction.ISTORE p -> p
                | Instruction.ISTORE_0 -> u1 0
                | Instruction.ISTORE_1 -> u1 1
                | Instruction.ISTORE_2 -> u1 2
                | Instruction.ISTORE_3 -> u1 3
                | Instruction.LSTORE p -> u1_succ p
                | Instruction.LSTORE_0 -> u1 1
                | Instruction.LSTORE_1 -> u1 2
                | Instruction.LSTORE_2 -> u1 3
                | Instruction.LSTORE_3 -> u1 4
                | _ -> raise Not_found in
              max (u1_succ index) acc
            with Not_found -> acc)
          acc
          instrs)
      g
      (u1 0) in
  let frames = IntMap.fold (fun x y acc -> (u2 x, y) :: acc) frames [] in
  u2 max_stack,
  u2 (max max_locals (max_locals' :> int)),
  StackState.encode (List.rev frames)
