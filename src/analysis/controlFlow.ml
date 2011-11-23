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

(* Base types *)

type return_kind =
  | Reference
  | Double
  | Float
  | Integer
  | Long
  | Void

type jump_kind =
  | References_equal
  | References_not_equal
  | Integers_equal
  | Integers_greater_or_equal
  | Integers_greater
  | Integers_lower_or_equal
  | Integers_lower
  | Integers_not_equal
  | Integer_equal_zero
  | Integer_greater_or_equal_zero
  | Integer_greater_zero
  | Integer_lower_or_equal_zero
  | Integer_lower_zero
  | Integer_not_equal_zero
  | Reference_null
  | Reference_not_null

let string_of_jump_kind = function
  | References_equal -> "eq/refs"
  | References_not_equal -> "ne/refs"
  | Integers_equal -> "eq/ints"
  | Integers_greater_or_equal -> "ge/ints"
  | Integers_greater -> "gt/ints"
  | Integers_lower_or_equal -> "le/ints"
  | Integers_lower -> "lt/ints"
  | Integers_not_equal -> "ne/ints"
  | Integer_equal_zero -> "eq/int"
  | Integer_greater_or_equal_zero -> "ge/int"
  | Integer_greater_zero -> "gt/int"
  | Integer_lower_or_equal_zero -> "le/int"
  | Integer_lower_zero -> "lt/int"
  | Integer_not_equal_zero -> "ne/int"
  | Reference_null -> "null"
  | Reference_not_null -> "not null"

let nb_args_of_jump_kind = function
  | References_equal
  | References_not_equal
  | Integers_equal
  | Integers_greater_or_equal
  | Integers_greater
  | Integers_lower_or_equal
  | Integers_lower
  | Integers_not_equal -> 2
  | Integer_equal_zero
  | Integer_greater_or_equal_zero
  | Integer_greater_zero
  | Integer_lower_or_equal_zero
  | Integer_lower_zero
  | Integer_not_equal_zero
  | Reference_null
  | Reference_not_null -> 1

let opposite_jump_kind = function
  | References_equal -> References_not_equal
  | References_not_equal -> References_equal
  | Integers_equal -> Integers_not_equal
  | Integers_greater_or_equal -> Integers_lower
  | Integers_greater -> Integers_lower_or_equal
  | Integers_lower_or_equal -> Integers_greater
  | Integers_lower -> Integers_greater_or_equal
  | Integers_not_equal -> Integers_equal
  | Integer_equal_zero -> Integer_not_equal_zero
  | Integer_greater_or_equal_zero -> Integer_lower_zero
  | Integer_greater_zero -> Integer_lower_or_equal_zero
  | Integer_lower_or_equal_zero -> Integer_greater_zero
  | Integer_lower_zero -> Integer_greater_or_equal_zero
  | Integer_not_equal_zero -> Integer_equal_zero
  | Reference_null -> Reference_not_null
  | Reference_not_null -> Reference_null

type switch_kind =
  | Table of s4 * s4
  | Lookup of s4 list

let string_of_switch_kind = function
  | Table _ -> "table"
  | Lookup _ -> "lookup"

let nb_args_of_switch_kind = function
  | Table _
  | Lookup _ -> 1


(* Exception *)

type error =
  | Unsupported_instruction of string
  | Vertex_not_in_graph
  | Invalid_switch_edge
  | Empty_graph
  | Cannot_remove_root

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unsupported_instruction x ->
      Printf.sprintf "unsupported instruction: %S" x
  | Vertex_not_in_graph -> "vertex is not in graph"
  | Invalid_switch_edge -> "invalid vertex edge"
  | Empty_graph -> "empty graph"
  | Cannot_remove_root -> "cannot remove root"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Flow information for instructions *)

type for_instruction =
  | Next_instruction
  | Unconditional_jump of s4
  | Conditional_jump of jump_kind * s4
  | Switch_jump of (switch_kind * s4 * s4 list)
  | Called_method
  | Calling_method of return_kind
  | Exception_handler

let for_instruction = function
  | Instruction.AALOAD -> Next_instruction
  | Instruction.AASTORE -> Next_instruction
  | Instruction.ACONST_NULL -> Next_instruction
  | Instruction.ALOAD _ -> Next_instruction
  | Instruction.ALOAD_0 -> Next_instruction
  | Instruction.ALOAD_1 -> Next_instruction
  | Instruction.ALOAD_2 -> Next_instruction
  | Instruction.ALOAD_3 -> Next_instruction
  | Instruction.ANEWARRAY _ -> Next_instruction
  | Instruction.ARETURN -> Calling_method Reference
  | Instruction.ARRAYLENGTH -> Next_instruction
  | Instruction.ASTORE _ -> Next_instruction
  | Instruction.ASTORE_0 -> Next_instruction
  | Instruction.ASTORE_1 -> Next_instruction
  | Instruction.ASTORE_2 -> Next_instruction
  | Instruction.ASTORE_3 -> Next_instruction
  | Instruction.ATHROW -> Exception_handler
  | Instruction.BALOAD -> Next_instruction
  | Instruction.BASTORE -> Next_instruction
  | Instruction.BIPUSH _ -> Next_instruction
  | Instruction.CALOAD -> Next_instruction
  | Instruction.CASTORE -> Next_instruction
  | Instruction.CHECKCAST _ -> Next_instruction
  | Instruction.D2F -> Next_instruction
  | Instruction.D2I -> Next_instruction
  | Instruction.D2L -> Next_instruction
  | Instruction.DADD -> Next_instruction
  | Instruction.DALOAD -> Next_instruction
  | Instruction.DASTORE -> Next_instruction
  | Instruction.DCMPG -> Next_instruction
  | Instruction.DCMPL -> Next_instruction
  | Instruction.DCONST_0 -> Next_instruction
  | Instruction.DCONST_1 -> Next_instruction
  | Instruction.DDIV -> Next_instruction
  | Instruction.DLOAD _ -> Next_instruction
  | Instruction.DLOAD_0 -> Next_instruction
  | Instruction.DLOAD_1 -> Next_instruction
  | Instruction.DLOAD_2 -> Next_instruction
  | Instruction.DLOAD_3 -> Next_instruction
  | Instruction.DMUL -> Next_instruction
  | Instruction.DNEG -> Next_instruction
  | Instruction.DREM -> Next_instruction
  | Instruction.DRETURN -> Calling_method Double
  | Instruction.DSTORE _ -> Next_instruction
  | Instruction.DSTORE_0 -> Next_instruction
  | Instruction.DSTORE_1 -> Next_instruction
  | Instruction.DSTORE_2 -> Next_instruction
  | Instruction.DSTORE_3 -> Next_instruction
  | Instruction.DSUB -> Next_instruction
  | Instruction.DUP -> Next_instruction
  | Instruction.DUP2 -> Next_instruction
  | Instruction.DUP2_X1 -> Next_instruction
  | Instruction.DUP2_X2 -> Next_instruction
  | Instruction.DUP_X1 -> Next_instruction
  | Instruction.DUP_X2 -> Next_instruction
  | Instruction.F2D -> Next_instruction
  | Instruction.F2I -> Next_instruction
  | Instruction.F2L -> Next_instruction
  | Instruction.FADD -> Next_instruction
  | Instruction.FALOAD -> Next_instruction
  | Instruction.FASTORE -> Next_instruction
  | Instruction.FCMPG -> Next_instruction
  | Instruction.FCMPL -> Next_instruction
  | Instruction.FCONST_0 -> Next_instruction
  | Instruction.FCONST_1 -> Next_instruction
  | Instruction.FCONST_2 -> Next_instruction
  | Instruction.FDIV -> Next_instruction
  | Instruction.FLOAD _ -> Next_instruction
  | Instruction.FLOAD_0 -> Next_instruction
  | Instruction.FLOAD_1 -> Next_instruction
  | Instruction.FLOAD_2 -> Next_instruction
  | Instruction.FLOAD_3 -> Next_instruction
  | Instruction.FMUL -> Next_instruction
  | Instruction.FNEG -> Next_instruction
  | Instruction.FREM -> Next_instruction
  | Instruction.FRETURN -> Calling_method Float
  | Instruction.FSTORE _ -> Next_instruction
  | Instruction.FSTORE_0 -> Next_instruction
  | Instruction.FSTORE_1 -> Next_instruction
  | Instruction.FSTORE_2 -> Next_instruction
  | Instruction.FSTORE_3 -> Next_instruction
  | Instruction.FSUB -> Next_instruction
  | Instruction.GETFIELD _ -> Next_instruction
  | Instruction.GETSTATIC _ -> Next_instruction
  | Instruction.GOTO ofs -> Unconditional_jump (s4_of_s2 ofs)
  | Instruction.GOTO_W ofs -> Unconditional_jump ofs
  | Instruction.I2B -> Next_instruction
  | Instruction.I2C -> Next_instruction
  | Instruction.I2D -> Next_instruction
  | Instruction.I2F -> Next_instruction
  | Instruction.I2L -> Next_instruction
  | Instruction.I2S -> Next_instruction
  | Instruction.IADD -> Next_instruction
  | Instruction.IALOAD -> Next_instruction
  | Instruction.IAND -> Next_instruction
  | Instruction.IASTORE -> Next_instruction
  | Instruction.ICONST_0 -> Next_instruction
  | Instruction.ICONST_1 -> Next_instruction
  | Instruction.ICONST_2 -> Next_instruction
  | Instruction.ICONST_3 -> Next_instruction
  | Instruction.ICONST_4 -> Next_instruction
  | Instruction.ICONST_5 -> Next_instruction
  | Instruction.ICONST_M1 -> Next_instruction
  | Instruction.IDIV -> Next_instruction
  | Instruction.IF_ACMPEQ ofs -> Conditional_jump (References_equal, (s4_of_s2 ofs))
  | Instruction.IF_ACMPNE ofs -> Conditional_jump (References_not_equal, (s4_of_s2 ofs))
  | Instruction.IF_ICMPEQ ofs -> Conditional_jump (Integers_equal, (s4_of_s2 ofs))
  | Instruction.IF_ICMPGE ofs -> Conditional_jump (Integers_greater_or_equal, (s4_of_s2 ofs))
  | Instruction.IF_ICMPGT ofs -> Conditional_jump (Integers_greater, (s4_of_s2 ofs))
  | Instruction.IF_ICMPLE ofs -> Conditional_jump (Integers_lower_or_equal, (s4_of_s2 ofs))
  | Instruction.IF_ICMPLT ofs -> Conditional_jump (Integers_lower, (s4_of_s2 ofs))
  | Instruction.IF_ICMPNE ofs -> Conditional_jump (Integers_not_equal, (s4_of_s2 ofs))
  | Instruction.IFEQ ofs -> Conditional_jump (Integer_equal_zero, (s4_of_s2 ofs))
  | Instruction.IFGE ofs -> Conditional_jump (Integer_greater_or_equal_zero, (s4_of_s2 ofs))
  | Instruction.IFGT ofs -> Conditional_jump (Integer_greater_zero, (s4_of_s2 ofs))
  | Instruction.IFLE ofs -> Conditional_jump (Integer_lower_or_equal_zero, (s4_of_s2 ofs))
  | Instruction.IFLT ofs -> Conditional_jump (Integer_lower_zero, (s4_of_s2 ofs))
  | Instruction.IFNE ofs -> Conditional_jump (Integer_not_equal_zero, (s4_of_s2 ofs))
  | Instruction.IFNONNULL ofs -> Conditional_jump (Reference_not_null, (s4_of_s2 ofs))
  | Instruction.IFNULL ofs -> Conditional_jump (Reference_null, (s4_of_s2 ofs))
  | Instruction.IINC _ -> Next_instruction
  | Instruction.ILOAD _ -> Next_instruction
  | Instruction.ILOAD_0 -> Next_instruction
  | Instruction.ILOAD_1 -> Next_instruction
  | Instruction.ILOAD_2 -> Next_instruction
  | Instruction.ILOAD_3 -> Next_instruction
  | Instruction.IMUL -> Next_instruction
  | Instruction.INEG -> Next_instruction
  | Instruction.INSTANCEOF _ -> Next_instruction
  | Instruction.INVOKEDYNAMIC _ -> Called_method
  | Instruction.INVOKEINTERFACE _ -> Called_method
  | Instruction.INVOKESPECIAL _ -> Called_method
  | Instruction.INVOKESTATIC _ -> Called_method
  | Instruction.INVOKEVIRTUAL _ -> Called_method
  | Instruction.IOR -> Next_instruction
  | Instruction.IREM -> Next_instruction
  | Instruction.IRETURN -> Calling_method Integer
  | Instruction.ISHL -> Next_instruction
  | Instruction.ISHR -> Next_instruction
  | Instruction.ISTORE _ -> Next_instruction
  | Instruction.ISTORE_0 -> Next_instruction
  | Instruction.ISTORE_1 -> Next_instruction
  | Instruction.ISTORE_2 -> Next_instruction
  | Instruction.ISTORE_3 -> Next_instruction
  | Instruction.ISUB -> Next_instruction
  | Instruction.IUSHR -> Next_instruction
  | Instruction.IXOR -> Next_instruction
  | Instruction.JSR _ -> fail (Unsupported_instruction "JSR")
  | Instruction.JSR_W _ -> fail (Unsupported_instruction "JSR_W")
  | Instruction.L2D -> Next_instruction
  | Instruction.L2F -> Next_instruction
  | Instruction.L2I -> Next_instruction
  | Instruction.LADD -> Next_instruction
  | Instruction.LALOAD -> Next_instruction
  | Instruction.LAND -> Next_instruction
  | Instruction.LASTORE -> Next_instruction
  | Instruction.LCMP -> Next_instruction
  | Instruction.LCONST_0 -> Next_instruction
  | Instruction.LCONST_1 -> Next_instruction
  | Instruction.LDC _ -> Next_instruction
  | Instruction.LDC2_W _ -> Next_instruction
  | Instruction.LDC_W _ -> Next_instruction
  | Instruction.LDIV -> Next_instruction
  | Instruction.LLOAD _ -> Next_instruction
  | Instruction.LLOAD_0 -> Next_instruction
  | Instruction.LLOAD_1 -> Next_instruction
  | Instruction.LLOAD_2 -> Next_instruction
  | Instruction.LLOAD_3 -> Next_instruction
  | Instruction.LMUL -> Next_instruction
  | Instruction.LNEG -> Next_instruction
  | Instruction.LOOKUPSWITCH (default, _, l) -> let k, o = List.split l in Switch_jump ((Lookup k), default, o)
  | Instruction.LOR -> Next_instruction
  | Instruction.LREM -> Next_instruction
  | Instruction.LRETURN -> Calling_method Long
  | Instruction.LSHL -> Next_instruction
  | Instruction.LSHR -> Next_instruction
  | Instruction.LSTORE _ -> Next_instruction
  | Instruction.LSTORE_0 -> Next_instruction
  | Instruction.LSTORE_1 -> Next_instruction
  | Instruction.LSTORE_2 -> Next_instruction
  | Instruction.LSTORE_3 -> Next_instruction
  | Instruction.LSUB -> Next_instruction
  | Instruction.LUSHR -> Next_instruction
  | Instruction.LXOR -> Next_instruction
  | Instruction.MONITORENTER -> Next_instruction
  | Instruction.MONITOREXIT -> Next_instruction
  | Instruction.MULTIANEWARRAY _ -> Next_instruction
  | Instruction.NEW _ -> Next_instruction
  | Instruction.NEWARRAY _ -> Next_instruction
  | Instruction.NOP -> Next_instruction
  | Instruction.POP -> Next_instruction
  | Instruction.POP2 -> Next_instruction
  | Instruction.PUTFIELD _ -> Next_instruction
  | Instruction.PUTSTATIC _ -> Next_instruction
  | Instruction.RET _ -> fail (Unsupported_instruction "RET")
  | Instruction.RETURN -> Calling_method Void
  | Instruction.SALOAD -> Next_instruction
  | Instruction.SASTORE -> Next_instruction
  | Instruction.SIPUSH _ -> Next_instruction
  | Instruction.SWAP -> Next_instruction
  | Instruction.TABLESWITCH (default, low, high, l) -> Switch_jump (Table (low, high), default, l)
  | Instruction.WIDE_ALOAD _ -> Next_instruction
  | Instruction.WIDE_ASTORE _ -> Next_instruction
  | Instruction.WIDE_DLOAD _ -> Next_instruction
  | Instruction.WIDE_DSTORE _ -> Next_instruction
  | Instruction.WIDE_FLOAD _ -> Next_instruction
  | Instruction.WIDE_FSTORE _ -> Next_instruction
  | Instruction.WIDE_IINC _ -> Next_instruction
  | Instruction.WIDE_ILOAD _ -> Next_instruction
  | Instruction.WIDE_ISTORE _ -> Next_instruction
  | Instruction.WIDE_LLOAD _ -> Next_instruction
  | Instruction.WIDE_LSTORE _ -> Next_instruction
  | Instruction.WIDE_RET _ -> fail (Unsupported_instruction "WIDE_RET")


(* Flow information for methods *)

type id = int array

let get_id =
  let next_id = ref [| 0 |] in
  fun () ->
    let res = Array.copy !next_id in
    let len = Array.length !next_id in
    let i = ref (pred len) in
    while (!i >= 0) && (!next_id).(!i) = max_int do
      decr i
    done;
    if !i < 0 then
      next_id := Array.init (succ len) (fun _ -> 0)
    else
      (!next_id).(!i) <- succ (!next_id).(!i);
    res

module IdMap = Map.Make (struct type t = id let compare = Pervasives.compare end)

module IdSet = Set.Make (struct type t = id let compare = Pervasives.compare end)

type 'a edge =
  | Unconditional_edge of 'a
  | Conditional_edge of jump_kind * 'a * 'a
  | Switch_edge of (switch_kind * 'a * 'a list)

let check_edge = function
  | Unconditional_edge _ -> ()
  | Conditional_edge _ -> ()
  | Switch_edge (Table (low, high), _, offsets) ->
      let table_sz = Int32.succ (Int32.sub (high :> int32) (low :> int32)) in
      if table_sz <> (Int32.of_int (List.length offsets)) then
        fail Invalid_switch_edge
  | Switch_edge (Lookup keys, _, offsets) ->
      if (List.length keys) <> (List.length offsets) then
        fail Invalid_switch_edge

let map_edge f = function
  | Unconditional_edge x -> Unconditional_edge (f x)
  | Conditional_edge (x, y, z) -> Conditional_edge (x, f y, f z)
  | Switch_edge (x, y, l) -> Switch_edge (x, f y, List.map f l)

let forall_edge p = function
  | Unconditional_edge x -> p x
  | Conditional_edge (_, x, y) -> (p x) && (p y)
  | Switch_edge (_, x, l) -> (p x) && (List.for_all p l)

let exists_edge p = function
  | Unconditional_edge x -> p x
  | Conditional_edge (_, x, y) -> (p x) || (p y)
  | Switch_edge (_, x, l) -> (p x) || (List.exists p l)

type 'a vertex = {
    vertex_id : id;
    vertex_instrs : Instruction.t list;
    vertex_label : 'a;
} and ('a, 'b) graph = {
    graph_vertices : 'a vertex IdMap.t;
    graph_root : id;
    graph_edges : ('b * id edge) IdMap.t;
    graph_handlers : ('b * (Name.for_class option) * id) list IdMap.t;
}

let equal_vertex x y =
  x.vertex_id = y.vertex_id

let equal_edge eq x y =
  match (x, y) with
  | (Unconditional_edge vx), (Unconditional_edge vy) -> eq vx vy
  | (Conditional_edge (jkx, d1x, d2x)), (Conditional_edge (jky, d1y, d2y)) ->
      (jkx = jky) && (eq d1x d1y) && (eq d2x d2y)
  | (Switch_edge (skx, dx, dlx)), (Switch_edge (sky, dy, dly)) ->
      (skx = sky) && (eq dx dy)
        && ((List.length dlx) = (List.length dly))
        && (List.for_all2 eq dlx dly)
  | _ -> false

let make_graph instrs lbl =
  let id = get_id () in
  let root = { vertex_id = id; vertex_instrs = instrs; vertex_label = lbl; } in
  let vertices = IdMap.add id root IdMap.empty in
  let graph = { graph_vertices = vertices;
                graph_root = id;
                graph_edges = IdMap.empty;
                graph_handlers = IdMap.empty; } in
  graph, root

let contains_vertex g v =
  IdMap.mem v.vertex_id g.graph_vertices

let add_vertex g instrs lbl =
  let id = get_id () in
  let vertex = { vertex_id = id; vertex_instrs = instrs; vertex_label = lbl; } in
  let graph = { g with graph_vertices = IdMap.add id vertex g.graph_vertices; } in
  graph, vertex

let map_edge_to_id e =
  map_edge (fun v -> v.vertex_id) e

let map_edge_to_vertex g e =
  map_edge (fun i -> IdMap.find i g.graph_vertices) e

let add_edge g v e lbl =
  check_edge e;
  if not ((contains_vertex g v) && (forall_edge (contains_vertex g) e)) then
    fail Vertex_not_in_graph
  else
    { g with graph_edges = IdMap.add v.vertex_id (lbl, (map_edge_to_id e)) g.graph_edges; }

let same_class cn1 cn2 =
  match cn1, cn2 with
  | (Some n1), (Some n2) -> Name.equal_for_class n1 n2
  | None, None -> true
  | Some _, None | None, Some _ -> false

let add_handler g v cn h lbl =
  if not ((contains_vertex g v) && (contains_vertex g h)) then
    fail Vertex_not_in_graph
  else
    let curr = try IdMap.find v.vertex_id g.graph_handlers with Not_found -> [] in
    let curr = List.filter (fun (_, cn', _) -> not (same_class cn cn')) curr in
    { g with graph_handlers = IdMap.add v.vertex_id ((lbl, cn, h.vertex_id) :: curr) g.graph_handlers; }

let change_root g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else
    { g with graph_root = v.vertex_id; }

let remove_vertex g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else if v.vertex_id = g.graph_root then
    fail Cannot_remove_root
  else
    let vertices = IdMap.remove v.vertex_id g.graph_vertices in
    let edges = IdMap.remove v.vertex_id g.graph_edges in
    let edges = IdMap.fold (fun k (l, e) acc -> if exists_edge (fun x -> x = v.vertex_id) e then acc else IdMap.add k (l, e) acc) edges IdMap.empty in
    let handlers = IdMap.remove v.vertex_id g.graph_handlers in
    let handlers =
      IdMap.fold
        (fun k l acc->
          let l' =
            List.filter
              (fun (_, _, i') -> i' <> v.vertex_id)
              l in
          if l' = [] then acc else IdMap.add k l' acc)
        handlers
        IdMap.empty in
    { g with graph_vertices = vertices; graph_edges = edges; graph_handlers = handlers; }

let remove_vertices g vl =
  List.iter
    (fun v ->
      if not (contains_vertex g v) then
        fail Vertex_not_in_graph
      else if v.vertex_id = g.graph_root then
        fail Cannot_remove_root)
    vl;
  let vl = List.map (fun x -> x.vertex_id) vl in
  let vertices, edges, handlers =
    List.fold_left
      (fun (vertices, edges, handlers) elem ->
        IdMap.remove elem vertices,
        IdMap.remove elem edges,
        IdMap.remove elem handlers)
      (g.graph_vertices, g.graph_edges, g.graph_handlers)
      vl in
  let edges =
    IdMap.fold
      (fun k (l, e) acc ->
        if exists_edge (fun x -> List.mem x vl) e then
          acc
        else
          IdMap.add k (l, e) acc)
      edges
      IdMap.empty in
  let handlers =
    IdMap.fold
      (fun k l acc->
        let l' =
          List.filter
            (fun (_, _, i') -> not (List.mem i' vl))
            l in
        if l' = [] then acc else IdMap.add k l' acc)
      handlers
      IdMap.empty in
    { g with graph_vertices = vertices; graph_edges = edges; graph_handlers = handlers; }

let remove_edge g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else if IdMap.mem v.vertex_id g.graph_edges then
    { g with graph_edges = IdMap.remove v.vertex_id g.graph_edges; }
  else
    g

let remove_handler g v cn v' =
  if not ((contains_vertex g v) && (contains_vertex g v')) then
    fail Vertex_not_in_graph
  else
    try
      let handlers = IdMap.find v.vertex_id g.graph_handlers in
      let handlers = List.filter (fun (_, cn', i) -> not ((same_class cn cn') && (i = v'.vertex_id))) handlers in
      if handlers <> [] then
        { g with graph_handlers = IdMap.add v.vertex_id handlers g.graph_handlers; }
      else
        { g with graph_handlers = IdMap.remove v.vertex_id g.graph_handlers; }
    with Not_found -> g

let root_of_graph g =
  IdMap.find g.graph_root g.graph_vertices

let instructions_of_vertex v =
  v.vertex_instrs

let label_of_vertex v =
  v.vertex_label

let edge_of_vertex g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else
    let lbl, e = IdMap.find v.vertex_id g.graph_edges in
    lbl, map_edge_to_vertex g e

let predecessors_of_vertex g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else
    let res =
      IdMap.fold
        (fun k (_, e) acc ->
          if exists_edge (fun x -> x = v.vertex_id) e then
            IdSet.add k acc
          else
            acc)
        g.graph_edges
        IdSet.empty in
    List.map (fun x -> IdMap.find x g.graph_vertices) (IdSet.elements res)

let handlers_of_vertex g v =
  if not (contains_vertex g v) then
    fail Vertex_not_in_graph
  else
    try
      List.map
        (fun (x, y, z) -> (x, y, IdMap.find z g.graph_vertices))
        (IdMap.find v.vertex_id g.graph_handlers)
    with Not_found -> []

let iter_vertices f g =
  IdMap.iter (fun k _ -> f (IdMap.find k g.graph_vertices)) g.graph_vertices

let iter_edges f g =
  IdMap.iter
    (fun k (v1, v2) -> f (IdMap.find k g.graph_vertices) v1 (map_edge_to_vertex g v2))
    g.graph_edges

let iter_handlers f g =
  IdMap.iter
    (fun k l ->
      List.iter
        (fun (v1, v2, v3) ->
          f (IdMap.find k g.graph_vertices) v1 v2 (IdMap.find v3 g.graph_vertices)) l)
    g.graph_handlers

let fold_vertices f g z =
  IdMap.fold
    (fun _ v acc -> f v acc)
    g.graph_vertices
    z

let fold_edges f g z =
  IdMap.fold
    (fun i (l, e) acc ->
      let v = IdMap.find i g.graph_vertices in
      f v l (map_edge_to_vertex g e) acc)
    g.graph_edges
    z

let fold_handlers f g z =
  IdMap.fold
    (fun i l acc ->
      let v = IdMap.find i g.graph_vertices in
      List.fold_left
        (fun acc (lbl, cn, i') ->
          let v' = IdMap.find i' g.graph_vertices in
          f v lbl cn v' acc)
        acc
        l)
    g.graph_handlers
    z

let map_graph f1 f2 f3 g =
  let vertices, conv =
    IdMap.fold
      (fun id v (acc_v, acc_c) ->
        let id' = get_id () in
        let lbl', instrs' = f1 v.vertex_label v.vertex_instrs in
        let v' = { vertex_id = id'; vertex_instrs = instrs'; vertex_label = lbl'; } in
        (IdMap.add id' v' acc_v), (IdMap.add id id' acc_c))
      g.graph_vertices
      (IdMap.empty, IdMap.empty) in
  let edges =
    IdMap.fold
      (fun id (lbl, e) acc ->
        let id' = IdMap.find id conv in
        let lbl' = f2 lbl (map_edge_to_vertex g e) in
        IdMap.add id' (lbl', map_edge (fun i -> IdMap.find i conv) e) acc)
      g.graph_edges
      IdMap.empty in
  let handlers =
    IdMap.fold
      (fun id1 l acc ->
        let id1' = IdMap.find id1 conv in
        let l' = List.map
          (fun (lbl, cn, id2) ->
            let lbl' = f3 lbl cn (IdMap.find id2 g.graph_vertices) in
            let id2' = IdMap.find id2 conv in
            (lbl', cn, id2'))
          l in
        IdMap.add id1' l' acc)
      g.graph_handlers
      IdMap.empty in
  { graph_vertices = vertices;
    graph_root = IdMap.find g.graph_root conv;
    graph_edges = edges;
    graph_handlers = handlers; }

let identity g =
  map_graph
    (fun x y -> x, y)
    (fun x _ -> x)
    (fun x _ _ -> x)
    g

let escape_xml s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '\'' -> Buffer.add_string buf "&apos;"
    | '\"' -> Buffer.add_string buf "&quot;"
    | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let dot_of_graph f1 f2 g =
  let string_of_node_id id =
    "node" ^ (Array.fold_left (fun acc elem -> acc ^ "_" ^ (string_of_int elem)) "" id) in
  let string_of_instruction i =
    let _, wide, mnemo, p, _ = Instruction.decompile 0 i in
    let b = Buffer.create 128 in
    let add_class_name cn = Buffer.add_string b (UTF8.to_string (Name.external_utf8_for_class cn)) in
    let add_field_name fn = Buffer.add_string b (UTF8.to_string (Name.utf8_for_field fn)) in
    let add_method_name fn = Buffer.add_string b (UTF8.to_string (Name.utf8_for_method fn)) in
    let add_type t = Buffer.add_string b (UTF8.to_string (Descriptor.external_utf8_of_java_type t)) in
    let add_point () = Buffer.add_char b '.' in
    let add_comma () = Buffer.add_char b ',' in
    let add_field_descriptor cls name =
      add_class_name cls;
      add_point ();
      add_field_name name in
    let add_meth_descriptor (params, ret) =
      Buffer.add_char b '(';
      let first = ref true in
      List.iter
        (fun x ->
          (if !first then first := false else add_comma ());
          add_type (x :> Descriptor.java_type))
        params;
      Buffer.add_char b ')';
      Buffer.add_char b ':';
      add_type ret in
    (if wide then Buffer.add_string b "wide ");
    Buffer.add_string b mnemo;
    List.iter
      (fun p ->
        Buffer.add_char b ' ';
        match p with
        | Instruction.Int_constant x -> Buffer.add_string b (Int64.to_string x)
        | Instruction.Offset x -> Buffer.add_string b (Int32.to_string x)
        | Instruction.Float_constant x -> Buffer.add_string b (string_of_float x)
        | Instruction.String_constant x -> Buffer.add_string b (UTF8.to_string (UTF8.escape x))
        | Instruction.Class_name x -> add_class_name x
        | Instruction.Array_type x -> Buffer.add_string b (UTF8.to_string x)
        | Instruction.Primitive_type x -> add_type x
        | Instruction.Field (x, y, _) -> add_field_descriptor x y
        | Instruction.Dynamic_method (_, x, y) -> add_method_name x; add_meth_descriptor y
        | Instruction.Method (x, y, z) -> add_class_name x; add_point(); add_method_name y; add_meth_descriptor z
        | Instruction.Array_method (x, y, z) -> add_type (x :> Descriptor.java_type); add_point (); add_method_name y; add_meth_descriptor z
        | Instruction.Method_type_constant x -> add_meth_descriptor x
        | Instruction.Method_handle_constant x ->
          let add = Buffer.add_string b in
          (match x with
          | `getField (x, y, _) -> add "getField%"; add_field_descriptor x y
          | `getStatic (x, y, _) -> add "getStatic%"; add_field_descriptor x y
          | `putField (x, y, _) -> add "putField%"; add_field_descriptor x y
          | `putStatic (x, y, _) -> add "putStatic%"; add_field_descriptor x y
          | `invokeVirtual (x, y, z) -> add "invokeVirtual%"; add_class_name x; add_point(); add_method_name y; add_meth_descriptor z
          | `invokeStatic (x, y, z) -> add "invokeStatic%"; add_class_name x; add_point(); add_method_name y; add_meth_descriptor z
          | `invokeSpecial (x, y, z) -> add "invokeSpecial%"; add_class_name x; add_point(); add_method_name y; add_meth_descriptor z
          | `newInvokeSpecial (x, y) -> add "newInvokeSpecial%"; add_meth_descriptor (y, `Class x)
          | `invokeInterface (x, y, z) -> add "invokeInterface%"; add_class_name x; add_point(); add_method_name y; add_meth_descriptor z))
      p;
    Buffer.contents b in
  let buf = Buffer.create 1024 in
  let add_node name shape root lbl =
    Buffer.add_string buf "  ";
    Buffer.add_string buf name;
    Buffer.add_string buf " [shape=";
    Buffer.add_string buf shape;
    Buffer.add_string buf ",";
    (if root then Buffer.add_string buf "color=blue,");
    Buffer.add_string buf "label=<";
    Buffer.add_string buf lbl;
    Buffer.add_string buf ">]\n" in
  let add_edge src dst lbl dotted =
    Buffer.add_string buf "  ";
    Buffer.add_string buf src;
    Buffer.add_string buf " -> ";
    Buffer.add_string buf dst;
    Buffer.add_string buf " [label=<";
    Buffer.add_string buf (if lbl = "" then "-" else lbl);
    Buffer.add_string buf ">";
    (if dotted then Buffer.add_string buf ",style=dotted");
    Buffer.add_string buf "]\n" in
  Buffer.add_string buf "digraph {\n";
  IdMap.iter
    (fun id v ->
      let instrs = String.concat "<br/>" (List.map (fun x -> escape_xml (string_of_instruction x)) (instructions_of_vertex v)) in
      let lbl = f1 (label_of_vertex v) in
      let lbl = (if lbl = "" then "-" else (escape_xml lbl)) ^ "<br/>" ^ instrs in
      add_node
        (string_of_node_id id)
        "box"
        (id = g.graph_root)
        lbl)
    g.graph_vertices;
  let rec itv acc x y = if y < x then acc else itv (y :: acc) x (s4_pred y) in
  IdMap.iter
    (fun id (lbl, e) ->
      let lbl = f2 lbl in
      let lbl = if lbl = "" then "-" else escape_xml lbl in
      match e with
      | Unconditional_edge id' ->
          add_edge (string_of_node_id id) (string_of_node_id id') lbl false
      | Conditional_edge (k, id1, id2) ->
          let cond = (string_of_node_id id) ^ "_cond" in
          add_node cond "circle" false ((string_of_jump_kind k) ^ "<br/>" ^ lbl);
          add_edge (string_of_node_id id) cond "" false;
          add_edge cond (string_of_node_id id1) "true" false;
          add_edge cond (string_of_node_id id2) "false" false
      | Switch_edge (k, d, ids) ->
          let cond = (string_of_node_id id) ^ "_cond" in
          add_node cond "circle" false ((string_of_switch_kind k) ^ "<br/>" ^ lbl);
          add_edge (string_of_node_id id) cond "" false;
          add_edge cond (string_of_node_id d) "default" false;
          let consts = match k with Table (x, y) -> itv [] x y | Lookup l -> l in
          List.iter2
            (fun c i ->
              add_edge cond (string_of_node_id i) (Int32.to_string c) false)
            (consts :> int32 list)
            ids)
    g.graph_edges;
  IdMap.iter
    (fun id l ->
      List.iter
        (fun (lbl, cn, id') ->
          let lbl = f2 lbl in
          let lbl = if lbl = "" then "-" else escape_xml lbl in
          let lbl = lbl ^ "<br/>" ^ (match cn with Some x -> UTF8.to_string (Name.external_utf8_for_class x) | None -> "-") in
          add_edge (string_of_node_id id) (string_of_node_id id') lbl true)
        l)
    g.graph_handlers;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

module Int32Set = Set.Make (struct type t = int32 let compare = Pervasives.compare end)

module Int32Map = Map.Make (struct type t = int32 let compare = Pervasives.compare end)

type line_mapper = u2 -> u2

let dummy_mapper _ = u2 0

let line_number_table_mapper lnt =
  let lnt = List.sort (fun x y -> -(compare x y)) lnt in
  fun ofs ->
    try
      snd (List.find (fun (x, _) -> x <= ofs) lnt)
    with Not_found -> u2 0

let graph_of_instructions ?(line_mapper = dummy_mapper) instrs exception_table =
  let (++) = Int32.add in
  let (+++) (x : int32) (y : s4) = Int32.add x (y :> int32) in
  (* step 1: label instructions with offsets, compute cut offsets from instructions *)
  let _, instrs_with_offsets, cut_offsets =
    List.fold_left
      (fun (ofs, lst, cut) instr ->
        let sz = Int32.of_int (Instruction.size_of (Int32.to_int ofs) instr) in
        let next_ofs = ofs ++ sz in
        let cut = match for_instruction instr with
        | Next_instruction -> cut
        | Unconditional_jump dest_ofs -> Int32Set.add (ofs +++ dest_ofs) (Int32Set.add next_ofs cut)
        | Conditional_jump (_, dest_ofs) -> Int32Set.add (ofs +++ dest_ofs) (Int32Set.add next_ofs cut)
        | Switch_jump (_, default_ofs, offsets) ->
            List.fold_left
              (fun acc elem -> Int32Set.add (ofs +++ elem) acc)
              (Int32Set.add (ofs +++ default_ofs) (Int32Set.add next_ofs cut))
              offsets
        | Called_method -> cut
        | Calling_method _ -> Int32Set.add next_ofs cut
        | Exception_handler -> Int32Set.add next_ofs cut in
        (next_ofs, (ofs, instr) :: lst, cut))
      (0l, [], Int32Set.empty)
      instrs in
  let instrs_with_offsets = List.rev instrs_with_offsets in
  let instrs_with_offsets =
    List.map
      (fun (x, y) ->
        (x, line_mapper (u2 (Int32.to_int x)), y))
      instrs_with_offsets in
  (* step 2: add cut offsets from exception handlers *)
  let cut_offsets =
    List.fold_left
      (fun acc elem ->
        let acc = Int32Set.add (Int32.of_int (elem.Attribute.try_start : u2 :> int)) acc in
        let acc = Int32Set.add (Int32.of_int (elem.Attribute.try_end  : u2 :> int)) acc in
        Int32Set.add (Int32.of_int (elem.Attribute.catch : u2 :> int)) acc)
      cut_offsets
      exception_table in
  let cut_offsets = Int32Set.remove 0l cut_offsets in
  (* step 3: split instruction list according to cut offsets *)
  let rec split acc_ofs acc_block acc_res = function
    | (ofs, line, instr) :: tl ->
        if Int32Set.mem ofs cut_offsets then
          split ofs [ofs, line, instr] ((acc_ofs, acc_block) :: acc_res) tl
        else
          split acc_ofs ((ofs, line, instr) :: acc_block) acc_res tl
    | [] ->
        let res = (acc_ofs, acc_block) :: acc_res in
        List.rev res in
  let blocks = split 0l [] [] instrs_with_offsets in
  let blocks =
    List.map
      (fun (offset, instrs) ->
        match instrs with
        | (ofs, _, instr) :: tl ->
          (match for_instruction instr with
          | Unconditional_jump _
          | Conditional_jump _
          | Switch_jump _ -> offset, List.rev tl, Some (ofs, instr)
          | Next_instruction
          | Called_method -> offset, List.rev instrs, Some (ofs, instr)
          | Calling_method _
          | Exception_handler -> offset, List.rev instrs, None)
        | [] -> offset, [], None)
      blocks in
  (* step 4: actually build graph *)
  let thd3 (_, _, x) = x in
  let snd3 (_, x, _) = x in
  match blocks with
  | (hd_ofs, hd_instrs, _) :: tl ->
    let graph, root = make_graph (List.map thd3 hd_instrs) (hd_ofs, (List.map snd3 hd_instrs)) in
    let graph, map =
      List.fold_left
        (fun (g, m) (ofs, instrs, _) ->
          let g, v = add_vertex g (List.map thd3 instrs) (ofs, (List.map snd3 instrs)) in
          (g, (Int32Map.add ofs v m)))
        (graph, (Int32Map.add 0l root Int32Map.empty))
        tl in
    let get_vertex x = Int32Map.find x map in
    let graph =
      List.fold_left
        (fun acc (ofs_block, _, last) ->
          match last with
          | Some (ofs_instr, instr) ->
            (match for_instruction instr with
            | Unconditional_jump d -> add_edge acc (get_vertex ofs_block) (Unconditional_edge (get_vertex (ofs_instr +++ d))) ()
            | Conditional_jump (k, d) ->
              let ifso = get_vertex (ofs_instr +++ d) in
              let ifno = get_vertex (ofs_instr ++ (Int32.of_int (Instruction.size_of (Int32.to_int ofs_instr) instr))) in
              add_edge acc (get_vertex ofs_block) (Conditional_edge (k, ifso, ifno)) ()
            | Switch_jump (k, d, offsets) ->
              let b = List.map (fun o -> get_vertex (ofs_instr +++ o)) offsets in
              let e = Switch_edge (k, (get_vertex (ofs_instr +++ d)), b) in
              add_edge acc (get_vertex ofs_block) e ()
            | Next_instruction
            | Called_method ->
              let next = get_vertex (ofs_instr ++ (Int32.of_int (Instruction.size_of (Int32.to_int ofs_instr) instr))) in
              add_edge acc (get_vertex ofs_block) (Unconditional_edge next) ()
            | Calling_method _ -> acc
            | Exception_handler -> acc)
          | None -> acc)
        graph
        blocks in
    let inside x1 y1 x2 y2 =
      (x1 >= x2) && (y1 <= y2) in
    let exception_table =
      List.sort
        (fun x y ->
          let start1 = x.Attribute.try_start in
          let end1 = x.Attribute.try_end in
          let start2 = y.Attribute.try_start in
          let end2 = y.Attribute.try_end in
          if inside start1 end1 start2 end2 then
            1
          else if inside start2 end2 start1 end1 then
            -1
          else
            compare x y)
        exception_table in
    let graph =
      List.fold_left
        (fun g elem ->
          let handler = get_vertex (Int32.of_int (elem.Attribute.catch : u2 :> int)) in
          Int32Map.fold
            (fun ofs v acc ->
              if (ofs >= (Int32.of_int (elem.Attribute.try_start : u2 :> int)))
                  && (ofs < (Int32.of_int (elem.Attribute.try_end : u2 :> int))) then
                add_handler acc v elem.Attribute.caught handler ()
              else
                acc)
            map
            g)
        graph
        exception_table in
    graph
  | [] -> fail Empty_graph
