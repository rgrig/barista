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


(** Types and functions related to control flow information.

    {b Warning:} although graphs expose a functional interface, their
    current implementation is based on identifiers for the vertices.
    While this is not observable for most programs, this may lead to
    inconsistent behaviour when graphs are marshalled and then
    unmarshalled. To ensure that an unmarshalled graph can be used
    with no risk of identifier collision, one should call [identity]
    on any unmarshlled graph. *)


(** {6 Base types} *)

type return_kind =
  | Reference (** Indicates that the instruction returns a reference. *)
  | Double (** Indicates that the instruction returns a double. *)
  | Float (** Indicates that the instruction returns a float. *)
  | Integer (** Indicates that the instruction returns an integer. *)
  | Long (** Indicates that the instruction returns a long. *)
  | Void (** Indicates that the instruction returns nothing. *)
(** The type representing the kind of values returned by a {i x}return instruction. *)

type jump_kind =
  | References_equal (** Indicates that the jump will be performed if the references are equal. *)
  | References_not_equal (** Indicates that the jump will be performed if the references are different. *)
  | Integers_equal (** Indicates that the jump will be performed if the integers are equal. *)
  | Integers_greater_or_equal (** Indicates that the jump will be performed if the first integer is greater than or equal to the second one. *)
  | Integers_greater (** Indicates that the jump will be performed if the first integer is greater than the second one. *)
  | Integers_lower_or_equal (** Indicates that the jump will be performed if the first integer is lower than or equal to the second one. *)
  | Integers_lower (** Indicates that the jump will be performed if the first integer is lower than the second one. *)
  | Integers_not_equal (** Indicates that the jump will be performed if the integers are different. *)
  | Integer_equal_zero (** Indicates that the jump will be performed if the integer is equal to zero. *)
  | Integer_greater_or_equal_zero (** Indicates that the jump will be performed if the integer is greater than or equal to zero. *)
  | Integer_greater_zero (** Indicates that the jump will be performed if the integer is greater than zero. *)
  | Integer_lower_or_equal_zero (** Indicates that the jump will be performed if the integer is lower than or equal to zero. *)
  | Integer_lower_zero (** Indicates that the jump will be performed if the integer is lower than zero. *)
  | Integer_not_equal_zero (** Indicates that the jump will be performed if the integer is different from zero. *)
  | Reference_null (** Indicates that the jump will be performed if the reference is {i null}. *)
  | Reference_not_null (** Indicates that the jump will be performed if the reference is not {i null}. *)
(** The type representing the kind of condition associated with a jump instruction. *)

val nb_args_of_jump_kind : jump_kind -> int
(** Returns the number of arguments used by the passed jump kind. *)

val opposite_jump_kind : jump_kind -> jump_kind
(** Returns the jump kind that is the opposite ({i i. e.} negation) of
    the passed one. *)

type switch_kind =
  | Table of Utils.s4 * Utils.s4 (** Indicates that the switch is a table switch (parameters being lower and upper bounds). *)
  | Lookup of Utils.s4 list (** Indicates that the switch is a lookup switch (parameter being the list of values). *)
(** The type representing the values associated with a switch instruction. *)

val nb_args_of_switch_kind : switch_kind -> int
(** Returns the number of arguments used by the passed switch kind. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Unsupported_instruction of string
  | Vertex_not_in_graph
  | Invalid_switch_edge
  | Empty_graph
  | Cannot_remove_root


(** {6 Flow information for instructions} *)

type for_instruction =
  | Next_instruction (** Indicates that the execution will continue with the following instruction. *)
  | Unconditional_jump of Utils.s4 (** Indicates that the execution will continue with the instruction at the specified offset. *)
  | Conditional_jump of jump_kind * Utils.s4 (** Indicates that the execution will continue with either the following instruction or the one at the specified offset, according to the kind of jump. *)
  | Switch_jump of (switch_kind * Utils.s4 * Utils.s4 list) (** Indicates that the execution will continue to an instruction specified by the switch kind (other parameters being default offset and list of offsets). *)
  | Called_method (** Indicates that the execution will continue in the method called by the instruction. *)
  | Calling_method of return_kind (** Indicates that the execution will continue in the calling method (parameter indicating the type of the returned value). *)
  | Exception_handler (** Indicates that the execution will continue in an exception handler (the instruction unconditionally throws an exception). *)
(** The type representing control flow information for an instruction. *)

val for_instruction : Instruction.t -> for_instruction
(** Returns the control flow information for the passed instruction.
    Raises [Exception] if the instruction is unsupported ({i jsr},
    {i jsr_w}, {i ret}, or {i wide ret}). *)


(** {6 Flow information for methods} *)

(** {7 Graph definition} *)

type 'a vertex
(** The type of graph vertices, each being labeled with an instruction
    list and a value of type ['a]. *)

type 'a edge =
  | Unconditional_edge of 'a (** Binary edge. *)
  | Conditional_edge of jump_kind * 'a * 'a (** Ternary edge labeled with jump kind (first destination is chosen when condition is true). *)
  | Switch_edge of (switch_kind * 'a * 'a list) (** N-ary edge labeled with switch kind, default destination, and destinations for values given by switch kind. *)
(** The type of graph edges, ['a] being the type of edge destinations. *)

type ('a, 'b) graph
(** The type of control flow graphs for methods (actually
    {i hypergraphs}), ['a] and ['b] being respectively the types of
    vertex and edge labels. Graphs are applicative data structures.

    A control flow graph is defined by:
    - a non-empty set of vertices;
    - a designated vertex: the {i root} (representing the method entry
      point);
    - a set of edges representing the control flow of the method;
    - a set of handlers, that are special edges from protected vertices
      to associated exception handlers.

    The following properties hold:
    - there is at most one {i standard} outgoing edge for each vertex;
    - there is at most one {i special} outgoing edge for each (vertex, caught exception) couple. *)

(** {7 Graph utilities} *)

val equal_vertex : 'a vertex -> 'a vertex -> bool
(** Equality over vertices. *)

val equal_edge : ('a -> 'a -> bool) -> 'a edge -> 'a edge -> bool
(** Equality over edges. *)

(** {7 Graph constructor and accessors} *)

val make_graph : Instruction.t list -> 'a -> (('a, 'b) graph * 'a vertex)
(** [make_graph instrs lbl] returns both a graph reduced to its root and
    the aforementioned root. Instructions [instrs] and label [lbl] are
    associated with the root. *)

val contains_vertex : ('a, 'b) graph -> 'a vertex -> bool
(** Checks whether the passed graph contains the passed vertex. *)

val add_vertex : ('a, 'b) graph -> Instruction.t list -> 'a -> (('a, 'b) graph * 'a vertex)
(** [add_vertex g instrs lbl] returns a couple [g', v] where [g'] is [g]
    augmented with a new vertex [v] labeled with [instrs] and [lbl]. *)

val add_edge : ('a, 'b) graph -> 'a vertex -> 'a vertex edge -> 'b -> ('a, 'b) graph
(** [add_edge g v e l] returns a graph that is [g] augmented with a new
    edge [e] from [v] labeled with [l]. Raises [Exception] if either [v]
    or any vertex of [e] is not in [g], or if the edge is an invalid
    switch edge. *)

val add_handler : ('a, 'b) graph -> 'a vertex -> Name.for_class option -> 'a vertex -> 'b -> ('a, 'b) graph
(** [add_handler g src cn dst lbl] returns a graph that is [g] augmented
    with a handler from [src] to [dst] for exception [cn] labeled with
    [lbl].
    Raises [Exception] if either [src] of [dst] is not in [g]. *) 

val change_root : ('a, 'b) graph -> 'a vertex -> ('a, 'b) graph
(** Changes the root of the passed graph to the passed vertex.
    Raises [Exception] if the vertex is not in the graph. *)

val remove_vertex : ('a, 'b) graph -> 'a vertex -> ('a, 'b) graph
(** [remove_vertex g v] returns a graph that is [g] without [v] and
    associated edge and handlers.
    Raises [Exception] if [v] is not in [g]. *)

val remove_vertices : ('a, 'b) graph -> 'a vertex list -> ('a, 'b) graph
(** [remove_vertices g l] returns a graph that is [g] without the
    vertices [l] and associated edges and handlers. Equivalent to
    [remove_vertex] over all elements from [l], except that performances
    are much better.
    Raises [Exception] if any vertex from [l] is not in [g]. *) 

val remove_edge : ('a, 'b) graph -> 'a vertex -> ('a, 'b) graph
(** [remove_edge g v] returns a graph that is [g] without the edge going
    out of [v].
    Raises [Exception] if [v] is not in [g]. *) 

val remove_handler : ('a, 'b) graph -> 'a vertex -> Name.for_class option -> 'a vertex -> ('a, 'b) graph
(** [remove_handler g src cn dst] returns a graph that is [g] without the
    handler from [src] to [dst] for exception [cn].
    Raises [Exception] if either [src] of [dst] is not in [g]. *) 

val root_of_graph : ('a, 'b) graph -> 'a vertex
(** Returns the root of the passed graph. *)

val instructions_of_vertex : 'a vertex -> Instruction.t list
(** Returns the instruction list of the passed vertex. *)

val label_of_vertex : 'a vertex -> 'a
(** Returns the label of the passed vertex. *)

val edge_of_vertex : ('a, 'b) graph -> 'a vertex -> 'b * ('a vertex edge)
(** Returns the edge for the passed vertex in the passed graph, as well
    as the label associated with this edge.
    Raises [Exception] if the vertex is not in the graph.
    Raises [Not_found] if there is no edge associated with the vertex. *)

val predecessors_of_vertex : ('a, 'b) graph -> 'a vertex -> 'a vertex list
(** Returns the list of vertices that have the passed vertex as a
    destination.
    Raises [Exception] if the vertex is not in the graph. *)

val handlers_of_vertex : ('a, 'b) graph -> 'a vertex -> ('b * (Name.for_class option) * ('a vertex)) list
(** Returns the list of handlers for the passed vertex in the passed
    graph.
    Raises [Exception] if the vertex is not in the graph. *)

(** {7 Graph traversal} *)

val iter_vertices : ('a vertex -> unit) -> ('a, 'b) graph -> unit
(** [iter_vertices f g] applies [f] in turn to the vertices of [g]. *)

val iter_edges : ('a vertex -> 'b -> 'a vertex edge -> unit) -> ('a, 'b) graph -> unit
(** [iter_edges f g] applies [f] in turn to the edges of [g]. *)

val iter_handlers : ('a vertex -> 'b -> (Name.for_class option) -> 'a vertex -> unit) -> ('a, 'b) graph -> unit
(** [iter_handlers f g] applies [f] in turn to the handlers of [g]. *)

val fold_vertices : ('a vertex -> 'c -> 'c) -> ('a, 'b) graph -> 'c -> 'c
(** [fold_vertices f g z] is [f v1 (f v2 (... (f vn z) ...))] where the
    [vi] are the vertices of [g]. *)

val fold_edges : ('a vertex -> 'b -> 'a vertex edge -> 'c -> 'c) -> ('a, 'b) graph -> 'c -> 'c
(** [fold_edges f g z] is [f s1 l1 e1 (f s2 l2 e2 (... (f sn ln en z) ...))]
    where the [si, li, ei] are the edges of [g] (components being source,
    label and destination). *)

val fold_handlers : ('a vertex -> 'b -> (Name.for_class option) -> 'a vertex -> 'c -> 'c) -> ('a, 'b) graph -> 'c -> 'c
(** [fold_handlers f g z] is [f s1 l1 c1 h1 (f s2 l2 c2 h2 (... (f sn ln cn hn z) ...))]
    where the [si, li, ci, hi] are the handlers of [g] (components being
    source, label, caught exception, and handler). *)

val map_graph : ('a -> Instruction.t list -> 'c * Instruction.t list) -> ('b -> 'a vertex edge -> 'd) -> ('b -> Name.for_class option -> 'a vertex -> 'd) -> ('a, 'b) graph -> ('c, 'd) graph
(** [map_graph f1 f2 f3 g] maps [g] into a new graph structurally
    identical, using:
    - [f1] to map vertex information;
    - [f2] to map edge information;
    - [f3] to map handler information. *)

val identity : ('a, 'b) graph -> ('a, 'b) graph
(** [identity g] maps [g] into a new graph structurally identical, but
    with new identifiers for vertices.

    Useful to guarantee that an unmarshalled graph has correct vertex
    identifiers. *)

(** {7 Graph conversion to dot format} *)

val dot_of_graph : ('a -> string) -> ('b -> string) -> ('a, 'b) graph -> string
(** [dot_of_graph f1 f2 g] converts [g] into its dot representation
    using [f1] to convert vertex labels into strings, and [f2] to convert
    edge labels into strings. *)

(** {7 Graph construction from method elements} *)

type line_mapper = Utils.u2 -> Utils.u2
(** The type of functions mapping code offsets to associated source line
    numbers. *)

val dummy_mapper : line_mapper
(** A line mapper always returning [0]. *)

val line_number_table_mapper : (Utils.u2 * Utils.u2) list -> line_mapper
(** [line_number_table_mapper lnt] returns a line mapper for the data of
    a {i LineNumberTable} attribute. *)

val graph_of_instructions : ?line_mapper:line_mapper -> Instruction.t list -> Attribute.exception_table_element list -> (int32 * (Utils.u2 list), unit) graph
(** [graph_of_instructions instrs exn_table] constructs the graph for the
    method whose instructions are [instrs] and exception table is
    [exn_table]. The vertices of the returned graph are labeled with the
    offset of the first instruction associated with the vertex, and a
    list number lines (one number by instruction).

    Raises [Exception] if [instrs] contains an instruction that is not
    supported by [for_instruction], or if the resulting graph would be
    empty. *)
