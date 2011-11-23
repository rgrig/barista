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

(** Utility functions for code manipulation, and optimization. *)


val remove_dead_code : ('a, 'b) ControlFlow.graph -> ('a, 'b) ControlFlow.graph
(** [remove_dead_code g] returns [g] without the vertices that cannot be
    reached from the root. *)

val optimize_jumps : ('a, 'b) ControlFlow.graph -> ('a, 'b) ControlFlow.graph
(** [optimize_jumps g] returns [g] except that empty nodes ({i i. e.}
    with no instruction) have been short-circuited (but not removed,
    possibly producing dead code). *)

val flatten_graph : ('a * (Utils.u2 list), 'b) ControlFlow.graph -> Instruction.t list * (Utils.u2 * Utils.u2) list * Attribute.exception_table_element list * (int32 * 'a, 'b) ControlFlow.graph
(** [flatten_graph g] returns a [l, t, e, g'] triple where [g'] is [g]
    labeled with code offsets corresponding to the instruction list [l].
    [t] is the data for the {i LineNumberTable} attribute, and [e] is the
    exception table for [l]. *)

val optimize_graph : (('a * (Utils.u2 list)), 'b) ControlFlow.graph -> (('a * (Utils.u2 list)), 'b) ControlFlow.graph
(** [optimize_graph g] returns an optimized version of graph [g].
    It is equivalent to [remove_dead_code (optimize_jumps (Peephole.optimize_graph g))]. *)

val compute_stack_infos : StackState.instance StackState.unifier -> (int32 * 'a, 'b) ControlFlow.graph -> StackState.t -> Utils.u2 * Utils.u2 * Attribute.stack_map_frame list
(** [compute_stack_infos u g s] returns [max_stack, max_locals, stack_map_frame]
    for the method whose control flow graph [g] and initial stack state
    [s] are passed. The [int32] values labelling the vertices are the
    offsets of the related instructions blocks. The function [u] is used
    to unify stack states.

    Raises [StackState.Exception] if the passed instruction lists
    associated with the graph vertices are incoherent. *)
