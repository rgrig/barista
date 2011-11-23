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

(** Peephole optimizations over jump-less instruction lists. *)


(** {6 Rewriting rules} *)

type rewriting_rules = (Utils.u2 * Instruction.t) list -> (Utils.u2 * Instruction.t) list
(** The type of function rewriting (line, instruction) lists. *)

val optimize_constants : rewriting_rules
(** Predefined rules to optimize constants. *)

val optimize_locals : rewriting_rules
(** Predefined rules to optimize local accesses. *)

val remove_nops_and_unused_pushed_values : rewriting_rules
(** Predefined rules to remove no-ops. *)

val optimize_iinc : rewriting_rules
(** Predefined rules to optimize iinc instructions. *)

val remove_load_store : rewriting_rules
(** Predefined rules to optimize load/store sequences. *)

val remove_get_put : rewriting_rules
(** Predefined rules to optimize get/put sequences. *)

val rewrite_store_store : rewriting_rules
(** Predefined rules to optimize store/store sequences. *)

val rewrite_store_load : rewriting_rules
(** Predefined rules to optimize store/load sequences. *)

val constant_on_the_top : rewriting_rules
(** Predefined rules to move constants on the top. *)

val remove_neutral_elements : rewriting_rules
(** Predefined rules to remove neutral elements. *)

val rewrite_absorbing_elements : rewriting_rules
(** Predefined rules to simplify by absorbing elements. *)

val apply_stength_reduction : rewriting_rules
(** Predefined rules to apply stength reduction. *)

val remove_identity : rewriting_rules
(** Predefined rules to remove identities. *)

val all_rules : rewriting_rules list
(** The list of all predefined rewriting rules, that is (in order):
    - [optimize_constants];
    - [optimize_locals];
    - [remove_nops_and_unused_pushed_values];
    - [optimize_iinc];
    - [remove_load_store];
    - [remove_get_put];
    - [rewrite_store_store];
    - [rewrite_store_load];
    - [constant_on_the_top];
    - [remove_neutral_elements];
    - [rewrite_absorbing_elements];
    - [apply_stength_reduction];
    - [remove_identity]. *)

type position =
  | Head (** Insert rule at the list begin. *)
  | Tail (** Insert rule at the list end. *)
  | Before of rewriting_rules (** Insert rule before specified one. *)
  | After of rewriting_rules (** Insert rule before specified one. *)
(** Type defining where a rule should be added inside a list. *)

val insert : rewriting_rules -> position -> rewriting_rules list -> rewriting_rules list
(** [insert r p l] inserts rule [r] into list [l] at position [p],
    using physical equality to compare rewriting rules.

    Raises [Not_found] if [p] references rewriting rules not present in
    the list. *)


(** {6 Application of rewriting rules} *)

val optimize_graph : ?rules : rewriting_rules list -> (('a * (Utils.u2 list)), 'b) ControlFlow.graph -> (('a * (Utils.u2 list)), 'b) ControlFlow.graph
(** [optimize_graph ~rules:l g] returns a graph similar to [g], except
    that every instruction list has been optimized, [l] being the list of
    rewriting rules to apply to instruction lists (until a fixpoint is
    reached). *)
