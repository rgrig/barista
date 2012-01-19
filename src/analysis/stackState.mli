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


(** Definition and utility functions for stack state manipulation. *)


(** {6 Types} *)

type locals
(** The type of locals: integer-index type information.
    Elements are of [Attribute.verification_type_info] type. *)

type stack
(** The type of {i operand} stacks.
    Elements are of [Attribute.verification_type_info] type. *)

type t = {
    locals : locals; (** type information for the locals. *)
    stack : stack; (** type information for the {i operand} stack. *)
  }
(** The type of stack states. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Unsupported_instruction of string
  | Empty_stack
  | Invalid_local_index of Utils.u2 * int
  | Invalid_stack_top of Attribute.verification_type_info * Attribute.verification_type_info
  | Invalid_local_contents of Utils.u2 * Attribute.verification_type_info * Attribute.verification_type_info
  | Reference_waited of Attribute.verification_type_info
  | Array_waited
  | Category1_waited
  | Category2_waited
  | Different_stack_sizes of int * int
  | Invalid_primitive_array_type
  | Empty_frame_list
  | Different_frames of Utils.u2


(** {6 Construction} *)

val make_empty : unit -> t
(** Returns an empty state (no local, and no operand on stack). *)

val make_of_parameters : (Name.for_class * bool) option -> Descriptor.for_parameter list -> t
(** [make_of_parameters cn p] returns the state describing the stack at
    the beginning of a method. [cn] is the enclosing class for the method
    if it is an instance method (the boolean indicating whether the
    method is a constructor), [None] being used for static methods. [p]
    is the list of parameters of the method. *)

val make_of_method : Name.for_class -> Method.t -> t
(** [make_of_method cn m] returns the state describing the stack at the
    beginning for the method [m] in the class [cn]. *)


(** {6 Access and modification} *)

val locals_size : t -> int
(** Returns the size of the locals for the passed state. *)

val stack_size : t -> int
(** Returns the size of the operand stack for the passed state. *)

val equal : t -> t -> bool
(** Equality over stack states. *)

val push : Attribute.verification_type_info -> stack -> stack
(** [push v s] returns a stack similar to [s] with [v] pushed on its
    top. *)

val top : stack -> Attribute.verification_type_info
(** [top s] returns the top element of [s].
    Raises [Exception] if [s] is empty. *)

val pop : stack -> stack
(** [pop s] returns [s] without its top element.
    Raises [Exception] if [s] is empty. *)

val pop_if : Attribute.verification_type_info -> stack -> stack
(** [pop_if v s] returns [s] without its top element if this element is
    equal to [v], raising [Exception] otherwise.
    Raises [Exception] if [s] is empty. *)

val pop_if_category1 : stack -> Attribute.verification_type_info * stack
(** [pop_if_category1 s] returns a couple with the top element, and [s]
    without its top element. Raises [Exception] if [s] is empty, or if
    its top element is not a {i category 1} element. *)

val empty : unit -> stack
(** Returns an empty stack. *)

val only_exception : Name.for_class -> stack
(** Returns a stack containing only the passed element. *)

val load : Utils.u2 -> locals -> Attribute.verification_type_info
(** Returns the type information for the local at the passed index.
    Raises [Exception] if the index is invalid. *)

val check_load : Utils.u2 -> locals -> Attribute.verification_type_info -> unit
(** Checks that the type information for the local at the passed index is
    equal to the passed one, raising [Exception] otherwise.
    Raises [Exception] if the index is invalid. *)

val store : Utils.u2 -> Attribute.verification_type_info -> locals -> locals
(** [store i v l] returns new locals, identical to [l] except that the
    [i]th element is equal to [v]. The size of [l] is augmented as
    necessary. *)


(** {6 Operations} *)

val update : Utils.u2 -> Instruction.t -> t -> t
(** [update ofs i st] returns the state [st] updated according to the
    instruction [i] located at offset [ofs].
    Raises [Exception] if the passed state is not compatible with the
    instruction, or if the instruction is unsupported ({i jsr},
    {i jsr_w}, {i ret}, or {i wide ret}). *)

type 'a unifier = 'a -> 'a -> 'a
(** The type of unifiers, that is functions that return an element that
    generalizes the passed ones. *)

type instance =
  [ `Array_type of Descriptor.array_type
  | `Class_or_interface of Name.for_class ]
(** Type abbreviation, used to represent any Java instance. *)

val make_array_unifier : Name.for_class unifier -> Descriptor.array_type -> Descriptor.array_type -> instance
(** Builds an array unifier from a class unifier that is used to unify
    array elements. *)

val make_unifier : Name.for_class unifier -> instance unifier
(** Builds a unifier from a class unifier. *)

val unify_to_java_lang_Object : instance unifier
(** A unifier that returns {i java.lang.Object} when passed classes are
    different. *)

val unify_to_closest_common_parent : ClassLoader.t -> (Name.for_class * Name.for_class option) list -> instance unifier
(** A unifier that returns the closest common parent of the passed
    classes. The class loader is used to load the parents of the passed
    classes. The passed list is a (class, parent) association list
    overriding the class loader. *)

val unify_to_parent_list : (Name.for_class * Name.for_class option) list -> instance unifier
(** A unifier that returns the closest common parent of the passed
    classes, using the passed list as a (class, parent) association
    list. *)

val unify : instance unifier -> t -> t -> t
(** [unify u st1 st2] returns a state that generalizes [st1] and [st2]. *)

val encode : ?optimize : bool -> (Utils.u2 * t) list -> Attribute.stack_map_frame list
(** Encodes the passed list of (offset, state) couples into attribute
    values.
    Raises [Exception] if the passed list is empty, or contains different
    frames at the same offset. *)
