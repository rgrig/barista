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

(** Bootstrap information for dynamic calls. *)


(** {6 Base types and function} *)

type method_handle =
  [ `getField of ConstantPool.field_reference
  | `getStatic of ConstantPool.field_reference
  | `putField of ConstantPool.field_reference
  | `putStatic of ConstantPool.field_reference
  | `invokeVirtual of ConstantPool.method_reference
  | `invokeStatic of ConstantPool.method_reference
  | `invokeSpecial of ConstantPool.method_reference
  | `newInvokeSpecial of ConstantPool.constructor_reference
  | `invokeInterface of ConstantPool.method_reference ]
(** The type for bootstrap methods used by {i invokedynamic} instructions. *)

val equal_method_handle : method_handle -> method_handle -> bool
(** Equality over method handles. *)

type method_argument =
  [ `String of Utils.UTF8.t
  | `Class of Name.for_class
  | `Integer of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `MethodHandle of method_handle
  | `MethodType of Descriptor.for_method ]
(** The type for bootstrap arguments used by {i invokedynamic} instructions. *)

val equal_method_argument : method_argument -> method_argument -> bool
(** Equality over method arguments. *)

type method_specifier = method_handle * (method_argument list)
(** The type of method specifiers, that is complete bootstrap information
    for an {i invokedynamic} instruction. *)

val equal_method_specifier : method_specifier -> method_specifier -> bool
(** Equality over method specifiers. *)


(** {6 Structure used for encoding} *)

type methods = method_specifier ExtendableArray.t
(** The type of information to be actually stored in a class file. *)

type error =
  | Too_large of int

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)

val make_methods : unit -> methods
(** Constructs an empty array of method specifiers. *)

val is_empty : methods -> bool
(** Tests whether the passed array is empty. *)

val add_method_specifier : methods -> method_specifier -> Utils.u2
(** [add_method_specifier m ms] augments [m] with [ms].
    Returns index of existing or created entry.
    Raises [Exception] if passed methods is too large. *)

val add : methods -> method_specifier -> unit
(** [add m ms] augments [m] with [ms].
    Raises [Exception] if passed methods is too large. *)
