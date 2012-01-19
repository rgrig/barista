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

(** Access flags for the various Java elements. *)


(** {6 Types} *)

type t =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Super
  | `Synchronized
  | `Bridge
  | `Volatile
  | `Transient
  | `Varargs
  | `Native
  | `Interface
  | `Abstract
  | `Strict
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]
(** Possible flags for any Java element. *)

type for_class =
  [ `Public
  | `Final
  | `Super
  | `Interface
  | `Abstract
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]
(** Possible flags for a class. *)

type for_inner_class =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Super
  | `Interface
  | `Abstract
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]
(** Possible flags for an inner-class. *)

type for_field =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Volatile
  | `Transient
  | `Synthetic
  | `Enum
  | `Module ]
(** Possible flags for a field. *)

type for_method =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Synchronized
  | `Bridge
  | `Varargs
  | `Native
  | `Abstract
  | `Strict
  | `Synthetic
  | `Module ]
(** Possible flags for a method. *)

type for_constructor =
  [ `Public
  | `Private
  | `Protected
  | `Strict
  | `Varargs
  | `Synthetic
  | `Module ]
(** Possible flags for a constructor. *)

type for_initializer =
  [ `Static
  | `Strict ]
(** Possible flags for an initializer. *)

type for_package =
  [ `Interface
  | `Abstract
  | `Synthetic ]
(** Possible flags for a package. *)

type for_module =
  [ `Interface
  | `Abstract
  | `Synthetic ]
(** Possible flags for a module. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_class_flags of t option
  | Invalid_inner_class_flags of t option
  | Invalid_field_flags of t option
  | Invalid_method_flags of t option
  | Invalid_constructor_flags of t option
  | Invalid_initializer_flags of t option
  | Invalid_package_flags of t option
  | Invalid_module_flags of t option
  | Several_visibility_flags
  | Unknown_flag of string


(** {6 Functions} *)

val to_u2 : t -> Utils.u2
(** [to_u2 f] converts flag [f] into an integer mask,
    as used in the class file format. *)

val list_to_u2 : t list -> Utils.u2
(** [to_u2 dom l] converts flag list [l] into an integer mask,
    as used in the class file format. *)

val from_u2 : bool -> Utils.u2 -> t list
(** [from_u2 meth mask] converts mask into flag list, [meth] indicating
    whether the considered Java element is a method (including
    constructors). *)

val to_string : t -> string
(** Converts the passed flag into a string. *)

val of_string : string -> t
(** Converts the passed string into a flag.
    Raises [Exception] if the passed string is not a valid flag. *)

val to_utf8 : t -> Utils.UTF8.t
(** Converts the passed flag into a UTF8 string. *)

val of_utf8 : Utils.UTF8.t -> t
(** Converts the passed UTF8 string into a flag.
    Raises [Exception] if the passed string is not a valid flag. *)

val list_to_utf8 : t list -> Utils.UTF8.t
(** Converts the passed flag list to its corresponding UTF8 string.
    Flags are separated by a single space, and a single space is also
    added at the end of the returned string. *)

val check_class_flags : t list -> for_class list
(** Acts as the identity function if the passed flags form a valid flag
    set for a class, raises [Exception] otherwise. *)

val check_inner_class_flags : t list -> for_inner_class list
(** Acts as the identity function if the passed flags form a valid flag
    set for an inner class, raises [Exception] otherwise. *)

val check_field_flags : bool -> t list -> for_field list
(** Acts as the identity function if the passed flags form a valid flag
    set for a field, raises [Exception] otherwise.
    The passed boolean indicates whether the checked field belongs to an
    interface. *)

val check_method_flags : bool -> t list -> for_method list
(** Acts as the identity function if the passed flags form a valid flag
    set for a method, raises [Exception] otherwise.
    The passed boolean indicates whether the checked method belongs to
    an interface. *)

val check_constructor_flags : t list -> for_constructor list
(** Acts as the identity function if the passed flags form a valid flag
    set for a constructor, raises [Exception] otherwise. *)

val check_initializer_flags : t list -> for_initializer list
(** Acts as the identity function if the passed flags form a valid flag
    set for an initializer, raises [Exception] otherwise. *)

val check_package_flags : t list -> for_package list
(** Acts as the identity function if the passed flags form a valid flag
    set for a package, raises [Exception] otherwise. *)

val check_module_flags : t list -> for_module list
(** Acts as the identity function if the passed flags form a valid flag
    set for a module, raises [Exception] otherwise. *)

val compare : t -> t -> int
(** Comparison over flags.
    The order is the one defined by {i java.lang.reflect.Modifier}. *)

val list_compare : t list -> t list -> int
(** Comparison over flag list. *)

val version_bounds : t -> Version.bounds
(** Returns the version bounds for the passed flag. *)
