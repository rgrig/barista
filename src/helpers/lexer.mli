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

(** Implements the lexing function used by the assembler to read tokens
    from a source file. *)


(** {6 Tokens} *)

type token =
  | Directive of string (** {i .directive} in source *)
  | Attribute of string (** {i \@attribute} in source *)
  | Label of Utils.UTF8.t (** {i label:} in source *)
  | Int of int64 (** follows the OCaml conventions *)
  | Float of float (** follows the OCaml conventions *)
  | String of Utils.UTF8.t (** {i "abcd"}, handles escape sequences *)
  | Class_name of Name.for_class (** in fully qualified form, does not accept a class outside of a package *)
  | Array_type of Utils.UTF8.t (** {i abc[]} *)
  | Primitive_type of Descriptor.java_type (** also accepts {i void} *)
  | Field of Name.for_class * Name.for_field * Descriptor.for_field (** class name, field name, and field descriptor *)
  | Dynamic_method of Name.for_method * Descriptor.for_method (** method name, and method descriptor *)
  | Method of Name.for_class * Name.for_method * Descriptor.for_method (** class name, method name, and method descriptor *)
  | Array_method of Descriptor.array_type * Name.for_method * Descriptor.for_method (** array type, method name, and method descriptor *)
  | Method_signature of Name.for_method * (Descriptor.for_parameter list) (** method name, and descriptors for method parameters *)
  | Method_type of Descriptor.for_method (** bare method type, with no name *)
  | Method_handle of Bootstrap.method_handle (** bare method descriptor, with no argument *)
  | Identifier of Utils.UTF8.t (** bare identifier *)
  | Arrow (** {i =>} *)
  | Tilde (** {i ~} *)
(** The possible tokens to be found in a source file. *)


(** {6 Exception} *)

type error =
  | Invalid_label of Utils.UTF8.t
  | Invalid_directive of Utils.UTF8.t
  | Invalid_attribute of Utils.UTF8.t
  | Invalid_string of Utils.UTF8.t
  | Invalid_character of Utils.UChar.t
  | Invalid_float of string
  | Invalid_integer of string
  | Invalid_method_handle of Utils.UTF8.t
  | Invalid_token
  | Name_error of Name.error
  | Descriptor_error of Descriptor.error
  | UChar_error of Utils.UChar.error
  | UTF8_error of Utils.UTF8.error

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Lexing funtion} *)

val tokens_of_line : Utils.UTF8.t -> token list
(** Converts the passed string into a list of tokens.
    Comments (starting with {i #}, and ending with the end of the string)
    are ignored.
    Raises [Exception] if the passed string is such that a list of
    correct tokens cannot be extracted. *)


(** {6 Miscellaneous} *)

val equal : token -> token -> bool
(** Equality over token values. Does not use co-/contra- variance
    (meaning that classes are compared on a name basis, and neither
    interfaces or generics are taken into account). *)
