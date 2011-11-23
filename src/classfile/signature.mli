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

(** Signatures definition as well as conversion functions from and to strings. *)


(** {6 Signatures definition} *)

(** The following types are direct mappings from the class file
    specification. One should refer to this document for the semantics
    of these types. *)

type class_signature = {
    formal_type_parameters : formal_type_parameter list;
    super_class_signature : class_type_signature;
    super_interface_signatures : class_type_signature list;
  }
and formal_type_parameter = {
    identifier : Utils.UTF8.t;
    class_bound : field_type_signature;
    interface_bounds : field_type_signature list;
  }
and field_type_signature =
  | Class_type_signature of class_type_signature
  | Array_type_signature of array_type_signature
  | Type_variable_signature of type_variable_signature
and class_type_signature = {
    qualified_class_name : Name.for_class;
    type_arguments : type_argument list;
    signature_suffix : class_type_signature_suffix list;
  }
and array_type_signature = type_signature
and type_signature =
  | Field_type_signature of field_type_signature
  | Base_type of Descriptor.java_type
and type_variable_signature = Utils.UTF8.t
and type_argument =
  | Star
  | Plus of field_type_signature
  | Minus of field_type_signature
  | Simple of field_type_signature
and class_type_signature_suffix = {
    suffix_identifier : Utils.UTF8.t;
    suffix_type_arguments : type_argument list;
  }
and method_signature = {
    formal_type_params : formal_type_parameter list;
    types : type_signature list;
    return : type_signature;
    throws_signatures : throws_signature list;
  }
and throws_signature =
  | Throws_class_type_signature of class_type_signature
  | Throws_type_variable_signature of type_variable_signature


(** {6 Exception} *)

type error =
  | Invalid_signature

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Conversion functions} *)

val class_signature_of_utf8 : Utils.UTF8.t -> class_signature
(** Converts a string (as used in class file) into the corresponding signature.
    Raises [Exception] if conversion fails. *)

val utf8_of_class_signature : class_signature -> Utils.UTF8.t
(** Converts a signature into the corresponding string (as used in class file).
    Raises [Exception] if conversion fails. *)

val field_type_signature_of_utf8 : Utils.UTF8.t -> field_type_signature
(** Converts a string (as used in class file) into the corresponding signature.
    Raises [Exception] if conversion fails. *)

val utf8_of_field_type_signature : field_type_signature -> Utils.UTF8.t
(** Converts a signature into the corresponding string (as used in class file).
    Raises [Exception] if conversion fails. *)

val method_signature_of_utf8 : Utils.UTF8.t -> method_signature
(** Converts a string (as used in class file) into the corresponding signature.
    Raises [Exception] if conversion fails. *)

val utf8_of_method_signature : method_signature -> Utils.UTF8.t
(** Converts a signature into the corresponding string (as used in class file).
    Raises [Exception] if conversion fails. *)
