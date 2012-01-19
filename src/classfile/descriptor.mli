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

(** Definition and utility functions related to descriptor handling. *)


(** {6 Java types definition} *)

type java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Void
  | `Class of Name.for_class
  | `Array of 'a ] constraint 'a = non_void_java_type
and non_void_java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Class of Name.for_class
  | `Array of 'a ] constraint 'a = non_void_java_type
(** Definition of Java types. *)

type array_type =
  [ `Array of 'a ] constraint 'a = [ `Boolean
                                   | `Byte
                                   | `Char
                                   | `Double
                                   | `Float
                                   | `Int
                                   | `Long
                                   | `Short
                                   | `Class of Name.for_class
                                   | `Array of 'a ]
(** Definition of Java array types. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_class_name
  | Invalid_array_element_type
  | Array_with_too_many_dimensions
  | Invalid_descriptor_string
  | Empty_descriptor_string
  | Invalid_field_type
  | Invalid_local_variable_type
  | Invalid_method_descriptor
  | Invalid_method_parameter_type
  | Void_not_allowed


(** {6 Utility functions} *)

val is_primitive : java_type -> bool
(** Tests whether the passed type is primitive. *)

val filter_void : error -> java_type -> non_void_java_type
(** [filter_void err jt] returns [jt] if it is not equal to [`Void].
    Otherwise, [Exception] is raised with [err] as a parameter. *)

val filter_non_array : error -> java_type -> array_type
(** [filter_non_array err jt] returns [jt] if it is an array.
    Otherwise, [Exception] is raised with [err] as a parameter. *)

val java_type_of_internal_utf8 : Utils.UTF8.t -> java_type
(** Converts a string (as used in class file) into the corresponding Java
    type.
    Raises [Exception] if conversion fails. *)

val internal_utf8_of_java_type : java_type -> Utils.UTF8.t
(** Converts a Java type into the corresponding string (as used in class
    file).
    Raises [Exception] if conversion fails. *)

val external_utf8_of_java_type : java_type -> Utils.UTF8.t
(** Returns the textual representation of the passed Java type. *)

val java_type_of_external_utf8 : Utils.UTF8.t -> java_type
(** Returns the Java type corresponding to the passed string.
    Raises [Exception] if the string does not represent a Java type.
    Also Raises [Exception] if the type is invalid. *)

val equal_java_type : java_type -> java_type -> bool
(* Equality over Java types. *)


(** {6 Field descriptors} *)

type for_field = non_void_java_type
(** Type for field descriptor. *)

val field_of_utf8 : Utils.UTF8.t -> for_field
(** Converts a string into the corresponding field descriptor.
    Raises [Exception] if conversion fails. *)

val utf8_of_field : for_field -> Utils.UTF8.t
(** Converts a field descriptor into the corresponding string.
    Raises [Exception] if conversion fails. *)

val java_type_of_external_utf8_no_void : Utils.UTF8.t -> for_field
(** Same as [java_type_of_external_utf8] but raises [Exception]
    if the decoded type is equal to the Java type {i void}. *)

val java_type_of_internal_utf8_no_void : Utils.UTF8.t -> for_field
(** Same as [java_type_of_internal_utf8] but raises [Exception]
    if the decoded type is equal to the Java type {i void}. *)

val equal_for_field : for_field -> for_field -> bool
(* Equality over field descriptors. *)


(** {6 Method descriptors} *)

type for_parameter = non_void_java_type
(** Type for parameter descriptor, bare alias of [for_field] used for
    increased readability. *)

val parameter_of_utf8 : Utils.UTF8.t -> for_parameter
(** Alias for [field_of_utf8], used for increased readability. *)

val utf8_of_parameter : for_parameter -> Utils.UTF8.t
(** Alias for [utf8_of_field], used for increased readability. *)

val equal_for_parameter : for_parameter -> for_parameter -> bool
(* Equality over parameter descriptors. *)

type for_method = (for_parameter list) * java_type
(** Type for method descriptor, first component is the list of formal
    parameters while second component is the return type. *)

val method_of_utf8 : Utils.UTF8.t -> for_method
(** Converts a string into the corresponding method descriptor.
    Raises [Exception] if conversion fails. *)

val utf8_of_method : for_method -> Utils.UTF8.t
(** Converts a method descriptor into the corresponding string.
    Raises [Exception] if conversion fails. *)

val equal_for_method : for_method -> for_method -> bool
(* Equality over method descriptors. *)

val compare_for_method : for_method -> for_method -> int
(** Comparison over method descriptors. *)
