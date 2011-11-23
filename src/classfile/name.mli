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

(** Types and utility functions related to name handling for class,
    field, method, package, and module names. *)


(** {6 Utility functions} *)

val replace_dot_with_slash : Utils.UTF8.t -> Utils.UTF8.t
(** [replace_dot_with_slash s] returns a copy of string [s] where each
    dot has been replaced by a slash. Useful to transform a fully
    qualified class name from external format into internal format. *)

val replace_slash_with_dot : Utils.UTF8.t -> Utils.UTF8.t
(** [replace_slash_with_dot s] returns a copy of string [s] where each
    slash and dollar has been replaced by a dot. Useful to transform a
    fully qualified class name from internal format into external
    format. *)

val is_valid_unqualified : Utils.UTF8.t -> bool
(** Checks whether the passed string is a valid unqualified name.
    That is, returns true iff the passed string is non-empty, and does
    not contain any dot, semi colon, opening square bracket or slash. *)

val is_valid_for_method : Utils.UTF8.t -> bool
(** Checks whether the passed string is a valid method name.
    That is, returns true iff the passed string is either non-empty, is
    the class constructor/initializer, or a valid unqualified name that
    does not contain any '<' or '>'. *)


(** {6 Exception} *)

type error =
  | Invalid_class_name of Utils.UTF8.t
  | Invalid_field_name of Utils.UTF8.t
  | Invalid_method_name of Utils.UTF8.t
  | Invalid_package_name of Utils.UTF8.t
  | Invalid_module_name of Utils.UTF8.t

exception Exception of error
(** Raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Name types and conversion functions} *)

type for_class
(** The type of class names. *)

type for_field
(** The type of field names. *)

type for_method
(** The type of method names. *)

type for_package
(** The type of package names. *)

type for_module
(** The type of module names. *)

val make_for_class_from_internal : Utils.UTF8.t -> for_class
(** Constructs a class name from an UTF8 string (slash being the
    separator between package elements, dollar being the separator
    between inner elements).
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_class_from_external : Utils.UTF8.t -> for_class
(** Constructs a class name from an UTF8 string (dot being the separator
    between package elements, dollar being the separator between inner
    elements).
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_field : Utils.UTF8.t -> for_field
(** Constructs a field name from an UTF8 string.
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_method : Utils.UTF8.t -> for_method
(** Constructs a method name from an UTF8 string.
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_package_from_internal : Utils.UTF8.t -> for_package
(** Constructs a package name from an UTF8 string (slash being the
    separator).
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_package_from_external : Utils.UTF8.t -> for_package
(** Constructs a package name from an UTF8 string (dot being the
    separator).
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_module_from_internal : Utils.UTF8.t -> for_module
(** Constructs a module name from an UTF8 string (slash being the
    separator).
    Raises [Exception] if passed UTF8 is invalid. *)

val make_for_module_from_external : Utils.UTF8.t -> for_module
(** Constructs a module name from an UTF8 string (dot being the
    separator).
    Raises [Exception] if passed UTF8 is invalid. *)

val printable_utf8_for_class : for_class -> Utils.UTF8.t
(** Converts a class name into external UTF8 form (dots between both
    package and inner elements). *)

val external_utf8_for_class : for_class -> Utils.UTF8.t
(** Converts a class name into external UTF8 form (dots between package
    elements, dollars between inner elements). *)

val internal_utf8_for_class : for_class -> Utils.UTF8.t
(** Converts a class name into internal UTF8 form (slashes between
    package elements, dollars between inner elements). *)

val utf8_for_field : for_field -> Utils.UTF8.t
(** Converts a field name into UTF8 form. *)

val utf8_for_method : for_method -> Utils.UTF8.t
(** Converts a method name into UTF8 form. *)

val external_utf8_for_package : for_package -> Utils.UTF8.t
(** Converts a package name into external UTF8 form. *)

val internal_utf8_for_package : for_package -> Utils.UTF8.t
(** Converts a package name into internal UTF8 form. *)

val external_utf8_for_module : for_module -> Utils.UTF8.t
(** Converts a module name into external UTF8 form. *)

val internal_utf8_for_module : for_module -> Utils.UTF8.t
(** Converts a module name into internal UTF8 form. *)

val equal_for_class : for_class -> for_class -> bool
(* Equality over class names. *)

val equal_for_field : for_field -> for_field -> bool
(* Equality over field names. *)

val equal_for_method : for_method -> for_method -> bool
(* Equality over method names. *)

val equal_for_package : for_package -> for_package -> bool
(* Equality over package names. *)

val equal_for_module : for_module -> for_module -> bool
(* Equality over module names. *)
