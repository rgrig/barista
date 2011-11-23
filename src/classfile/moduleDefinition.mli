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

(** Module definition as well as conversion functions from/to [ClassFile.t]. *)


(** {6 Type} *)

type t = {
    access_flags : AccessFlag.for_module list;
    name : Name.for_module;
    attributes : Attribute.for_module list;
  }
(** Definition of a Java module. *)


(** {6 Exception} *)

type error =
  | Invalid_module_name
  | Too_many of string
  | Version_error of Version.error
  | Invalid_module_definition

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Conversion functions} *)

val decode : ClassFile.t -> t
(** Converts from a [ClassFile.t] into a module definition.
    Raises [Exception] if an error occurs during conversion. *)

val encode : ?version : Version.t -> t -> ClassFile.t
(** Converts a module definition into to a [ClassFile.t], using the passed version.
    The default version is [Version.default].
    Raises [Exception] if an error occurs during conversion. *)
