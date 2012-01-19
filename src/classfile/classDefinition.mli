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

(** Class definition as well as conversion functions from/to [ClassFile.t].

    Class definition instances are high-level Java class definitions,
    low-level definitions being provided by [ClassFile.t] instances. *)


(** {6 Type} *)

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : Method.t list;
    attributes : Attribute.for_class list;
  }
(** Definition of a Java class. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_class_name
  | Bootstrap_methods_defined_twice
  | Too_many of string
  | Version_error of Version.error


(** {6 Traversal} *)

val map : Traversal.class_definition_mapper -> t -> t
(** Applies the "mapper" to the class definition. *)

val iter : Traversal.class_definition_iterator -> t -> unit
(** Applies the "iterator" to the class definition. *)


(** {6 Conversion functions} *)

val decode : ClassFile.t -> t
(** Converts from a [ClassFile.t] into a class definition.
    Raises [Exception] if an error occurs during conversion. *)

val encode : ?version : Version.t -> t -> ClassFile.t
(** Converts a class definition into to a [ClassFile.t], using the passed version.
    The default version is [Version.default].
    Raises [Exception] if an error occurs during conversion. *)
