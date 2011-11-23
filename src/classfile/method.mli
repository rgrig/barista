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

(** Methods in both low- and high-level forms.

    It also provides conversion functions between levels as well as i/o
    functions for low-level. *)


(** {6 Low-level form} *)

type info = {
    access_flags : Utils.u2;
    name_index : Utils.u2;
    descriptor_index : Utils.u2;
    attributes_count : Utils.u2;
    attributes_array : Attribute.info array;
  }
(** Represents a method as defined in the class file format specification. *)


(** {6 Exception} *)

type error =
  | Invalid_name of Utils.UTF8.t
  | Invalid_name_value of Utils.u2
  | Invalid_descriptor_value of Utils.u2

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 I/O functions} *)

val read_info : InputStream.t -> info
(** [read_info st] reads a method from [st].
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_info : OutputStream.t -> info -> unit
(** [write_info st m] writes method [m] onto [st].
    Raises [OutputStream.Exception] if an i/o error occurs. *)


(** {6 High-level form} *)

type regular = {
    flags : AccessFlag.for_method list;
    name : Name.for_method;
    descriptor : Descriptor.for_method;
    attributes : Attribute.for_method list;
  }
(** Represents a {i regular} (possibly static) method. *)

type constructor = {
    cstr_flags : AccessFlag.for_constructor list;
    cstr_descriptor : Descriptor.for_parameter list;
    cstr_attributes : Attribute.for_method list;
  }
(** Represents an instance constructor method. *)

type class_initializer = {
    init_flags : AccessFlag.for_initializer list;
    init_attributes : Attribute.for_method list;
  }
(** Represents a class initializer method. *)

type t =
  | Regular of regular (** Regular method. *)
  | Constructor of constructor (** Instance constructor. *)
  | Initializer of class_initializer (** Class initializer. *)
(** Represents the different kinds of methods. *)


(** {6 Conversion functions} *)

val decode : bool -> Bootstrap.methods -> ConstantPool.t -> info -> t
(** Converts from a low-level into a high-level form according to passed
    pool, and bootstrap method information. The first parameter indicates
    whether the enclosing element is an interface or a class.
    Raises [Exception] if an error occurs during conversion. *)

val encode : Bootstrap.methods -> ConstantPool.extendable -> t -> info
(** Converts from a high-level into a low-level form, using passed
    extendable pool, and bootstrap method information.
    Raises [Exception] if an error occurs during conversion. *)

val compare : t -> t -> int
(** Comparison over methods. *)
