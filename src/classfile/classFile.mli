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

(** Class file definition as well as i/o functions.
    Class file instances are low-level Java class definitions,
    high-level definitions being provided by [ClassDefinition.t]
    instances. *)


(** {6 Type} *)

type t = {
    magic : Utils.u4;
    minor_version : Utils.u2;
    major_version : Utils.u2;
    constant_pool_count : Utils.u2;
    constant_pool : ConstantPool.t;
    access_flags : Utils.u2;
    this_class : Utils.u2;
    super_class : Utils.u2;
    interfaces_count : Utils.u2;
    interfaces : Utils.u2 array;
    fields_count : Utils.u2;
    fields : Field.info array;
    methods_count : Utils.u2;
    methods : Method.info array;
    attributes_count : Utils.u2;
    attributes : Attribute.info array;
  }
(** This structure is barely a translation from the class file as it is
    defined in Sun's Java specification. {i x_count} fields are redundant
    as they represent the length of {i x} fields but are nevertheless
    kept to ensure that this structure is as close as possible from the
    Java specification.

    The only slight difference is related to the constant pool.
    As a constant pool is indexed from {i 1} to {i size - 1}, it has thus
    been decided to create an array of size [size] and to put a dummy
    value at index 0 (this dummy value is also used for the auxiliary
    index of the elements (double and long ones) that use two indexes in
    the constant pool). *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_magic_number of Utils.u4
  | Unsupported_version of Utils.u2 * Utils.u2
  | Invalid_this
  | Invalid_super
  | Invalid_super_interface
  | Invalid_parent_interface


(** {6 I/O functions} *)

val read : InputStream.t -> t
(** [read st] reads a class file from the passed stream.
    Raises [Exception] if the read structure is not a valid class file.
    Raises [InputStream.Exception] if an i/o error occurs.
    Raises [ConstantPool.Exception] if the underlying constant pool is
    not consistent. *)

val write : t -> OutputStream.t -> unit
(** [write cf st] writes the class file [cf] onto the stream [st].
    Raises [OutputStream.Exception] if an i/o error occurs. *)
