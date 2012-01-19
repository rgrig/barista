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

(** Annotations in both low- and high-level forms.

    It also provides conversion functions between levels as well as i/o
    functions for low-level. *)


(** {6 Low-level form} *)

type location = Utils.u1 list
(** Location, that is list of indexes used to disambiguate an annotation target. *)

type intervals = (Utils.u2 * Utils.u2 * Utils.u2) list
(** Intervals, that is (start, length, index) triple where:
    - start is the begin of the interval in the code;
    - length is the size of the interval;
    - index is the position of the local variable. *)

type target =
  | Typecast of Utils.u2
  | Typecast_loc of Utils.u2 * location
  | Instance_of of Utils.u2
  | Instance_of_loc of Utils.u2 * location
  | New of Utils.u2
  | New_loc of Utils.u2 * location
  | Method_receiver
  | Local_variable of intervals
  | Local_variable_loc of intervals * location
  | Method_return_type
  | Method_return_type_loc of location
  | Method_parameter of Utils.u1
  | Method_parameter_loc of Utils.u1 * location
  | Field
  | Field_loc of location
  | Class_type_parameter_bound of Utils.u1 * Utils.u1
  | Class_type_parameter_bound_loc of Utils.u1 * Utils.u1 * location
  | Method_type_parameter_bound of Utils.u1 * Utils.u1
  | Method_type_parameter_bound_loc of Utils.u1 * Utils.u1 * location
  | Super_type of Utils.u2
  | Super_type_loc of Utils.u2 * location
  | Thrown_exception of Utils.u2
  | Type_argument_constructor_call of Utils.u2 * Utils.u1
  | Type_argument_constructor_call_loc of Utils.u2 * Utils.u1 * location
  | Type_argument_method_call of Utils.u2 * Utils.u1
  | Type_argument_method_call_loc of Utils.u2 * Utils.u1 * location
  | Wildcard_bound of target
  | Wildcard_bound_loc of target * location
  | Class_literal of Utils.u2
  | Class_literal_loc of Utils.u2 * location
  | Method_type_parameter of Utils.u1
  | Class_type_parameter of Utils.u1
(** Represents the target, that is the element actually annotated by an
    extended annotation. *)

type primitive_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short ]
(** Represents the primitives types that can be used inside an annotation. *)

type info_element_value =
  | Primitive of primitive_type * Utils.u2 (** primitive value (given by type and index of value in constant pool) *)
  | String of Utils.u2 (** string value (given by index of value in constant pool) *)
  | Enum of Utils.u2 * Utils.u2 (** enum value (given by indexes of class and identifier) *)
  | Class of Utils.u2 (** class value (given by index of value in constant pool) *)
  | Annotation of info (** embedded annotation *)
  | Array of Utils.u2 * (info_element_value array) (** array of values *)
and info = {
    type_index : Utils.u2;
    num_element_value_pairs : Utils.u2;
    element_value_pairs : (Utils.u2 * info_element_value) array;
  }
(** Represents an annotation as defined in the class file format
    specification. *)
and extended_info = {
    ext_type_index : Utils.u2;
    ext_num_element_value_pairs : Utils.u2;
    ext_element_value_pairs : (Utils.u2 * info_element_value) array;
    ext_target : target;
  }
(** Represents an extended annotation as defined in the class file
    format specification. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_tag of Utils.UChar.t
  | Inconsistent_primitive_value
  | Invalid_string_value of Utils.u2
  | Invalid_enum_value of Utils.u2 * Utils.u2
  | Invalid_class_value of Utils.u2
  | Invalid_annotation_type_value of Utils.u2
  | Invalid_element_name of Utils.u2
  | Invalid_list_length of int
  | Invalid_target of int


(** {6 I/O functions} *)

val read_target : InputStream.t -> target
(** [read_target st] reads an element target from [st].
    Raises [Exception] if [st] does not contain a valid element target.
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_target : OutputStream.t -> target -> unit
(** [write_target st e] writes element target [e] onto [st].
    Raises [Exception] if [e] is not a valid element target.
    Raises [OutputStream.Exception] if an i/o error occurs. *)

val read_info_element_value : InputStream.t -> info_element_value
(** [read_info_element_value st] reads an element value from [st].
    Raises [Exception] if [st] does not contain a valid element value.
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_info_element_value : OutputStream.t -> info_element_value -> unit
(** [write_info_element_value st e] writes element value [e] onto [st].
    Raises [Exception] if [e] is not a valid element value.
    Raises [OutputStream.Exception] if an i/o error occurs. *)

val read_info : InputStream.t -> info
(** [read_info st] reads an annotation from [st].
    Raises [Exception] if [st] does not contain a valid annotation.
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_info : OutputStream.t -> info -> unit
(** [write_info st a] writes annotation [a] onto [st].
    Raises [Exception] if [a] is not a valid annotation.
    Raises [OutputStream.Exception] if an i/o error occurs. *)

val read_extended_info : InputStream.t -> extended_info
(** [read_extended_info st] reads an extended annotation from [st].
    Raises [Exception] if [st] does not contain a valid extended
    annotation.
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_extended_info : OutputStream.t -> extended_info -> unit
(** [write_extended_info st a] writes extended annotation [a] onto [st].
    Raises [Exception] if [a] is not a valid extended annotation.
    Raises [OutputStream.Exception] if an i/o error occurs. *)


(** {6 High-level form} *)

type element_value =
  | Boolean_value of bool (** boolean value *)
  | Byte_value of int (** byte value *)
  | Char_value of Utils.UChar.t (** char value *)
  | Double_value of float (** double value *)
  | Float_value of float (** float value *)
  | Int_value of int32 (** int value *)
  | Long_value of int64 (** long value *)
  | Short_value of int (** short value *)
  | String_value of Utils.UTF8.t (** string value *)
  | Enum_value of Name.for_class * Name.for_field (** enum value (given by class and identifier) *)
  | Class_value of Name.for_class (** class value *)
  | Annotation_value of t (** embedded annotation *)
  | Array_value of element_value list (** array of values *)
and t = Name.for_class * ((Utils.UTF8.t * element_value) list)
(** Represents an annotation in high-level form. *)
and extended = Name.for_class * ((Utils.UTF8.t * element_value) list) * target
(** Represents an extended annotation in high-level form. *)


(** {6 Conversion functions} *)

val decode_element_value : ConstantPool.t -> info_element_value -> element_value
(** Converts from a low-level into a high-level form according to
    passed pool.
    Raises [Exception] if an error occurs during conversion. *)

val decode : ConstantPool.t -> info -> t
(** Converts from a low-level into a high-level form according to
    passed pool.
    Raises [Exception] if an error occurs during conversion. *)

val decode_extended : ConstantPool.t -> extended_info -> extended
(** Converts from a low-level into a high-level form according to
    passed pool.
    Raises [Exception] if an error occurs during conversion. *)

val encode_element_value : ConstantPool.extendable -> element_value -> info_element_value
(** Converts from a high-level into a low-level form, using passed
    extendable pool.
    Raises [Exception] if an error occurs during conversion. *)

val encode : ConstantPool.extendable -> t -> info
(** Converts from a high-level into a low-level form, using passed
    extendable pool.
    Raises [Exception] if an error occurs during conversion. *)

val encode_extended : ConstantPool.extendable -> extended -> extended_info
(** Converts from a high-level into a low-level form, using passed
    extendable pool.
    Raises [Exception] if an error occurs during conversion. *)
