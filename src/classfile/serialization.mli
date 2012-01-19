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

(** Support for serialization/serialization, using the {i Object
    Serialization protocol} version 2. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | Invalid_magic of Utils.u2
  | Invalid_version of Utils.u2
  | Invalid_stream
  | Array_type_waited
  | Unknown_reference
  | Invalid_class_flags of Utils.s1
  | Missing_read_function
  | Missing_write_function
  | Missing_field of Utils.UTF8.t
  | Invalid_field_type of Utils.UTF8.t


(** {6 Object values} *)

type descriptor
(** The type of class descriptors. *)

and read_method = InputStream.t -> instance -> unit
(** The type of functions that should mimic
    {i java.ioExternalizable.readExternal(-)} by consuming data from the
    stream, and modifying the passed instance. *)

and write_method = OutputStream.t -> instance -> unit
(** The type of functions that should mimic
    {i java.ioExternalizable.writeExternal(-)} by producing data onto the
    stream from the passed instance. *)

and instance
(** The type of {i classical} instances
    (association from fields to values). *)

and array_instance =
  | Boolean_array of bool array (** Boolean array. *)
  | Byte_array of Utils.s1 array (** Byte array. *)
  | Char_array of Utils.u2 array (** Char array. *)
  | Double_array of float array (** Double array. *)
  | Float_array of float array (** Float array. *)
  | Int_array of Utils.s4 array (** Int array. *)
  | Long_array of Utils.s8 array (** Long array. *)
  | Object_array of object_value array (** Object array. *)
  | Short_array of Utils.s2 array (** Short array. *)
(** The type of array instances. *)

and object_value =
  | Null (** Null reference. *)
  | Block_data of string (** Block of {i unparsed} data. *)
  | String of Utils.UTF8.t (** String instance. *)
  | Class_desc of descriptor (** Class descriptor. *)
  | Instance of instance (** {i Classical} instance. *)
  | Array_instance of descriptor * array_instance (** Array instance. *)
  | Enum of (descriptor * Utils.UTF8.t) (** Enum value. *)
(** The type of object instances. *)

and field_value =
  | Boolean_value of bool (** Boolean value. *)
  | Byte_value of Utils.s1 (** Byte value. *)
  | Char_value of Utils.u2 (** Char value. *)
  | Double_value of float (** Double value. *)
  | Float_value of float (** Float value. *)
  | Int_value of Utils.s4 (** Int value. *)
  | Long_value of Utils.s8 (** Long value. *)
  | Object_value of object_value (** Object value. *)
  | Short_value of Utils.s2 (** Short value. *)
(** The type of field values. *)

val make_descriptor : Utils.UTF8.t -> Utils.s8 -> object_value list ->
  (Descriptor.for_field * Name.for_field) list -> descriptor option ->
  bool -> (read_method * write_method) option -> descriptor
(** [make_descriptor name serial annot fields super ext methods]
    constructs a new descriptor for class [name] with serial identifier
    [serial], annotations [annot], fields [fields], and super descriptor
    [super]. [ext] indicates whether the class implements
    {i java.io.Externalizable}, and methods provides optional custom
    read/write methods. *)

val make_proxy_descriptor : Utils.UTF8.t list -> object_value list -> descriptor option -> descriptor
(** [make_proxy_descriptor interfaces annot super] constructs a
    descriptor for a proxy class with annotations [annot], and super
    descriptor [super]. [interfaces] is the list of interfaces
    implemented by the proxy. *)

val serial_of_descriptor : descriptor -> Utils.s8
(** Returns the serial version of the passed descriptor. *)

val class_name_of_descriptor : descriptor -> Utils.UTF8.t
(** Returns the class name of the passed descriptor. *)

val class_annotation_of_descriptor : descriptor -> object_value list
(** Returns the annotations of the passed descriptor. *)

val super_class_desc_of_descriptor : descriptor -> descriptor option
(** Returns the parent descriptor of the passed descriptor. *)

val fields_of_descriptor : descriptor -> (Descriptor.for_field * Name.for_field) list
(** Returns the fields of the passed descriptor. *)

val methods_of_descriptor : descriptor -> (read_method * write_method) option
(** Returns the {i Externalizable} methods of the passed descriptor. *)

val make_instance : descriptor -> (Name.for_field * field_value) list -> object_value list -> instance
(** [make_instance desc fields annot] constructs an instance associated
    with descriptor [desc] and annotations [annot]. The association list
    [fields] should contain an element (with correct type) for each field
    referenced in the descritor. *)


(** {6 Serialization and deserialization of object values} *)

val encode : OutputStream.t -> object_value list -> unit
(** [encode os l] writes the values from [l] onto [os].
    Raises [OutputStream.Exception] if an i/o error occurs. *)

val encode_one : OutputStream.t -> object_value -> unit
(** [encode_one os x] is a shorthand for [encode os [x]]. *)

val decode : InputStream.t -> object_value list
(** [decode is] returns the list of values read from [is].
    Raises [Exception] if data on the stream does not conform to the
    serialization protocol.
    Raises [OutputStream.Exception] if an i/o error occurs. *)


(** {6 Helper functions} *)

val descriptor_of_definition : ClassLoader.t -> ClassDefinition.t -> descriptor
(** [descriptor_of_definition loader definition] constructs a new descriptor
    for the class whose [definition] is passed, [loader] being used to
    construct descriptors for parent classes.
    Raises [ClassLoader.Exception] if a parent class cannot be found. *)

val instance_of_function : descriptor -> object_value list -> (Name.for_field -> field_value) -> instance
(** [instance_of_function desc annot f] constructs an instance associated with
    descriptor [desc] and annotations [annot]. [f] is used to retrieve the
    value of each field. *)

val object_value_of_function : descriptor -> object_value list -> (Name.for_field -> field_value) -> object_value
(** [object_value_of_function desc annot f] is a shorthand for
    [Instance (instance_of_function desc annot f)]. *)
