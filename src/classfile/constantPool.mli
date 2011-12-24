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

(** Constant pool definition as well as i/o functions. *)


(** {6 Types} *)

type field_reference = Name.for_class * Name.for_field * Descriptor.for_field
(** Type for references to a field (class name, field name, and field descriptor). *)

type method_reference = Name.for_class * Name.for_method * Descriptor.for_method
(** Type for references to a method (class name, method name, and method descriptor). *)

type constructor_reference = Name.for_class * (Descriptor.for_parameter list)
(** Type for references to a constructor (class name, and parameter list). *)

type reference =
  | Reference_getField of field_reference
    (** Reference equivalent to a 'getField' call. *)
  | Reference_getStatic of field_reference
    (** Reference equivalent to a 'getStatic' call. *)
  | Reference_putField of field_reference
     (** Reference equivalent to a 'putField' call. *)
  | Reference_putStatic of field_reference
    (** Reference equivalent to a 'putStatic' call. *)
  | Reference_invokeVirtual of method_reference
    (** Reference equivalent to a 'invokeVirtual' call. *)
  | Reference_invokeStatic of method_reference
    (** Reference equivalent to a 'invokeStatic' call. *)
  | Reference_invokeSpecial of method_reference
    (** Reference equivalent to a 'invokeSpecial' call. *)
  | Reference_newInvokeSpecial of constructor_reference
    (** Reference equivalent to a 'newInvokeSpecial' call. *)
  | Reference_invokeInterface of method_reference
    (** Reference equivalent to a 'invokeInterface' call. *)
(** Type for references to an element from a method handle. *)

type reference_kind =
  | REF_getField (** Reference equivalent to a 'getField' call. *)
  | REF_getStatic (** Reference equivalent to a 'getStatic' call. *)
  | REF_putField (** Reference equivalent to a 'putField' call. *)
  | REF_putStatic (** Reference equivalent to a 'putStatic' call. *)
  | REF_invokeVirtual (** Reference equivalent to a 'invokeVirtual' call. *)
  | REF_invokeStatic (** Reference equivalent to a 'invokeStatic' call. *)
  | REF_invokeSpecial (** Reference equivalent to a 'invokeSpecial' call. *)
  | REF_newInvokeSpecial (** Reference equivalent to a 'newInvokeSpecial' call. *)
  | REF_invokeInterface (** Reference equivalent to a 'invokeInterface' call. *)
(** Type for reference kinds to an element from a method handle. *)

type element =
  | Class of Utils.u2
    (** name index (UTF8 element: class name in internal format) *)
  | Fieldref of Utils.u2 * Utils.u2
    (** class index (Class element: enclosing class or interface),
        name and type index (NameAndType element: field descriptor) *)
  | Methodref of Utils.u2 * Utils.u2
    (** class index (Class element: class only),
        name and type index (NameAndType element: method descriptor) *)
  | InterfaceMethodref of Utils.u2 * Utils.u2
    (** class index (Class element: interface only),
        name and type index (NameAndType element: method descriptor) *)
  | String of Utils.u2 (** value index (UTF8 element) *)
  | Integer of int32 (** value (big-endian) *)
  | Float of int32 (** value (IEEE 754 / big-endian) *)
  | Long of int32 * int32 (** high bytes, low bytes (big-endian) *)
  | Double of int32 * int32 (** high bytes, low bytes (IEEE 754 / big-endian) *)
  | NameAndType of Utils.u2 * Utils.u2
    (** name index (UTF8 element: "<init>", "<clinit>" or unqualified field / method name),
        descriptor index (Utf8 element: field / method descriptor) *)
  | UTF8 of Utils.UTF8.t (** value (in "normal" format) *)
  | MethodHandle of reference_kind * Utils.u2 (** reference kind, reference index *)
  | MethodType of Utils.u2 (** type index *)
  | InvokeDynamic of Utils.u2 * Utils.u2
    (** index into "BootstrapMethods" attribute, name and type index *)
  | ModuleId of Utils.u2 * Utils.u2
    (** name index (Utf8 element), version index (Utf8 element) *)
(** Type for constant pool entry. *)

val dummy_element : element
(** Element used for 0-index entry as well as for unused long or double
    entries. *)

type tag =
  | CONSTANT_Class (** Tag for 'Class' element *)
  | CONSTANT_Fieldref (** Tag for 'Fieldref' element *)
  | CONSTANT_Methodref (** Tag for 'Methodref' element *)
  | CONSTANT_InterfaceMethodref (** Tag for 'InterfaceMethodref' element *)
  | CONSTANT_String (** Tag for 'String' element *)
  | CONSTANT_Integer (** Tag for 'Integer' element *)
  | CONSTANT_Float (** Tag for 'Float' element *)
  | CONSTANT_Long (** Tag for 'Long' element *)
  | CONSTANT_Double (** Tag for 'Double' element *)
  | CONSTANT_NameAndType (** Tag for 'NameAndType' element *)
  | CONSTANT_Utf8 (** Tag for 'UTF8' element *)
  | CONSTANT_MethodHandle (** Tag for 'MethodHandle' element *)
  | CONSTANT_MethodType (** Tag for 'MethodType' element *)
  | CONSTANT_InvokeDynamic (** Tag for 'InvokeDynamic' element *)
  | CONSTANT_ModuleId (** Tag for 'ModuleId' element *)
(** Type for tag identifying element kind. *)

type t
(** Type for constant pools. *)


(** {6 Exception} *)

type error =
  | Invalid_reference_kind of Utils.u1
  | Invalid_tag of Utils.u1
  | Too_large of int
  | Invalid_reference
  | Reference_out_of_bounds of int * int
  | Dummy_access of Utils.u2
  | Malformed_Class_entry of Utils.u2
  | Malformed_Fieldref_entry of Utils.u2 * Utils.u2
  | Malformed_Methodref_entry of Utils.u2 * Utils.u2
  | Malformed_InterfaceMethodRef_entry of Utils.u2 * Utils.u2
  | Malformed_String_entry of Utils.u2
  | Malformed_NameAndType_entry of Utils.u2 * Utils.u2
  | Malformed_MethodHandle_entry of reference_kind * Utils.u2
  | Malformed_MethodType_entry of Utils.u2
  | Malformed_InvokeDynamic_entry of Utils.u2 * Utils.u2
  | Malformed_ModuleId_entry of Utils.u2 * Utils.u2
  | Unexpected_tag of int * int (* fst expected, snd found *)

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 I/O functions} *)

val read_element : InputStream.t -> element
(* [read_element st] reads an element from [st].
   Raises [InputStream.Exception] if an i/o error occurs.
   Raises [Exception] if stream does not contain a valid pool element. *)

val read : InputStream.t -> Utils.u2 -> t
(* [read st sz] reads a constant pool of size [sz] from [st].
   Raises [InputStream.Exception] if an i/o error occurs.
   Raises [Exception] if stream does not contain [sz] valid pool
   elements, or if [(sz >= 65536) || (sz < 0)]. *)

val write_element : OutputStream.t -> element -> unit
(* [write_element elem st] writes [elem] onto [st] if not dummy.
   Raises [OutputStream.Exception] if an i/o error occurs. *)

val write : OutputStream.t -> t -> unit
(* [write pool st] writes [pool] elements onto stream; size is not
   written. Dummy values are not written.
   Raises [OutputStream.Exception] if an i/o error occurs.
   Raises [Exception] if pool is too large. *)


(** {6 Checking and queries} *)

val size : t -> Utils.u2
(** Returns the size of the passed pool. *)

val get_entry : t -> Utils.u2 -> element
(** [get_entry pool index] returns the entry at [index] in [pool] if
    [index] is valid, raising [Exception] otherwise.
    Raises [Exception] if an attempt is made to get a dummy element. *)

val get_utf8_entry : t -> Utils.u2 -> Utils.UTF8.t
(** A specialized version of [get_entry] that unpacks a UTF8 string if the entry
    is a UTF8 string, and raises an exception otherwise. *)

val get_class_name : t -> Utils.u2 -> Name.for_class
(** A specialized version of [get_entry] that also builds a high-level name.
    An exception is raised if the given index does not point to a class name. *)

val check : t -> unit
(** Checks the passed pool for consistency.
    Raises [Exception] if pool is not consistent. *)

val check_entry_for_kind : t -> Utils.u2 -> tag -> bool
(** [check_entry_for_kind pool index tag] tests whether the entry at
    [index] is of the kind designated by [tag]. *)

val version_bounds : element -> Version.bounds
(** Returns the version bounds for the passed pool element. *)

val check_version : Version.t -> t -> unit
(** [check_version v pool] checks that [pool] conforms to version [v],
    raising [Version.Exception] if not. *)


(** {6 Extendable pools} *)

type extendable
(** Type for extendable constant pools (used for conversion from class
    definitions to class files. The [add_xyz] functions below provide
    maximum sharing of constant pool elements. *)

val make_extendable : unit -> extendable
(** Builds an empty extendable pool. *)

val make_extendable_from_pool : t -> extendable
(** Builds an extendable pool from a {i classic} one.
    Raises [Exception] if the passed pool is too large. *)

val get_extendable_entry : extendable -> Utils.u2 -> element
(** [get_extendable_entry pool index] returns the entry at [index] in
    [pool] if [index] is valid, raising [Exception] otherwise.
    Raises [Exception] if an attempt is made to get a dummy element. *)

val add_class : extendable -> Name.for_class -> Utils.u2
(** [add_class pool class_name] augments [pool] with class whose name is
    [class_name]. Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_array_class : extendable -> Descriptor.array_type -> Utils.u2
(** [add_array_class pool array_type] augments [pool] with array type
    whose descriptor is [array_type]. Returns index of existing or
    created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_field : extendable -> Name.for_class -> Name.for_field -> Descriptor.for_field -> Utils.u2
(** [add_field pool class_name field_name field_type] augments [pool]
    with field [field_name] of type [field_type] in class [class_name].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_method : extendable -> Name.for_class -> Name.for_method -> Descriptor.for_method -> Utils.u2
(** [add_method pool class_name method_name method_type] augments [pool]
    with method [method_name] of type [method_type] in class [class_name].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_interface_method : extendable -> Name.for_class -> Name.for_method -> Descriptor.for_method -> Utils.u2
(** [add_interface_method pool interface_name method_name method_type] augments
    [pool] with interface method [method_name] of type [method_type] in
    interface [interface_name].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_array_method : extendable -> Descriptor.array_type -> Name.for_method -> Descriptor.for_method -> Utils.u2
(** [add_array_method pool array_type method_name method_type] augments
    [pool] with array method [method_name] of type [method_type] in array
    type [array_type].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_string : extendable -> Utils.UTF8.t -> Utils.u2
(** [add_string pool s] augments [pool] with string [s].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_integer : extendable -> int32 -> Utils.u2
(** [add_integer pool i] augments [pool] with integer [i].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_float : extendable -> float -> Utils.u2
(** [add_float pool f] augments [pool] with float [f].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_long : extendable -> int64 -> Utils.u2
(** [add_long pool l] augments [pool] with long [l].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_double : extendable -> float -> Utils.u2
(** [add_double pool d] augments [pool] with double [d].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_name_and_type : extendable -> Utils.UTF8.t -> Utils.UTF8.t -> Utils.u2
(** [add_name_and_type pool n t] augments [pool] with mane [n] and type
    [t].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_utf8 : extendable -> Utils.UTF8.t -> Utils.u2
(** [add_utf8 pool s] augments [pool] with constant [s].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_method_handle : extendable -> reference -> Utils.u2
(** [add_method_handle pool reference] augments [pool] with an handle to
    [reference].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_method_type : extendable -> Descriptor.for_method -> Utils.u2
(** [add_method_type pool method_type] augments [pool] with method type
    [method_type].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_invoke_dynamic : extendable -> Utils.u2 -> Name.for_method -> Descriptor.for_method -> Utils.u2
(** [add_invoke_dynamic pool index method_name method_type] augments
    [pool] with invoke dynamic with [index] into {i BootstrapMethods}
    attributes, method [method_name] of type [method_type].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val add_moduleid : extendable -> Utils.UTF8.t -> Utils.UTF8.t -> Utils.u2
(** [add_moduleid pool n v] augments [pool] with name [n] and version
    [v].
    Returns index of existing or created entry.
    Raises [Exception] if the passed pool is too large. *)

val to_pool : extendable -> t
(** Converts an extendable pool into a pool. *)
