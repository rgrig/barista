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

(** Attributes in both low- and high-level forms.
    It also provides conversion functions between levels as well as i/o
    functions for low-level. *)


(** {6 Low-level form} *)

type info = {
    name_index : Utils.u2;
    length : Utils.u4;
    data : string;
  }
(** Represents an attribute as defined in the class file format specification. *)


(** {6 Exception} *)

type error =
  | Invalid_code_attribute
  | Invalid_code_length
  | Defined_twice of string
  | Invalid_field_attribute
  | Invalid_method_attribute
  | Invalid_class_attribute
  | Invalid_package_attribute
  | Invalid_module_attribute
  | Invalid_constant_value
  | Invalid_enclosing_method
  | Invalid_local_variable_table
  | Invalid_local_variable_type_table
  | Invalid_list_length
  | Invalid_attribute_name_value
  | Invalid_exception_name
  | Invalid_exception
  | Invalid_inner_class
  | Invalid_outer_class
  | Invalid_signature
  | Invalid_source_file
  | Invalid_stack_map_frame
  | Invalid_stack_map_verification_type
  | Invalid_bootstrap_method_handle
  | Invalid_bootstrap_argument
  | Invalid_module
  | Invalid_module_dependency_kind
  | Missing_module_attribute

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 I/O functions} *)

val read_info : InputStream.t -> info
(** [read_info st] reads an attribute from [st].
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_info : OutputStream.t -> info -> unit
(** [write_info st a] writes an attribute [a] onto [st].
    Raises [OutputStream.Exception] if an i/o error occurs. *)


(** {6 High-level form} *)

type constant_value =
  | Long_value of int64 (** long constant *)
  | Float_value of float (** float constant *)
  | Double_value of float (** double constant *)
  | Boolean_value of bool (** boolean constant *)
  | Byte_value of int (** byte constant *)
  | Character_value of int (** char constant *)
  | Short_value of int (** short constant *)
  | Integer_value of int32 (** int constant *)
  | String_value of Utils.UTF8.t (** string constant *)
(** Constant values used by ConstantValue attribute. *)

type verification_type_info =
  | Top_variable_info (** {i top} type. *)
  | Integer_variable_info (** {i int} type. *)
  | Float_variable_info (** {i float} type. *)
  | Long_variable_info (** {i long} type. *)
  | Double_variable_info (** {i double} type. *)
  | Null_variable_info (** {i null} value type. *)
  | Uninitialized_this_variable_info (** uninitialized type (for {i this}). *)
  | Object_variable_info of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] (** {i Object} type (parameter is the class name). *)
  | Uninitialized_variable_info of Utils.u2 (** uninitialized type (parameter is the code offset of the {i new} instruction that created the object). *)
(** Verification types. *)

type stack_map_frame =
  | Same_frame of Utils.u2 (** Same locals, operand stack is empty (parameter is code offset). *)
  | Same_locals_1_stack_item_frame of Utils.u2 * verification_type_info (** Same locals with one stack item (parameters are code offset and type of stack item). *)
  | Chop_1_frame of Utils.u2 (** Frame choped by 1 local element, operand stack is empty (parameter is code offset). *)
  | Chop_2_frame of Utils.u2 (** Frame choped by 2 local elements, operand stack is empty (parameter is code offset). *)
  | Chop_3_frame of Utils.u2 (** Frame choped by 3 local elements, operand stack is empty (parameter is code offset). *)
  | Append_1_frame of Utils.u2 * verification_type_info (** Frame appended by 1 local element, operand stack is empty (parameters are code offset and type of additional element). *)
  | Append_2_frame of Utils.u2 * verification_type_info * verification_type_info (** Frame appended by 2 local elements, operand stack is empty (parameters are code offset and types of additional elements). *)
  | Append_3_frame of Utils.u2 * verification_type_info * verification_type_info * verification_type_info (** Frame appended by 3 local elements, operand stack is empty (parameters are code offset and types of additional elements). *)
  | Full_frame of Utils.u2 * (verification_type_info list) * (verification_type_info list) (** Full frame definition (parameters are code offset and lists of locals and stack elements). *)
(** Stack map frame modifications. *)

type dependency_kind =
  | Optional_dependency (** The dependency is optional. *)
  | Same_class_loader (** Both modules must be loaded by the same class loader. *)
  | Not_observable (** The depended module types are not observable from the depending module. *)
(** The kinds of module-to-module dependencies. *)

type t =
  [ `ConstantValue of constant_value (** attribute for field initialization *)
  | `Code of code_value (** attribute for method code *)
  | `Exceptions of Name.for_class list (** attribute for thrown exceptions *)
  | `InnerClasses of inner_class_element list (** inner classes *)
  | `EnclosingMethod of enclosing_method_value (** inner-most class and inner-most method *)
  | `Synthetic (** auto-generated element *)
  | `Signature of signature (** element signature *)
  | `SourceFile of Utils.UTF8.t (** source file for class/package/module *)
  | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
  | `LineNumberTable of (Utils.u2 * Utils.u2) list (** (code address, source line) mapping *)
  | `LocalVariableTable of local_variable_table_element list (** types for local variables *)
  | `LocalVariableTypeTable of local_variable_type_table_element list (** signatures for local variables *)
  | `Deprecated (** deprecated elements *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeVisibleParameterAnnotations of Annotation.t list list (** annotations *)
  | `RuntimeInvisibleParameterAnnotations of Annotation.t list list (** annotations *)
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `AnnotationDefault of Annotation.element_value (** annotations default value *)
  | `StackMapTable of stack_map_frame list (** stack map table *)
  | `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
  | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
  | `ModuleRequires of (Utils.UTF8.t * Utils.UTF8.t * dependency_kind) list (** module dependencies *)
  | `ModulePermits of (Utils.UTF8.t * Utils.UTF8.t) list (** module exported elements *)
  | `ModuleProvides of (Utils.UTF8.t * Utils.UTF8.t) list (** module exported elements *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
and signature =
  [ `Class of Signature.class_signature (** Signature for a class. *)
  | `Method of Signature.method_signature (** Signature for a method. *)
  | `Field of Signature.field_type_signature (** Signature for a field. *) ]
and code_attribute = [
  | `LineNumberTable of (Utils.u2 * Utils.u2) list (** line numbers *)
  | `LocalVariableTable of local_variable_table_element list (** types for local variables *)
  | `LocalVariableTypeTable of local_variable_type_table_element list (** signatures for local variables *)
  | `StackMapTable of stack_map_frame list (** stack map table *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
and code_value = {
    max_stack : Utils.u2;
    max_locals : Utils.u2;
    code : Instruction.t list;
    exception_table : exception_table_element list;
    attributes : code_attribute list;
  }
and exception_table_element = {
    try_start : Utils.u2;
    try_end : Utils.u2;
    catch : Utils.u2;
    caught : Name.for_class option;
  }
and inner_class_element = {
    inner_class : Name.for_class option;
    outer_class : Name.for_class option;
    inner_name : Utils.UTF8.t option;
    inner_flags : AccessFlag.for_inner_class list;
  }
and enclosing_method_value = {
    innermost_class : Name.for_class;
    enclosing_method : (Name.for_method * Descriptor.for_method) option;
  }
and local_variable_table_element = {
    local_start : Utils.u2;
    local_length : Utils.u2;
    local_name : Utils.UTF8.t;
    local_descriptor : Descriptor.for_field;
    local_index : Utils.u2;
  }
and local_variable_type_table_element = {
    local_type_start : Utils.u2;
    local_type_length : Utils.u2;
    local_type_name : Utils.UTF8.t;
    local_type_signature : Signature.field_type_signature;
    local_type_index : Utils.u2;
  }

type for_field =
  [ `ConstantValue of constant_value (** attribute for field initialization *)
  | `Synthetic (** auto-generated element *)
  | `Signature of [`Field of Signature.field_type_signature] (** element signature *)
  | `Deprecated (** deprecated elements *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
(** Possible attributes for a field. *)

type for_method =
  [ `Code of code_value (** attribute for method code *)
  | `Exceptions of Name.for_class list (** attribute for thrown exceptions *)
  | `Synthetic (** auto-generated element *)
  | `Signature of [`Method of Signature.method_signature] (** element signature *)
  | `Deprecated (** deprecated elements *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeVisibleParameterAnnotations of Annotation.t list list (** annotations *)
  | `RuntimeInvisibleParameterAnnotations of Annotation.t list list (** annotations *)
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `AnnotationDefault of Annotation.element_value (** annotations default value *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
(** Possible attributes for a method. *)

type for_class =
  [ `InnerClasses of inner_class_element list (** inner classes *)
  | `EnclosingMethod of enclosing_method_value (** inner-most class and inner-most method *)
  | `Synthetic (** auto-generated element *)
  | `Signature of [`Class of Signature.class_signature] (** element signature *)
  | `SourceFile of Utils.UTF8.t (** source file for class *)
  | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
  | `Deprecated (** deprecated elements *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list (** extended annotations *)
  | `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
  | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
(** Possible attributes for a class. *)

type for_package =
  [ `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
  | `SourceFile of Utils.UTF8.t (** source file for package *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
(** Possible attributes for a package. *)

type for_module =
  [ `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
  | `SourceFile of Utils.UTF8.t (** source file for package *)
  | `ModuleRequires of (Utils.UTF8.t * Utils.UTF8.t * dependency_kind) list (** module dependencies *)
  | `ModulePermits of (Utils.UTF8.t * Utils.UTF8.t) list (** module exported elements *)
  | `ModuleProvides of (Utils.UTF8.t * Utils.UTF8.t) list (** module exported elements *)
  | `RuntimeVisibleAnnotations of Annotation.t list (** annotations *)
  | `RuntimeInvisibleAnnotations of Annotation.t list (** annotations *)
  | `Unknown of Utils.UTF8.t * string (** unknown attribute *) ]
(** Possible attributes for a module. *)

type enclosing_element =
  | Class (** Kind of classes. *)
  | Method (** Kind of methods. *)
  | Field (** Kind of fields. *)
  | Package (** Kind of package. *)
  | Module (** Kind of module. *)
(** Kinds of elements. *)


(** {6 Conversion functions} *)

val string_of_verification_type_info : verification_type_info -> string
(** Converts the passed verification type information into a string. *)

val verification_type_info_of_parameter_descriptor : Descriptor.for_parameter -> verification_type_info
(** Converts the passed descriptor into a verification type information. *)

val equal_verification_type_info : verification_type_info -> verification_type_info -> bool
(** Equality over verification type informations. *)

val check_code_attributes : t list -> code_attribute list
(** Checks that the passed list is a valid one for a code attribute.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val check_field_attributes : t list -> for_field list
(** Checks that the passed list is a valid one for a field.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val check_method_attributes : t list -> for_method list
(** Checks that the passed list is a valid one for a method.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val check_class_attributes : t list -> for_class list
(** Checks that the passed list is a valid one for a class.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val check_package_attributes : t list -> for_package list
(** Checks that the passed list is a valid one for a package.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val check_module_attributes : t list -> for_module list
(** Checks that the passed list is a valid one for a module.
    The list is returned if it is valid, raising [Exception] otherwise. *)

val decode : enclosing_element -> Bootstrap.methods -> ConstantPool.t -> info -> t
(** Converts from a low-level into a high-level form according to passed pool,
    and method bootstrap information.
    Raises [Exception] if an error occurs during conversion. *)

val encode : Bootstrap.methods -> ConstantPool.extendable -> t -> info
(** Converts from a high-level into a low-level form, using passed pool.
    Raises [Exception] if an error occurs during conversion. *)

val compare : t -> t -> int
(** Comparison over attributes. *)

val version_bounds : t -> Version.bounds
(** Returns the version bounds for the passed attribute. *)


(** {6 Common extractors} *)

val extract_code : t list -> code_value
(** Returns the contents of the {i Code} attribute from the passed list.
    Raises [Not_found] if no such attribute exists. *)

val extract_exceptions : t list -> Name.for_class list
(** Returns the contents of the {i Exceptions} attribute from the passed
    list.
    Raises [Not_found] if no such attribute exists. *)

val extract_class_signature : for_class list -> Signature.class_signature
(** Returns the contents of the {i Signature} attribute from the passed
    list.
    Raises [Not_found] if no such attribute exists. *)

val extract_bootstrap_info : t list -> Bootstrap.method_specifier list
(** Returns the contents of the {i BootstrapMethods} attribute from the
    passed list.
    Raises [Not_found] if no such attribute exists. *)

val extract_field_signature : for_field list -> Signature.field_type_signature
(** Returns the contents of the {i Signature} attribute from the passed
    list.
    Raises [Not_found] if no such attribute exists. *)

val extract_method_signature : for_method list -> Signature.method_signature
(** Returns the contents of the {i Signature} attribute from the passed
    list.
    Raises [Not_found] if no such attribute exists. *)

val extract_annotations : t list -> Annotation.t list
(** Returns the list of annotations from the passed list.
    Returns the empty list if no such annotation exists. *)
