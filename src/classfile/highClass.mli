(**
  A truly high-level representation of Java bytecode.

    - agnostic of any byte counts (such as offsets or instruction sizes)
    - no instruction variants whose goal is to save speed/memory
      (such as ICONST/ICONST_0, LDC/LDC_W)
    - no constant pool
 *)

(* NOTE: This is supposed to be what [ClassDefinition] should have been in
the first place. *)

module HighInstruction : sig (* {{{ *)
  type t
  type label
  module LabelHash : Hashtbl.S with type key = label

  val version_bounds : t -> Version.bounds
end (* }}} *)

module HighAttribute : sig (* {{{ *)
  type constant_value =
    | Long_value of int64
    | Float_value of float
    | Double_value of float
    | Boolean_value of bool
    | Byte_value of int
    | Character_value of int
    | Short_value of int
    | Integer_value of int32
    | String_value of Utils.UTF8.t

  type inner_class_element = {
      inner_class : Name.for_class option;
      outer_class : Name.for_class option;
      inner_name : Utils.UTF8.t option;
      inner_flags : AccessFlag.for_inner_class list;
    }

  type enclosing_method_value = {
      innermost_class : Name.for_class;
      enclosing_method : (Name.for_method * Descriptor.for_method) option;
    }

  type code_attribute = [
    | `LineNumberTable of int HighInstruction.LabelHash.t
    | `Unknown of Utils.UTF8.t * string

    (* TODO: Treating these properly requires some symbolic execution. *)
    | `LocalVariableTable of unit (** types for local variables *)
    | `LocalVariableTypeTable of unit (** signatures for local variables *)
  ]

  type exception_table_element = {
      try_start : HighInstruction.label;
      try_end : HighInstruction.label;
      catch : HighInstruction.label;
      caught : Name.for_class option;
    }

  type code_value = {
      code : HighInstruction.t list;
      exception_table : exception_table_element list;
      attributes : code_attribute list;
    }

  type for_class =
    [ `InnerClasses of inner_class_element list
    | `EnclosingMethod of enclosing_method_value
    | `Synthetic (** auto-generated element *)
    | `ClassSignature of Signature.class_signature
    | `SourceFile of Utils.UTF8.t
    | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
    | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
    | `Unknown of Utils.UTF8.t * string ]

  type for_method =
    [ `Code of code_value
    | `Exceptions of Name.for_class list
    | `Synthetic (** auto-generated element *)
    | `MethodSignature of Signature.method_signature
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleParameterAnnotations of Annotation.t list list
    | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `AnnotationDefault of Annotation.element_value
    | `Unknown of Utils.UTF8.t * string ]

  type for_field =
    [ `ConstantValue of constant_value
    | `Synthetic
    | `Signature of [`Field of Signature.field_type_signature]
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `Unknown of Utils.UTF8.t * string ]

  type t = [ for_class | for_method | for_field | code_attribute ]

  val version_bounds : t -> Version.bounds

  val decode_method : ConstantPool.t -> Attribute.info -> for_method
  val decode_class :  ConstantPool.t -> Attribute.info -> for_class

  val encode_method :  ConstantPool.t -> for_method -> Attribute.info
  val encode_class :  ConstantPool.t -> for_class -> Attribute.info

end (* }}} *)

module HighMethod : sig (* {{{ *)
  type regular = {
      flags : AccessFlag.for_method list;
      name : Name.for_method;
      descriptor : Descriptor.for_method;
      attributes : HighAttribute.for_method list;
    }

  type constructor = {
      cstr_flags : AccessFlag.for_constructor list;
      cstr_descriptor : Descriptor.for_parameter list;
      cstr_attributes : HighAttribute.for_method list;
    }

  type class_initializer = {
      init_flags : AccessFlag.for_initializer list;
      init_attributes : HighAttribute.for_method list;
    }

  type t =
    | Regular of regular
    | Constructor of constructor
    | Initializer of class_initializer

  val decode : bool -> ConstantPool.t -> Method.info -> t

  val encode : ConstantPool.extendable -> t -> Method.info
end (* }}} *)

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : HighMethod.t list;
    attributes : HighAttribute.for_class list;
  }

type error =
  | Invalid_attribute_name
  | Invalid_class_name
  | Invalid_code_attribute
  | Invalid_code_length
  | Invalid_constant_value
  | Invalid_descriptor
  | Invalid_exception_name
  | Invalid_method_handle
  | Invalid_module
  | Invalid_pool_element
  | Invalid_pool_entry
  | Invalid_pool_entry_type of (ConstantPool.element * string)
  | Invalid_pool_index
  | Invalid_primitive_array_type
  | Invalid_source_file
  | Misplaced_attribute of (string * string)
  | Too_many of string
  | Unsupported_instruction of string

exception Exception of error

val check_version_high : ?version : Version.t -> t -> t
val decode : ?version : Version.t -> ClassFile.t -> t
val encode : ?version : Version.t -> t -> ClassFile.t

(*
vim:tw=0:
*)
