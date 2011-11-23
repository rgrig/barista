(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module CF = ClassFile
module U = Utils

(* }}} *)

(* errors *) (* {{{ *)
type error =
  | Invalid_class_name

exception Exception of error

let fail e = raise (Exception e)

(* }}} *)
module Instruction = struct (* {{{ *)
  type t = unit
  type label = int
  module LabelHash = Hashtbl.Make (struct
    type t = label
    let equal = (=)
    let hash x = x
  end)
end (* }}} *)

module Attribute = struct (* {{{ *)
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
    | `LineNumberTable of int Instruction.LabelHash.t
    | `Unknown of Utils.UTF8.t * string ]
(* TODO
    | `LocalVariableTable of unit (** types for local variables *)
    | `LocalVariableTypeTable of local_variable_type_table_element list (** signatures for local variables *)
    | `StackMapTable of stack_map_frame list
 *)

  type exception_table_element = {
      try_start : Instruction.label;
      try_end : Instruction.label;
      catch : Instruction.label;
      caught : Name.for_class option;
    }

  type code_value = {
      code : Instruction.t list;
      exception_table : exception_table_element list;
      attributes : code_attribute list;
    }

  type for_class =
    [ `InnerClasses of inner_class_element list
    | `EnclosingMethod of enclosing_method_value
    | `Synthetic (** auto-generated element *)
    | `Signature of [`Class of Signature.class_signature]
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
    | `Signature of [`Method of Signature.method_signature]
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleParameterAnnotations of Annotation.t list list
    | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `AnnotationDefault of Annotation.element_value
    | `Unknown of Utils.UTF8.t * string ]

  let decode _ _ _ = failwith "todo"
end (* }}} *)

module Method = struct (* {{{ *)
  type regular = {
      flags : AccessFlag.for_method list;
      name : Name.for_method;
      descriptor : Descriptor.for_method;
      attributes : Attribute.for_method list;
    }

  type constructor = {
      cstr_flags : AccessFlag.for_constructor list;
      cstr_descriptor : Descriptor.for_parameter list;
      cstr_attributes : Attribute.for_method list;
    }

  type class_initializer = {
      init_flags : AccessFlag.for_initializer list;
      init_attributes : Attribute.for_method list;
    }

  type t =
    | Regular of regular
    | Constructor of constructor
    | Initializer of class_initializer

  let decode _ _ _ = failwith "todo"
end (* }}} *)

module A = Attribute
module M = Method

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : M.t list;
    attributes : A.for_class list;
  }

let check_version_high ?(version = Version.default) c =
  ignore version;
  if true then failwith "todo";
  c

let decode ?(version = Version.default) cf =
  let pool = cf.CF.constant_pool in
  let check_version v =
    let v' = cf.CF.major_version, cf.CF.minor_version in
    let v' = Version.version_of_major_minor v' in
    Version.at_most "class file version" v v';
    (* TODO: The following line should be [ClassFile.check ...]. *)
    ConstantPool.check_version v' pool in
  let get_class_name idx = match ConstantPool.get_entry pool idx with
    | ConstantPool.Class idx' ->
        (match ConstantPool.get_entry pool idx' with
          | ConstantPool.UTF8 n ->
              Name.make_for_class_from_internal n
          | _ -> fail Invalid_class_name)
    | _ -> fail Invalid_class_name in
  check_version version;
  let flags = AF.check_class_flags (AF.from_u2 false cf.CF.access_flags) in
  let extends =
    if cf.CF.super_class = U.u2 0
    then None
    else Some (get_class_name cf.ClassFile.super_class) in
  let is_interface = List.mem `Interface flags in
  let field_decode = Field.decode is_interface pool in
  let method_decode = Method.decode is_interface pool in
  let attribute_decode = Attribute.decode is_interface pool in
  check_version_high ~version:version { access_flags = flags;
    name = get_class_name cf.CF.this_class;
    extends = extends;
    implements = List.map get_class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }

let encode ?(version = Version.default) _ = ignore version; failwith "todo"

