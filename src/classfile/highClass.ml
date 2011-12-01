(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module CF = ClassFile
module U = Utils

(* }}} *)

(* errors *) (* {{{ *)
type error =
  | Invalid_class_name
  | Invalid_module
  | Invalid_attribute_name
  | Invalid_constant_value

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

module OA = Attribute (* {{{ *)
  (* used only for the low-level stuff, which is unchanged *)

module Attribute = struct
  open Consts

  type enclosing_elemen =
    | Class
    | Method
    | Field
    | Package
    | Module

  (* high-level *)

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

  type t = [ for_class | for_method | for_field ]

  (* helper functions *)

  let get_utf8 pool idx err =
    match ConstantPool.get_entry pool idx with
    | ConstantPool.UTF8 v -> v
    | _ -> fail err

  let read_annotations pool st =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_info st in
        Annotation.decode pool a)

  let read_extended_annotations pool st =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_extended_info st in
        Annotation.decode_extended pool a)

  let read_annotations_list pool st =
    let nb = InputStream.read_u1 st in
    let res = ref [] in
    for i = 1 to (nb :> int) do
      let local =
        InputStream.read_elements
          st
          (fun st ->
            let a = Annotation.read_info st in
            Annotation.decode pool a) in
      res := local :: !res
    done;
    List.rev !res

  let read_module_info pool st =
    let module_index = InputStream.read_u2 st in
    let name_index, version_index =
      match ConstantPool.get_entry pool module_index with
      | ConstantPool.ModuleId (n, v) -> n, v
      | _ -> fail Invalid_module in
    let name = get_utf8 pool name_index Invalid_module in
    let version = get_utf8 pool version_index Invalid_module in
    name, version

  let decode_attr_constant_value _ pool _ st =
        let const_index = InputStream.read_u2 st in
        match ConstantPool.get_entry pool const_index with
        | ConstantPool.Long (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Long_value v)
        | ConstantPool.Float v ->
            `ConstantValue (Float_value (Int32.float_of_bits v))
        | ConstantPool.Double (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Double_value (Int64.float_of_bits v))
        | ConstantPool.Integer v ->
            `ConstantValue (Integer_value v)
        | ConstantPool.String idx ->
            `ConstantValue (String_value (get_utf8 pool idx Invalid_constant_value))
        | _ -> fail Invalid_constant_value

  let decode_attr_code _ = failwith "todo"
  let decode_attr_exceptions _ = failwith "todo"
  let decode_attr_inner_classes _ = failwith "todo"
  let decode_attr_enclosing_method _ = failwith "todo"
  let decode_attr_synthetic _ = failwith "todo"
  let decode_attr_signature _ = failwith "todo"
  let decode_attr_source_file _ = failwith "todo"
  let decode_attr_source_debug_extension _ = failwith "todo"
  let decode_attr_line_number_table _ = failwith "todo"
  let decode_attr_local_variable_table _ = failwith "todo"
  let decode_attr_local_variable_type_table _ = failwith "todo"
  let decode_attr_deprecated _ = failwith "todo"
  let decode_attr_runtime_visible_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_annotations _ = failwith "todo"
  let decode_attr_runtime_visible_parameter_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_parameter_annotations _ = failwith "todo"
  let decode_attr_runtime_visible_type_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_type_annotations _ = failwith "todo"
  let decode_attr_annotation_default _ = failwith "todo"
  let decode_attr_stack_map_table _ = failwith "todo"
  let decode_attr_bootstrap_methods _ = failwith "todo"
  let decode_attr_module _ = failwith "todo"
  let decode_attr_module_requires _ = failwith "todo"
  let decode_attr_module_permits _ = failwith "todo"
  let decode_attr_module_provides _ = failwith "todo"

  module UTF8Hashtbl = Hashtbl.Make (Utils.UTF8)

  let decoders :
    (enclosing_elemen ->
      ConstantPool.t ->
        OA.info -> InputStream.t -> t) UTF8Hashtbl.t
  =
    let ds = [
      attr_constant_value, decode_attr_constant_value;
      attr_code, decode_attr_code;
      attr_exceptions, decode_attr_exceptions;
      attr_inner_classes, decode_attr_inner_classes;
      attr_enclosing_method, decode_attr_enclosing_method;
      attr_synthetic, decode_attr_synthetic;
      attr_signature, decode_attr_signature;
      attr_source_file, decode_attr_source_file;
      attr_source_debug_extension, decode_attr_source_debug_extension;
      attr_line_number_table, decode_attr_line_number_table;
      attr_local_variable_table, decode_attr_local_variable_table;
      attr_local_variable_type_table, decode_attr_local_variable_type_table;
      attr_deprecated, decode_attr_deprecated;
      attr_runtime_visible_annotations, decode_attr_runtime_visible_annotations;
      attr_runtime_invisible_annotations, decode_attr_runtime_invisible_annotations;
      attr_runtime_visible_parameter_annotations, decode_attr_runtime_visible_parameter_annotations;
      attr_runtime_invisible_parameter_annotations, decode_attr_runtime_invisible_parameter_annotations;
      attr_runtime_visible_type_annotations, decode_attr_runtime_visible_type_annotations;
      attr_runtime_invisible_type_annotations, decode_attr_runtime_invisible_type_annotations;
      attr_annotation_default, decode_attr_annotation_default;
      attr_stack_map_table, decode_attr_stack_map_table;
      attr_bootstrap_methods, decode_attr_bootstrap_methods;
      attr_module, decode_attr_module;
      attr_module_requires, decode_attr_module_requires;
      attr_module_permits, decode_attr_module_permits;
      attr_module_provides, decode_attr_module_provides
    ] in
    let r = UTF8Hashtbl.create 61 in
    List.iter (fun (k, v) -> UTF8Hashtbl.add r k v) ds;
    r

  (* visible functions *)

  let for_class _ = failwith "todo"

  let decode element pool i =
    let st = InputStream.make_of_string i.OA.data in
    let attr_name = get_utf8 pool i.OA.name_index Invalid_attribute_name in
    try
      for_class (UTF8Hashtbl.find decoders attr_name element pool i st)
    with Not_found ->
      `Unknown (attr_name, i.OA.data)
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
  let attribute_decode = A.decode A.Class pool in
  check_version_high ~version:version { access_flags = flags;
    name = get_class_name cf.CF.this_class;
    extends = extends;
    implements = List.map get_class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }

let encode ?(version = Version.default) _ = ignore version; failwith "todo"

