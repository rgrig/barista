(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module C = Consts
module CF = ClassFile
module U = Utils

(* }}} *)

(* errors *) (* {{{ *)
type error =
  | Invalid_attribute_name
  | Invalid_class_name
  | Invalid_code_attribute
  | Invalid_code_length
  | Invalid_constant_value
  | Invalid_descriptor
  | Invalid_exception_name
  | Invalid_method_name
  | Invalid_module

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

  let decode _ _ _ = failwith "todo"
  let size_of _ _ = failwith "todo"
end (* }}} *)

module A = Attribute
module HighAttribute = struct (* {{{ *)
  open Consts

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

  let get_class_name pool idx err =
    match ConstantPool.get_entry pool idx with
      | ConstantPool.Class idx ->
	let n = get_utf8 pool idx err in
	Name.make_for_class_from_internal n
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

  let read_info st =
    let name = InputStream.read_u2 st in
    let len = InputStream.read_u4 st in
    if (len :> int64) > (Int64.of_int max_int) then
      raise (InputStream.Exception InputStream.Data_is_too_large)
    else
      let dat = InputStream.read_bytes st (Int64.to_int (len :> int64)) in
      { A.name_index = name;
	length = len;
	data = dat; }

  let check_code_attributes l =
    let map = function
      | (`LineNumberTable _ as x)
(*      | (`LocalVariableTable _ as x)
      | (`LocalVariableTypeTable _ as x)
      | (`StackMapTable _ as x) *)
      | (`Unknown _ as x) -> x
      | #t -> fail Invalid_code_attribute in
    List.map map l
      
  let decode_attr_constant_value _ _ pool _ st =
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

  let decode_attr_code decode element pool i st =
    (* read these anyway to get into the stream *)
    let (*mx_stack*) _ = InputStream.read_u2 st in
    let (*mx_locals*) _ = InputStream.read_u2 st in
    let code_len' = InputStream.read_u4 st in
    let code_len =
      if (code_len' :> int64) < 65536L then
        Int64.to_int (code_len' :> int64)
      else
        fail Invalid_code_length in
    let code_content = InputStream.read_bytes st code_len in
    let code_stream = InputStream.make_of_string code_content in
    let instr_codes = ByteCode.read code_stream 0 in
    let fold_size (l, ofs, lbl) inst =
      let s = Instruction.size_of ofs inst in
      (inst, ofs, lbl) :: l, ofs + s, lbl + 1 in
    let instr_codes_annot, _, _ = List.fold_left fold_size ([], 0, 0) instr_codes in
    let ofs_to_label ofs =
      let (_, _, lbl) = List.find (fun (_, o, _) -> o = ofs) instr_codes_annot in
      lbl in
    let u2_ofs_to_label ofs = ofs_to_label (ofs : Utils.u2 :> int) in
    let instrs = List.map (Instruction.decode pool ofs_to_label) instr_codes in
    let exceptions =
      InputStream.read_elements
        st
        (fun st ->
          let start_pc = InputStream.read_u2 st in
          let end_pc = InputStream.read_u2 st in
          let handler_pc = InputStream.read_u2 st in
          let catch_index = InputStream.read_u2 st in
          let catch_type =
            if (catch_index :> int) <> 0 then
              Some (get_class_name pool catch_index Invalid_exception_name)
            else
              None in
          { try_start = u2_ofs_to_label start_pc;
            try_end = u2_ofs_to_label end_pc;
            catch = u2_ofs_to_label handler_pc;
            caught = catch_type; }) in
    let attrs =
      InputStream.read_elements
        st
        (fun st ->
          let a = read_info st in
          decode element pool a) in
    `Code { code = instrs;
            exception_table = exceptions;
            attributes = check_code_attributes attrs; }
      (* TODO: should code attributes be checked here? *)

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
      ((A.enclosing_element ->
	ConstantPool.t ->
	A.info -> for_class) ->
       A.enclosing_element ->
       ConstantPool.t ->
       A.info -> InputStream.t -> t) UTF8Hashtbl.t =
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

  let decode_method _ _  = failwith "todo"

  let rec decode element pool i =
    let st = InputStream.make_of_string i.A.data in
    let attr_name = get_utf8 pool i.A.name_index Invalid_attribute_name in
    try
      for_class (UTF8Hashtbl.find decoders attr_name decode element pool i st)
    with Not_found ->
      `Unknown (attr_name, i.A.data)
end (* }}} *)

module M = Method
module HighMethod = struct (* {{{ *)
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

  let decode is_interface pool m =
    let name = match ConstantPool.get_entry pool m.M.name_index with
    | ConstantPool.UTF8 n ->
        if Name.is_valid_for_method n then
          n
        else
          fail Invalid_method_name
    | _ -> fail Invalid_method_name in
    let descriptor = match ConstantPool.get_entry pool m.M.descriptor_index with
     | ConstantPool.UTF8 d -> Descriptor.method_of_utf8 d
     | _ -> fail Invalid_descriptor in
     let attributes =
         U.map_array_to_list
            (HighAttribute.decode_method Attribute.Method pool)
            m.M.attributes_array in
     U.switch
       U.UTF8.equal
       [ C.class_initializer,
         (fun _ ->
           let flags =
             AccessFlag.check_initializer_flags (AccessFlag.from_u2 true m.M.access_flags) in
           Initializer { init_flags = flags; init_attributes = attributes });

         C.class_constructor,
         (fun _ ->
           let flags =
             AccessFlag.check_constructor_flags (AccessFlag.from_u2 true m.M.access_flags) in
           Constructor { cstr_flags = flags; cstr_descriptor = fst descriptor; cstr_attributes = attributes }) ]
       (fun _ ->
         let flags =
           AccessFlag.check_method_flags is_interface
            (AccessFlag.from_u2 true m.M.access_flags) in
         let name = Name.make_for_method name in
         Regular { flags; name; descriptor; attributes })
       name
end (* }}} *)

module HA = HighAttribute
module HM = HighMethod

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : HM.t list;
    attributes : HA.for_class list;
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
  let method_decode = HM.decode is_interface pool in
  let attribute_decode = HA.decode A.Class pool in
  check_version_high ~version:version { access_flags = flags;
    name = get_class_name cf.CF.this_class;
    extends = extends;
    implements = List.map get_class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }

let encode ?(version = Version.default) _ = ignore version; failwith "todo"

