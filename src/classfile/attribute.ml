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

open Utils
open Consts


(* Low-level form *)

type info = {
    name_index : u2;
    length : u4;
    data : string;
  }


(* Exception *)

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

let fail e = raise (Exception e)

let fail_if b a x =
  if !b then
    fail (Defined_twice a)
  else begin
    b := true;
    x
  end

let string_of_error = function
  | Invalid_code_attribute -> "invalid code attribute"
  | Invalid_code_length -> "invalid code length"
  | Defined_twice id -> id ^ " attribute defined twice"
  | Invalid_field_attribute -> "invalid field attribute"
  | Invalid_method_attribute -> "invalid method attribute"
  | Invalid_class_attribute -> "invalid class attribute"
  | Invalid_package_attribute -> "invalid package attribute"
  | Invalid_module_attribute -> "invalid module attribute"
  | Invalid_constant_value -> "invalid constant value"
  | Invalid_enclosing_method -> "invalid enclosing method"
  | Invalid_local_variable_table -> "invalid local variable table"
  | Invalid_local_variable_type_table -> "invalid local variable type table"
  | Invalid_list_length -> "invalid list length"
  | Invalid_attribute_name_value -> "invalid attribute name value"
  | Invalid_exception_name -> "invalid exception name"
  | Invalid_exception -> "invalid exception"
  | Invalid_inner_class -> "invalid inner class"
  | Invalid_outer_class -> "invalid outer class"
  | Invalid_signature -> "invalid signature"
  | Invalid_source_file -> "invalid source file"
  | Invalid_stack_map_frame -> "invalid stack map frame"
  | Invalid_stack_map_verification_type -> "invalid stack map verification type"
  | Invalid_bootstrap_method_handle -> "invalid bootstrap method handle"
  | Invalid_bootstrap_argument -> "invalid bootstrap argument"
  | Invalid_module -> "invalid module attribute"
  | Invalid_module_dependency_kind -> "invalid module dependency kind"
  | Missing_module_attribute -> "missing module attribute"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* I/O functions *)

let read_info st =
  let name = InputStream.read_u2 st in
  let len = InputStream.read_u4 st in
  if (len :> int64) > (Int64.of_int max_int) then
    raise (InputStream.Exception InputStream.Data_is_too_large)
  else
    let dat = InputStream.read_bytes st (Int64.to_int (len :> int64)) in
    { name_index = name;
      length = len;
      data = dat; }

let write_info st i =
  OutputStream.write_u2 st i.name_index;
  OutputStream.write_u4 st i.length;
  OutputStream.write_bytes st i.data


(* High-level form *)

type constant_value =
  | Long_value of int64
  | Float_value of float
  | Double_value of float
  | Boolean_value of bool
  | Byte_value of int
  | Character_value of int
  | Short_value of int
  | Integer_value of int32
  | String_value of UTF8.t

type verification_type_info =
  | Top_variable_info
  | Integer_variable_info
  | Float_variable_info
  | Long_variable_info
  | Double_variable_info
  | Null_variable_info
  | Uninitialized_this_variable_info
  | Object_variable_info of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | Uninitialized_variable_info of u2

type stack_map_frame =
  | Same_frame of u2
  | Same_locals_1_stack_item_frame of u2 * verification_type_info
  | Chop_1_frame of u2
  | Chop_2_frame of u2
  | Chop_3_frame of u2
  | Append_1_frame of u2 * verification_type_info
  | Append_2_frame of u2 * verification_type_info * verification_type_info
  | Append_3_frame of u2 * verification_type_info * verification_type_info * verification_type_info
  | Full_frame of u2 * (verification_type_info list) * (verification_type_info list)

type dependency_kind =
  | Optional_dependency
  | Same_class_loader
  | Not_observable

type t =
  [ `ConstantValue of constant_value
  | `Code of code_value
  | `Exceptions of Name.for_class list
  | `InnerClasses of inner_class_element list
  | `EnclosingMethod of enclosing_method_value
  | `Synthetic
  | `Signature of signature
  | `SourceFile of UTF8.t
  | `SourceDebugExtension of UTF8.t
  | `LineNumberTable of (u2 * u2) list
  | `LocalVariableTable of local_variable_table_element list
  | `LocalVariableTypeTable of local_variable_type_table_element list
  | `Deprecated
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeVisibleParameterAnnotations of Annotation.t list list
  | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `AnnotationDefault of Annotation.element_value
  | `StackMapTable of stack_map_frame list
  | `BootstrapMethods of Bootstrap.method_specifier list
  | `Module of UTF8.t * UTF8.t
  | `ModuleRequires of (UTF8.t * UTF8.t * dependency_kind) list
  | `ModulePermits of (UTF8.t * UTF8.t) list
  | `ModuleProvides of (UTF8.t * UTF8.t) list
  | `Unknown of UTF8.t * string ]
and signature =
  [ `Class of Signature.class_signature
  | `Method of Signature.method_signature
  | `Field of Signature.field_type_signature ]
and code_attribute = [
  | `LineNumberTable of (u2 * u2) list
  | `LocalVariableTable of local_variable_table_element list
  | `LocalVariableTypeTable of local_variable_type_table_element list
  | `StackMapTable of stack_map_frame list
  | `Unknown of UTF8.t * string ]
and code_value = {
    max_stack : u2;
    max_locals : u2;
    code : Instruction.t list;
    exception_table : exception_table_element list;
    attributes : code_attribute list;
  }
and exception_table_element = {
    try_start : u2;
    try_end : u2;
    catch : u2;
    caught : Name.for_class option;
  }
and inner_class_element = {
    inner_class : Name.for_class option;
    outer_class : Name.for_class option;
    inner_name : UTF8.t option;
    inner_flags : AccessFlag.for_inner_class list;
  }
and enclosing_method_value = {
    innermost_class : Name.for_class;
    enclosing_method : (Name.for_method * Descriptor.for_method) option;
  }
and local_variable_table_element = {
    local_start : u2;
    local_length : u2;
    local_name : UTF8.t;
    local_descriptor : Descriptor.for_field;
    local_index : u2;
  }
and local_variable_type_table_element = {
    local_type_start : u2;
    local_type_length : u2;
    local_type_name : UTF8.t;
    local_type_signature : Signature.field_type_signature;
    local_type_index : u2;
  }

type for_field =
  [ `ConstantValue of constant_value
  | `Synthetic
  | `Signature of [`Field of Signature.field_type_signature]
  | `Deprecated
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `Unknown of UTF8.t * string ]

type for_method =
  [ `Code of code_value
  | `Exceptions of Name.for_class list
  | `Synthetic
  | `Signature of [`Method of Signature.method_signature]
  | `Deprecated
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeVisibleParameterAnnotations of Annotation.t list list
  | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `AnnotationDefault of Annotation.element_value
  | `Unknown of UTF8.t * string ]

type for_class =
  [ `InnerClasses of inner_class_element list
  | `EnclosingMethod of enclosing_method_value
  | `Synthetic
  | `Signature of [`Class of Signature.class_signature]
  | `SourceFile of UTF8.t
  | `SourceDebugExtension of UTF8.t
  | `Deprecated
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `BootstrapMethods of Bootstrap.method_specifier list
  | `Module of UTF8.t * UTF8.t
  | `Unknown of UTF8.t * string ]

type for_package =
  [ `Module of UTF8.t * UTF8.t
  | `SourceFile of UTF8.t
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `Unknown of UTF8.t * string ]

type for_module =
  [ `Module of UTF8.t * UTF8.t
  | `SourceFile of UTF8.t
  | `ModuleRequires of (UTF8.t * UTF8.t * dependency_kind) list
  | `ModulePermits of (UTF8.t * UTF8.t) list
  | `ModuleProvides of (UTF8.t * UTF8.t) list
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `Unknown of UTF8.t * string ]

type enclosing_element =
  | Class
  | Method
  | Field
  | Package
  | Module

(* Conversion functions *)

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

let get_name_and_type pool idx err =
  match ConstantPool.get_entry pool idx with
  | ConstantPool.NameAndType (idx1, idx2) ->
      (get_utf8 pool idx1 err), (get_utf8 pool idx2 err)
  | _ -> fail err

let string_of_verification_type_info = function
  | Top_variable_info -> "top"
  | Integer_variable_info -> "int"
  | Float_variable_info -> "float"
  | Long_variable_info -> "long"
  | Double_variable_info -> "double"
  | Null_variable_info -> "null"
  | Uninitialized_this_variable_info -> "uninit this"
  | Object_variable_info (`Class_or_interface cn) ->
      UTF8.to_string_noerr (Name.external_utf8_for_class cn)
  | Object_variable_info (`Array_type ((`Array _) as a)) ->
      let res = Descriptor.external_utf8_of_java_type (a :> Descriptor.java_type) in
      (UTF8.to_string_noerr res)
  | Uninitialized_variable_info ofs ->
      Printf.sprintf "uninit %d" (ofs :> int)

let verification_type_info_of_parameter_descriptor = function
  | `Boolean -> Integer_variable_info
  | `Byte -> Integer_variable_info
  | `Char -> Integer_variable_info
  | `Double -> Double_variable_info
  | `Float -> Float_variable_info
  | `Int -> Integer_variable_info
  | `Long -> Long_variable_info
  | `Short -> Integer_variable_info
  | `Class cn -> Object_variable_info (`Class_or_interface cn)
  | `Array e -> Object_variable_info (`Array_type (`Array e))

let equal_verification_type_info x y =
  match (x, y) with
  | (Object_variable_info (`Class_or_interface cn1)),
    (Object_variable_info (`Class_or_interface cn2)) ->
      Name.equal_for_class cn1 cn2
  | (Object_variable_info (`Array_type at1)),
    (Object_variable_info (`Array_type at2)) ->
      Descriptor.equal_java_type
        (at1 :> Descriptor.java_type)
        (at2 :> Descriptor.java_type)
  | (Object_variable_info _),
    (Object_variable_info _) -> false
  | (Uninitialized_variable_info uvi1),
    (Uninitialized_variable_info uvi2) -> uvi1 = uvi2
  | _ -> x = y

let read_verification_type_info st pool =
  let tag = InputStream.read_u1 st in
  match (tag :> int) with
  | 0 -> Top_variable_info
  | 1 -> Integer_variable_info
  | 2 -> Float_variable_info
  | 4 -> Long_variable_info
  | 3 -> Double_variable_info
  | 5 -> Null_variable_info
  | 6 -> Uninitialized_this_variable_info
  | 7 ->
      let idx = InputStream.read_u2 st in
      let d = match ConstantPool.get_entry pool idx with
      | ConstantPool.Class idx' ->
          (match ConstantPool.get_entry pool idx' with
          | ConstantPool.UTF8 v ->
              if UChar.equal opening_square_bracket (UTF8.get v 0) then
                let t = Descriptor.java_type_of_internal_utf8 v in `Array_type (Descriptor.filter_non_array Descriptor.Invalid_array_element_type t)
              else
                `Class_or_interface (Name.make_for_class_from_internal v)
          | _ -> fail Invalid_stack_map_verification_type)
      | _ -> fail Invalid_stack_map_verification_type in
      Object_variable_info d
  | 8 -> Uninitialized_variable_info (InputStream.read_u2 st)
  | _ -> fail Invalid_stack_map_verification_type

let write_verification_type_info st pool = function
  | Top_variable_info ->
      OutputStream.write_u1 st (u1 0)
  | Integer_variable_info ->
      OutputStream.write_u1 st (u1 1)
  | Float_variable_info ->
      OutputStream.write_u1 st (u1 2)
  | Long_variable_info ->
      OutputStream.write_u1 st (u1 4)
  | Double_variable_info ->
      OutputStream.write_u1 st (u1 3)
  | Null_variable_info ->
      OutputStream.write_u1 st (u1 5)
  | Uninitialized_this_variable_info ->
      OutputStream.write_u1 st (u1 6)
  | Object_variable_info d ->
      OutputStream.write_u1 st (u1 7);
      let idx = match d with
      | `Class_or_interface u -> ConstantPool.add_class pool u
      | `Array_type t -> ConstantPool.add_array_class pool t in
      OutputStream.write_u2 st idx
  | Uninitialized_variable_info ofs ->
      OutputStream.write_u1 st (u1 8);
      OutputStream.write_u2 st ofs

let read_stack_map_frame st pool prev_ofs =
  let map_ofs o = u2 (prev_ofs + (o : u2 :> int) + 1) in
  let tag = InputStream.read_u1 st in
  match (tag :> int) with
  | 247 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      let vti = read_verification_type_info st pool in
      (Same_locals_1_stack_item_frame (ofs, vti)), ofs
  | 248 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      (Chop_3_frame ofs), ofs
  | 249 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      (Chop_2_frame ofs), ofs
  | 250 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      (Chop_1_frame ofs), ofs
  | 251 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      (Same_frame ofs), ofs
  | 252 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      let t1 = read_verification_type_info st pool in
      (Append_1_frame (ofs, t1)), ofs
  | 253 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      let t1 = read_verification_type_info st pool in
      let t2 = read_verification_type_info st pool in
      (Append_2_frame (ofs, t1, t2)), ofs
  | 254 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      let t1 = read_verification_type_info st pool in
      let t2 = read_verification_type_info st pool in
      let t3 = read_verification_type_info st pool in
      (Append_3_frame (ofs, t1, t2, t3), ofs)
  | 255 ->
      let ofs = map_ofs (InputStream.read_u2 st) in
      let locals = InputStream.read_elements st (fun x -> read_verification_type_info x pool) in
      let stack_items = InputStream.read_elements st (fun x -> read_verification_type_info x pool) in
      (Full_frame (ofs, locals, stack_items)), ofs
  | _ when ((tag :> int) >= 0) && ((tag :> int) <= 63) ->
      let ofs = map_ofs (u2_of_u1 tag) in
      (Same_frame ofs), ofs
  | _ when ((tag :> int) >= 64) && ((tag :> int) <= 127) ->
      let ofs = map_ofs (u2 ((tag :> int) - 64)) in
      let vti = read_verification_type_info st pool in
      (Same_locals_1_stack_item_frame (ofs, vti)), ofs
  | _ ->
      fail Invalid_stack_map_frame

let write_stack_map_frame st pool prev_ofs x =
  let map_ofs o = u2 ((o : u2 :> int) - prev_ofs - 1) in
  match x with
  | Same_frame o ->
      let ofs = map_ofs o in
      if ((ofs :> int) >= 0) && ((ofs :> int) <= 63) then
        (OutputStream.write_u1 st (u1 (ofs :> int));
         o)
      else
        (OutputStream.write_u1 st (u1 251);
         OutputStream.write_u2 st ofs;
         o)
  | Same_locals_1_stack_item_frame (o, t) ->
      let ofs = map_ofs o in
      if ((ofs :> int) >= 0) && ((ofs :> int) <= 63) then
        (OutputStream.write_u1 st (u1 (64 + (ofs :> int)));
         write_verification_type_info st pool t;
         o)
      else
        (OutputStream.write_u1 st (u1 247);
         OutputStream.write_u2 st ofs;
         write_verification_type_info st pool t;
         o)
  | Chop_1_frame o ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 250);
      OutputStream.write_u2 st ofs;
      o
  | Chop_2_frame o ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 249);
      OutputStream.write_u2 st ofs;
      o
  | Chop_3_frame o ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 248);
      OutputStream.write_u2 st ofs;
      o
  | Append_1_frame (o, t1) ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 252);
      OutputStream.write_u2 st ofs;
      write_verification_type_info st pool t1;
      o
  | Append_2_frame (o, t1, t2) ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 253);
      OutputStream.write_u2 st ofs;
      write_verification_type_info st pool t1;
      write_verification_type_info st pool t2;
      o
  | Append_3_frame (o, t1, t2, t3) ->
      let ofs = map_ofs o in
      OutputStream.write_u1 st (u1 254);
      OutputStream.write_u2 st ofs;
      write_verification_type_info st pool t1;
      write_verification_type_info st pool t2;
      write_verification_type_info st pool t3;
      o
  | Full_frame (o, locals, stack_items) ->
      let ofs = map_ofs o in
      let len_locals = List.length locals in
      let len_stack_items = List.length stack_items in
      if (len_locals <= max_u2) && (len_stack_items <= max_u2) then
        (OutputStream.write_u1 st (u1 255);
         OutputStream.write_u2 st ofs;
         OutputStream.write_u2 st (u2 len_locals);
         List.iter (write_verification_type_info st pool) locals;
         OutputStream.write_u2 st (u2 len_stack_items);
         List.iter (write_verification_type_info st pool) stack_items;
         o)
      else
        fail Invalid_stack_map_frame

let u1_of_dependency_kind = function
  | Optional_dependency -> u1 0x01
  | Same_class_loader -> u1 0x02
  | Not_observable -> u1 0x03

let dependency_kind_of_u1 x =
  match (x : u1 :> int) with
  | 0x01 -> Optional_dependency
  | 0x02 -> Same_class_loader
  | 0x03 -> Not_observable
  | _ -> fail Invalid_module_dependency_kind

let check_code_attributes l =
  let map = function
    | (`LineNumberTable _) as x -> x
    | (`LocalVariableTable _) as x -> x
    | (`LocalVariableTypeTable _) as x -> x
    | (`Unknown _) as x -> x
    | (`StackMapTable _) as x -> x
    | #t -> fail Invalid_code_attribute in
  List.map map l

let check_field_attributes l =
  let const = ref false in
  let synth = ref false in
  let sign = ref false in
  let depr = ref false in
  let v_ann = ref false in
  let i_ann = ref false in
  let v_t_ann = ref false in
  let i_t_ann = ref false in
  let map = function
    | (`ConstantValue _) as x -> fail_if const "ConstantValue" x
    | `Synthetic -> fail_if synth "Synthetic" `Synthetic
    | (`Signature (`Field _)) as x -> fail_if sign "Signature" x
    | `Deprecated -> fail_if depr "Deprecated" `Deprecated
    | (`RuntimeVisibleAnnotations _) as x -> fail_if v_ann "RuntimeVisibleAnnotations" x
    | (`RuntimeInvisibleAnnotations _) as x -> fail_if i_ann "RuntimeInvisibleAnnotations" x
    | (`RuntimeVisibleTypeAnnotations _) as x -> fail_if v_t_ann "RuntimeVisibleTypeAnnotations" x
    | (`RuntimeInvisibleTypeAnnotations _) as x -> fail_if i_t_ann "RuntimeInvisibleTypeAnnotations" x
    | (`Unknown _) as x -> x
    | #t -> fail Invalid_field_attribute in
  List.map map l

let check_method_attributes l =
  let code = ref false in
  let exn = ref false in
  let synth = ref false in
  let sign = ref false in
  let depr = ref false in
  let v_ann = ref false in
  let i_ann = ref false in
  let v_p_ann = ref false in
  let i_p_ann = ref false in
  let v_t_ann = ref false in
  let i_t_ann = ref false in
  let ann_d = ref false in
  let map = function
    | (`Code _) as x -> fail_if code "Code" x
    | (`Exceptions _) as x -> fail_if exn "Exceptions" x
    | `Synthetic -> fail_if synth "Synthetic" `Synthetic
    | (`Signature (`Method _)) as x -> fail_if sign "Signature" x
    | `Deprecated -> fail_if depr "Deprecated" `Deprecated
    | (`RuntimeVisibleAnnotations _) as x -> fail_if v_ann "RuntimeVisibleAnnotations" x
    | (`RuntimeInvisibleAnnotations _) as x -> fail_if i_ann "RuntimeInvisibleAnnotations" x
    | (`RuntimeVisibleParameterAnnotations _) as x -> fail_if v_p_ann "RuntimeVisibleParameterAnnotations" x
    | (`RuntimeInvisibleParameterAnnotations _) as x -> fail_if i_p_ann "RuntimeInvisibleParameterAnnotations" x
    | (`RuntimeVisibleTypeAnnotations _) as x -> fail_if v_t_ann "RuntimeVisibleTypeAnnotations" x
    | (`RuntimeInvisibleTypeAnnotations _) as x -> fail_if i_t_ann "RuntimeInvisibleTypeAnnotations" x
    | (`AnnotationDefault _) as x -> fail_if ann_d "AnnotationDefault" x
    | (`Unknown _) as x -> x
    | #t -> fail Invalid_method_attribute in
  List.map map l

let check_class_attributes l =
  let inner = ref false in
  let encl = ref false in
  let synth = ref false in
  let file = ref false in
  let debug = ref false in
  let sign = ref false in
  let depr = ref false in
  let v_ann = ref false in
  let i_ann = ref false in
  let v_t_ann = ref false in
  let i_t_ann = ref false in
  let bsm = ref false in
  let mdl = ref false in
  let map = function
    | (`InnerClasses _) as x -> fail_if inner "InnerClasses" x
    | (`EnclosingMethod _) as x -> fail_if encl "EnclosingMethod" x
    | `Synthetic -> fail_if synth "Synthetic" `Synthetic
    | (`SourceFile _) as x -> fail_if file "SourceFile" x
    | (`SourceDebugExtension _) as x -> fail_if debug "SourceDebugExtension" x
    | (`Signature (`Class _)) as x -> fail_if sign "Signature" x
    | `Deprecated -> fail_if depr "Deprecated" `Deprecated
    | (`RuntimeVisibleAnnotations _) as x -> fail_if v_ann "RuntimeVisibleAnnotations" x
    | (`RuntimeInvisibleAnnotations _) as x -> fail_if i_ann "RuntimeInvisibleAnnotations" x
    | (`RuntimeVisibleTypeAnnotations _) as x -> fail_if v_t_ann "RuntimeVisibleTypeAnnotations" x
    | (`RuntimeInvisibleTypeAnnotations _) as x -> fail_if i_t_ann "RuntimeInvisibleTypeAnnotations" x
    | (`BootstrapMethods _) as x -> fail_if bsm "BootstrapMethods" x
    | (`Module _) as x -> fail_if mdl "Module" x
    | (`Unknown _) as x -> x
    | #t -> fail Invalid_class_attribute in
  List.map map l

let check_module_attributes l =
  let file = ref false in
  let v_ann = ref false in
  let i_ann = ref false in
  let mdl = ref false in
  let mdl_req = ref false in
  let mdl_prm = ref false in
  let mdl_prv = ref false in
  let map = function
    | (`SourceFile _) as x -> fail_if file "SourceFile" x
    | (`RuntimeVisibleAnnotations _) as x -> fail_if v_ann "RuntimeVisibleAnnotations" x
    | (`RuntimeInvisibleAnnotations _) as x -> fail_if i_ann "RuntimeInvisibleAnnotations" x
    | (`Module _) as x -> fail_if mdl "Module" x
    | (`ModuleRequires _) as x -> fail_if mdl_req "ModuleRequires" x
    | (`ModulePermits _) as x -> fail_if mdl_prm "ModulePermits" x
    | (`ModuleProvides _) as x -> fail_if mdl_prv "ModuleProvides" x
    | (`Unknown _) as x -> x
    | #t -> fail Invalid_module_attribute in
  if !mdl then
    List.map map l
  else
    fail Missing_module_attribute

let check_package_attributes l =
  let file = ref false in
  let v_ann = ref false in
  let i_ann = ref false in
  let mdl = ref false in
  let map = function
    | (`SourceFile _) as x -> fail_if file "SourceFile" x
    | (`RuntimeVisibleAnnotations _) as x -> fail_if v_ann "RuntimeVisibleAnnotations" x
    | (`RuntimeInvisibleAnnotations _) as x -> fail_if i_ann "RuntimeInvisibleAnnotations" x
    | (`Module _) as x -> fail_if mdl "Module" x
    | (`Unknown _) as x -> x
    | #t -> fail Invalid_package_attribute in
  List.map map l

let rec decode element bsm pool i =
  let st = InputStream.make_of_string i.data in
  let read_annotations () =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_info st in
        Annotation.decode pool a) in
  let read_extended_annotations () =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_extended_info st in
        Annotation.decode_extended pool a) in
  let read_annotations_list () =
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
    List.rev !res in
  let read_module_info () =
    let module_index = InputStream.read_u2 st in
    let name_index, version_index =
      match ConstantPool.get_entry pool module_index with
      | ConstantPool.ModuleId (n, v) -> n, v
      | _ -> fail Invalid_module in
    let name = get_utf8 pool name_index Invalid_module in
    let version = get_utf8 pool version_index Invalid_module in
    name, version in
  let attr_name = get_utf8 pool i.name_index Invalid_attribute_name_value in
  switch UTF8.equal
    [ attr_constant_value,
      (fun _ ->
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
        | _ -> fail Invalid_constant_value);
      attr_code,
      (fun _ ->
        let mx_stack = InputStream.read_u2 st in
        let mx_locals = InputStream.read_u2 st in
        let code_len' = InputStream.read_u4 st in
        let code_len =
          if (code_len' :> int64) < 65536L then
            Int64.to_int (code_len' :> int64)
          else
            fail Invalid_code_length in
        let code_content = InputStream.read_bytes st code_len in
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
              { try_start = start_pc;
                try_end = end_pc;
                catch = handler_pc;
                caught = catch_type; }) in
        let attrs =
          InputStream.read_elements
            st
            (fun st ->
              let a = read_info st in
              decode element bsm pool a) in
        let code_stream = InputStream.make_of_string code_content in
        let instrs =
          List.map
            (fun i -> Instruction.decode bsm pool i)
            (ByteCode.read code_stream 0) in
        `Code { max_stack = mx_stack;
                max_locals = mx_locals;
                code = instrs;
                exception_table = exceptions;
                attributes = check_code_attributes attrs; });
      attr_exceptions,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let idx = InputStream.read_u2 st in
              get_class_name pool idx Invalid_exception) in
        `Exceptions res);
      attr_inner_classes,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let inner_info_index = InputStream.read_u2 st in
              let outer_info_index = InputStream.read_u2 st in
              let inner_name_index = InputStream.read_u2 st in
              let inner_class_access_flag = InputStream.read_u2 st in
              let inner_class =
                if (inner_info_index :> int) = 0 then
                  None
                else
                  Some (get_class_name pool inner_info_index Invalid_inner_class) in
              let outer_class =
                if (outer_info_index :> int) = 0 then
                  None
                else
                  Some (get_class_name pool outer_info_index Invalid_outer_class) in
              let inner_name =
                if (inner_name_index :> int) = 0 then
                  None
                else
                  Some (get_utf8 pool inner_name_index Invalid_inner_class) in
              let inner_flags = AccessFlag.from_u2 false inner_class_access_flag in
              let inner_flags = AccessFlag.check_inner_class_flags inner_flags in
              { inner_class; outer_class; inner_name; inner_flags }) in
        `InnerClasses res);
      attr_enclosing_method,
      (fun _ ->
        let class_index = InputStream.read_u2 st in
        let method_index = InputStream.read_u2 st in
        let class_name = get_class_name pool class_index Invalid_enclosing_method in
        let method_desc = if (method_index :> int) <> 0 then
          try
            match ConstantPool.get_entry pool method_index with
            | ConstantPool.NameAndType (name, desc) ->
                Some ((Name.make_for_method (get_utf8 pool name Invalid_enclosing_method)),
                      (Descriptor.method_of_utf8 (get_utf8 pool desc Invalid_enclosing_method)))
            | _ -> fail Invalid_enclosing_method
          with _ -> fail Invalid_enclosing_method
        else
          None in
        `EnclosingMethod { innermost_class = class_name;
                           enclosing_method = method_desc });
      attr_synthetic,
      (fun _ ->
        `Synthetic);
      attr_signature,
      (fun _ ->
        let signature_index = InputStream.read_u2 st in
        let s = get_utf8 pool signature_index Invalid_signature in
        let s' = (match element with
        | Class -> `Class (Signature.class_signature_of_utf8 s)
        | Method -> `Method (Signature.method_signature_of_utf8 s)
        | Field -> `Field (Signature.field_type_signature_of_utf8 s)
        | Package -> fail Invalid_package_attribute
        | Module -> fail Invalid_module_attribute) in
        `Signature s');
      attr_source_file,
      (fun _ ->
        let sourcefile_index = InputStream.read_u2 st in
        `SourceFile (get_utf8 pool sourcefile_index Invalid_source_file));
      attr_source_debug_extension,
      (fun _ ->
        let extension = UTF8.of_modified (UTF8.modified_of_bytes i.data) in
        `SourceDebugExtension extension);
      attr_line_number_table,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let start_pc = InputStream.read_u2 st in
              let line_number = InputStream.read_u2 st in
              (start_pc, line_number)) in
        `LineNumberTable res);
      attr_local_variable_table,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let start_pc = InputStream.read_u2 st in
              let length = InputStream.read_u2 st in
              let name_index = InputStream.read_u2 st in
              let desc_index = InputStream.read_u2 st in
              let index = InputStream.read_u2 st in
              let name = get_utf8 pool name_index Invalid_local_variable_table in
              let desc = get_utf8 pool desc_index Invalid_local_variable_table in
              let field_desc = Descriptor.field_of_utf8 desc in
              if Name.is_valid_unqualified name then
                { local_start = start_pc;
                  local_length = length;
                  local_name = name;
                  local_descriptor = field_desc;
                  local_index = index }
              else
                fail Invalid_local_variable_table) in
        `LocalVariableTable res);
      attr_local_variable_type_table,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let start_pc = InputStream.read_u2 st in
              let length = InputStream.read_u2 st in
              let name_index = InputStream.read_u2 st in
              let sign_index = InputStream.read_u2 st in
              let index = InputStream.read_u2 st in
              let name = get_utf8 pool name_index Invalid_local_variable_type_table in
              let s = get_utf8 pool sign_index Invalid_local_variable_type_table in
              let sign = Signature.field_type_signature_of_utf8 s in
              if Name.is_valid_unqualified name then
                { local_type_start = start_pc;
                  local_type_length = length;
                  local_type_name = name;
                  local_type_signature = sign;
                  local_type_index = index }
              else
                fail Invalid_local_variable_type_table) in
        `LocalVariableTypeTable res);
      attr_deprecated,
      (fun _ ->
        `Deprecated);
      attr_runtime_visible_annotations,
      (fun _ ->
        `RuntimeVisibleAnnotations (read_annotations ()));
      attr_runtime_invisible_annotations,
      (fun _ ->
        `RuntimeInvisibleAnnotations (read_annotations ()));
      attr_runtime_visible_parameter_annotations,
      (fun _ ->
        `RuntimeVisibleParameterAnnotations (read_annotations_list ()));
      attr_runtime_invisible_parameter_annotations,
      (fun _ ->
        `RuntimeInvisibleParameterAnnotations (read_annotations_list ()));
      attr_runtime_visible_type_annotations,
      (fun _ ->
        `RuntimeVisibleTypeAnnotations (read_extended_annotations ()));
      attr_runtime_invisible_type_annotations,
      (fun _ ->
        `RuntimeInvisibleTypeAnnotations (read_extended_annotations ()));
      attr_annotation_default,
      (fun _ ->
        let eiv = Annotation.read_info_element_value st in
        `AnnotationDefault (Annotation.decode_element_value pool eiv));
      attr_stack_map_table,
      (fun _ ->
        let nb = InputStream.read_u2 st in
        let res = ref [] in
        let ofs = ref (-1) in
        for i = 1 to (nb :> int) do
          let e, o = read_stack_map_frame st pool !ofs in
          res := e :: !res;
          ofs := (o :> int)
        done;
        `StackMapTable (List.rev !res));
      attr_bootstrap_methods,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let err = Invalid_bootstrap_method_handle in
              let get_field index =
                match ConstantPool.get_entry pool index with
                | ConstantPool.Fieldref (fc, fd) ->
                    let cn = get_class_name pool fc err in
                    let fn, ft = get_name_and_type pool fd err in
                    cn, Name.make_for_field fn, Descriptor.field_of_utf8 ft
                | _ -> fail err in
              let get_method index =
                match ConstantPool.get_entry pool index with
                | ConstantPool.Methodref (mc, md) ->
                    let cn = get_class_name pool mc err in
                    let mn, mt = get_name_and_type pool md err in
                    cn, Name.make_for_method mn, Descriptor.method_of_utf8 mt
                | _ -> fail err in
              let get_constructor index =
                match ConstantPool.get_entry pool index with
                | ConstantPool.Methodref (mc, md) ->
                    let cn = get_class_name pool mc err in
                    let mn, mt = get_name_and_type pool md err in
                    if UTF8.equal mn class_constructor then
                      cn, fst (Descriptor.method_of_utf8 mt)
                    else
                      fail err
                | _ -> fail err in
              let method_handle kind index =
                match kind with
                | ConstantPool.REF_getField ->
                    `getField (get_field index)
                | ConstantPool.REF_getStatic ->
                    `getStatic (get_field index)
                | ConstantPool.REF_putField ->
                    `putField (get_field index)
                | ConstantPool.REF_putStatic ->
                    `putStatic (get_field index)
                | ConstantPool.REF_invokeVirtual ->
                    `invokeVirtual (get_method index)
                | ConstantPool.REF_invokeStatic ->
                    `invokeStatic (get_method index)
                | ConstantPool.REF_invokeSpecial ->
                    `invokeSpecial (get_method index)
                | ConstantPool.REF_newInvokeSpecial ->
                    `newInvokeSpecial (get_constructor index)
                | ConstantPool.REF_invokeInterface ->
                    `invokeInterface (get_method index) in
              let method_index = InputStream.read_u2 st in
              let method_ref =
                match ConstantPool.get_entry pool method_index with
                | ConstantPool.MethodHandle (kind, index) ->
                    method_handle kind index
                | _ -> fail Invalid_bootstrap_method_handle in
              let args =
                InputStream.read_elements
                  st
                  (fun st ->
                    let arg_index = InputStream.read_u2 st in
                    match ConstantPool.get_entry pool arg_index with
                    | ConstantPool.String idx ->
                        (match ConstantPool.get_entry pool idx with
                        | ConstantPool.UTF8 u -> `String u
                        | _ -> fail Invalid_bootstrap_method_handle)
                    | ConstantPool.Class idx ->
                        (match ConstantPool.get_entry pool idx with
                        | ConstantPool.UTF8 u ->
                            `Class (Name.make_for_class_from_internal u)
                        | _ -> fail Invalid_bootstrap_method_handle)
                    | ConstantPool.Integer i -> `Integer i
                    | ConstantPool.Long (hi, lo) ->
                        let l = Int64.logor
                            (Int64.shift_left (Int64.of_int32 hi) 32)
                            (Int64.of_int32 lo) in
                        `Long l
                    | ConstantPool.Float f ->
                        `Float (Int32.float_of_bits f)
                    | ConstantPool.Double (hi, lo) ->
                        let d = Int64.logor
                            (Int64.shift_left (Int64.of_int32 hi) 32)
                            (Int64.of_int32 lo) in
                        `Double (Int64.float_of_bits d)
                    | ConstantPool.MethodHandle (kind, idx) ->
                        `MethodHandle (method_handle kind idx)
                    | ConstantPool.MethodType idx ->
                        (match ConstantPool.get_entry pool idx with
                        | ConstantPool.UTF8 u ->
                            `MethodType (Descriptor.method_of_utf8 u)
                        | _ -> fail Invalid_bootstrap_method_handle)
                    | _ -> fail Invalid_bootstrap_argument) in
              method_ref, args) in
        `BootstrapMethods res);
      attr_module,
      (fun _ ->
        let name, version = read_module_info () in
        `Module (name, version));
      attr_module_requires,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun st ->
              let n, v = read_module_info () in
              let k = dependency_kind_of_u1 (InputStream.read_u1 st) in
              (n, v, k)) in
        `ModuleRequires res);
      attr_module_permits,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun _ ->
              let n, v = read_module_info () in
              (n, v)) in
        `ModulePermits res);
      attr_module_provides,
      (fun _ ->
        let res =
          InputStream.read_elements
            st
            (fun _ ->
              let n, v = read_module_info () in
              (n, v)) in
        `ModuleProvides res) ]
    (fun attr_name -> `Unknown (attr_name, i.data))
    attr_name

let rec encode bsm pool a =
  let checked_length l =
    let res = List.length l in
    if res <= max_u2 then
      u2 res
    else
      fail Invalid_list_length in
  let checked_length_u1 l =
    let res = List.length l in
    if res <= max_u1 then
      u1 res
    else
      fail Invalid_list_length in
  let buffer = Buffer.create 64 in
  let st = OutputStream.make_of_buffer buffer in
  let write_annotations l =
    OutputStream.write_elements
      checked_length
      st
      (fun st a ->
        let a' = Annotation.encode pool a in
        Annotation.write_info st a')
      l in
  let write_extended_annotations l =
    OutputStream.write_elements
      checked_length
      st
      (fun st a ->
        let a' = Annotation.encode_extended pool a in
        Annotation.write_extended_info st a')
      l in
  let write_annotations_list l =
    let len = checked_length_u1 l in
    OutputStream.write_u1 st len;
    List.iter
      (fun l' ->
        OutputStream.write_elements
          checked_length
          st
          (fun st a ->
            let a' = Annotation.encode pool a in
            Annotation.write_info st a')
          l')
      l in
  let return n =
    let name_idx = ConstantPool.add_utf8 pool n in
    let content = Buffer.contents buffer in
    { name_index = name_idx;
      length = u4 (Int64.of_int (String.length content));
      data = content; } in
  match a with
  | `ConstantValue v ->
      (match v with
      | Long_value l ->
          let idx = ConstantPool.add_long pool l in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | Float_value f ->
          let idx = ConstantPool.add_float pool f in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | Double_value d ->
          let idx = ConstantPool.add_double pool d in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | Boolean_value b ->
          let idx = ConstantPool.add_integer pool (if b then 1l else 0l) in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | Byte_value v | Character_value v | Short_value v ->
          let idx = ConstantPool.add_integer pool (Int32.of_int v) in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | Integer_value i ->
          let idx = ConstantPool.add_integer pool i in
          OutputStream.write_u2 st idx;
          return attr_constant_value
      | String_value s ->
          let idx = ConstantPool.add_string pool s in
          OutputStream.write_u2 st idx;
          return attr_constant_value)
  | `Code c ->
      let code_content =
        List.map
          (Instruction.encode bsm pool)
          c.code in
      let code_buffer = Buffer.create 16 in
      let code_stream = OutputStream.make_of_buffer code_buffer in
      ByteCode.write code_stream 0 code_content;
      OutputStream.close code_stream;
      let actual_code = Buffer.contents code_buffer in
      OutputStream.write_u2 st c.max_stack;
      OutputStream.write_u2 st c.max_locals;
      let code_length = String.length actual_code in
      if code_length > max_u2 then fail Invalid_code_length;
      OutputStream.write_u4 st (u4 (Int64.of_int code_length));
      OutputStream.write_bytes st actual_code;
      OutputStream.write_elements
        checked_length
        st
        (fun st elem ->
          let catch_idx = match elem.caught with
          | Some exn_name -> ConstantPool.add_class pool exn_name
          | None -> u2 0 in
          OutputStream.write_u2 st elem.try_start;
          OutputStream.write_u2 st elem.try_end;
          OutputStream.write_u2 st elem.catch;
          OutputStream.write_u2 st catch_idx)
        c.exception_table;
      let len' = checked_length c.attributes in
      OutputStream.write_u2 st len';
      let sub_buffer = Buffer.create 16 in
      let sub_st = OutputStream.make_of_buffer sub_buffer in
      List.iter
        (fun a ->
          let res = encode bsm pool (a :> t) in
          write_info sub_st res)
        c.attributes;
      OutputStream.close sub_st;
      OutputStream.write_bytes st (Buffer.contents sub_buffer);
      return attr_code
  | `Exceptions l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st s ->
          let idx = ConstantPool.add_class pool s in
          OutputStream.write_u2 st idx)
        l;
      return attr_exceptions
  | `InnerClasses l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st { inner_class; outer_class; inner_name; inner_flags } ->
          let inner_idx = match inner_class with
          | None -> u2 0
          | Some c -> ConstantPool.add_class pool c in
          let outer_idx = match outer_class with
          | None -> u2 0
          | Some c -> ConstantPool.add_class pool c in
          let name_idx = match inner_name with
          | None -> u2 0
          | Some c -> ConstantPool.add_utf8 pool c in
          let fl = AccessFlag.list_to_u2 (inner_flags :> AccessFlag.t list) in
          OutputStream.write_u2 st inner_idx;
          OutputStream.write_u2 st outer_idx;
          OutputStream.write_u2 st name_idx;
          OutputStream.write_u2 st fl)
        l;
      return attr_inner_classes
  | `EnclosingMethod { innermost_class; enclosing_method } ->
      let class_idx = ConstantPool.add_class pool innermost_class in
      let meth_idx = match enclosing_method with
      | Some (n, d) ->
          ConstantPool.add_name_and_type pool
            (Name.utf8_for_method n)
            (Descriptor.utf8_of_method d)
      | None -> u2 0 in
      OutputStream.write_u2 st class_idx;
      OutputStream.write_u2 st meth_idx;
      return attr_enclosing_method
  | `Synthetic ->
      return attr_synthetic
  | `Signature s ->
      let s' = (match s with
      | `Class cs -> Signature.utf8_of_class_signature cs
      | `Method ms -> Signature.utf8_of_method_signature ms
      | `Field fs -> Signature.utf8_of_field_type_signature fs) in
      let idx = ConstantPool.add_utf8 pool s' in
      OutputStream.write_u2 st idx;
      return attr_signature
  | `SourceFile sf ->
      let idx = ConstantPool.add_utf8 pool sf in
      OutputStream.write_u2 st idx;
      return attr_source_file
  | `SourceDebugExtension sde ->
      let bytes = UTF8.bytes_of_modified (UTF8.to_modified sde) in
      Buffer.add_string buffer bytes;
      return attr_source_debug_extension
  | `LineNumberTable l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st (start_pc, line_number) ->
          OutputStream.write_u2 st start_pc;
          OutputStream.write_u2 st line_number)
        l;
      return attr_line_number_table
  | `LocalVariableTable l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st { local_start ; local_length; local_name; local_descriptor; local_index } ->
          let name_index = ConstantPool.add_utf8 pool local_name in
          let desc_val = Descriptor.utf8_of_field local_descriptor in
          let desc_index = ConstantPool.add_utf8 pool desc_val in
          OutputStream.write_u2 st local_start;
          OutputStream.write_u2 st local_length;
          OutputStream.write_u2 st name_index;
          OutputStream.write_u2 st desc_index;
          OutputStream.write_u2 st local_index)
        l;
      return attr_local_variable_table
  | `LocalVariableTypeTable l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st { local_type_start; local_type_length; local_type_name; local_type_signature; local_type_index } ->
          let name_index = ConstantPool.add_utf8 pool local_type_name in
          let sign_val = Signature.utf8_of_field_type_signature local_type_signature in
          let sign_index = ConstantPool.add_utf8 pool sign_val in
          OutputStream.write_u2 st local_type_start;
          OutputStream.write_u2 st local_type_length;
          OutputStream.write_u2 st name_index;
          OutputStream.write_u2 st sign_index;
          OutputStream.write_u2 st local_type_index)
        l;
      return attr_local_variable_type_table
  | `Deprecated ->
      return attr_deprecated
  | `RuntimeVisibleAnnotations l ->
      write_annotations l;
      return attr_runtime_visible_annotations
  | `RuntimeInvisibleAnnotations l ->
      write_annotations l;
      return attr_runtime_invisible_annotations
  | `RuntimeVisibleParameterAnnotations l ->
      write_annotations_list l;
      return attr_runtime_visible_parameter_annotations
  | `RuntimeInvisibleParameterAnnotations l ->
      write_annotations_list l;
      return attr_runtime_invisible_parameter_annotations
  | `RuntimeVisibleTypeAnnotations l ->
      write_extended_annotations l;
      return attr_runtime_visible_type_annotations
  | `RuntimeInvisibleTypeAnnotations l ->
      write_extended_annotations l;
      return attr_runtime_invisible_type_annotations
  | `AnnotationDefault ev ->
      let eiv = Annotation.encode_element_value pool ev in
      Annotation.write_info_element_value st eiv;
      return attr_annotation_default
  | `StackMapTable l ->
      let len = checked_length l in
      OutputStream.write_u2 st len;
      let ofs = ref (-1) in
      List.iter
        (fun x ->
          let o = write_stack_map_frame st pool !ofs x in
          ofs := (o :> int))
        l;
      return attr_stack_map_table
  | `BootstrapMethods l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st (method_ref, args) ->
          let reference = function
          | `getField x -> ConstantPool.Reference_getField x
          | `getStatic x -> ConstantPool.Reference_getStatic x
          | `putField x -> ConstantPool.Reference_putField x
          | `putStatic x -> ConstantPool.Reference_putStatic x
          | `invokeVirtual x -> ConstantPool.Reference_invokeVirtual x
          | `invokeStatic x -> ConstantPool.Reference_invokeStatic x
          | `invokeSpecial x -> ConstantPool.Reference_invokeSpecial x
          | `newInvokeSpecial x -> ConstantPool.Reference_newInvokeSpecial x
          | `invokeInterface x -> ConstantPool.Reference_invokeInterface x in
          let idx = ConstantPool.add_method_handle pool (reference method_ref) in
          OutputStream.write_u2 st idx;
          OutputStream.write_elements
            checked_length
            st
            (fun st elem ->
              let idx = match elem with
              | `String x -> ConstantPool.add_string pool x
              | `Class x -> ConstantPool.add_class pool x
              | `Integer x -> ConstantPool.add_integer pool x
              | `Long x -> ConstantPool.add_long pool x
              | `Float x -> ConstantPool.add_float pool x
              | `Double x -> ConstantPool.add_double pool x
              | `MethodHandle x -> ConstantPool.add_method_handle pool (reference x)
              | `MethodType x -> ConstantPool.add_method_type pool x in
              OutputStream.write_u2 st idx)
            args)
        l;
      return attr_bootstrap_methods;
  | `Module (n, v) ->
      let idx = ConstantPool.add_moduleid pool n v in
      OutputStream.write_u2 st idx;
      return attr_module
  | `ModuleRequires l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st (n, v, k) ->
          let idx = ConstantPool.add_moduleid pool n v in
          OutputStream.write_u2 st idx;
          OutputStream.write_u1 st (u1_of_dependency_kind k))
        l;
      return attr_module_requires
  | `ModulePermits l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st (n, v) ->
          let idx = ConstantPool.add_moduleid pool n v in
          OutputStream.write_u2 st idx)
        l;
      return attr_module_permits
  | `ModuleProvides l ->
      OutputStream.write_elements
        checked_length
        st
        (fun st (n, v) ->
          let idx = ConstantPool.add_moduleid pool n v in
          OutputStream.write_u2 st idx)
        l;
      return attr_module_provides
  | `Unknown (n, v) ->
      Buffer.add_string buffer v;
      return n

let compare x y =
  let rank = function
    | `ConstantValue _ -> 3
    | `Code _ -> 8
    | `Exceptions _ -> 3
    | `InnerClasses _ -> 2
    | `EnclosingMethod _ -> 2
    | `Synthetic -> 0
    | `Signature _ -> 1
    | `SourceFile _ -> 1
    | `SourceDebugExtension _ -> 6
    | `LineNumberTable _ -> -1
    | `LocalVariableTable _ -> -1
    | `LocalVariableTypeTable _ -> -1
    | `Deprecated -> 0
    | `RuntimeVisibleAnnotations _ -> 5
    | `RuntimeInvisibleAnnotations _ -> 5
    | `RuntimeVisibleParameterAnnotations _ -> 5
    | `RuntimeInvisibleParameterAnnotations _ -> 5
    | `RuntimeVisibleTypeAnnotations _ -> 5
    | `RuntimeInvisibleTypeAnnotations _ -> 5
    | `AnnotationDefault _ -> 4
    | `StackMapTable _ -> 9
    | `BootstrapMethods _ -> 10
    | `Module _ -> 1
    | `ModuleRequires _ -> 99
    | `ModulePermits _ -> 99
    | `ModuleProvides _ -> 99
    | `Unknown _ -> 7 in
  let cmp = compare (rank x) (rank y) in
  if cmp = 0 then compare x y else cmp

let rec version_bounds = function
  | `ConstantValue _ ->
      Version.make_bounds "'ConstantValue' attribute" Version.Java_1_0 None
  | `Code cv ->
      let code_bounds = Version.make_bounds "'Code' attribute" Version.Java_1_0 None in
      let instrs_bounds = List.map Instruction.version_bounds cv.code in
      let attrs_bounds = List.map version_bounds (cv.attributes :> t list) in
      Version.intersect_list (code_bounds :: (instrs_bounds @ attrs_bounds))
  | `Exceptions _ ->
      Version.make_bounds "'Exceptions' attribute" Version.Java_1_0 None
  | `InnerClasses _ ->
      Version.make_bounds "'InnerClasses' attribute" Version.Java_1_1 None
  | `EnclosingMethod _ ->
      Version.make_bounds "'EnclosingMethod' attribute" Version.Java_1_5 None
  | `Synthetic ->
      Version.make_bounds "'Synthetic' attribute" Version.Java_1_1 None
  | `Signature _ ->
      Version.make_bounds "'Signature' attribute" Version.Java_1_5 None
  | `SourceFile _ ->
      Version.make_bounds "'SourceFile' attribute" Version.Java_1_0 None
  | `SourceDebugExtension _ ->
      Version.make_bounds "'SourceDebugExtension' attribute" Version.Java_1_5 None
  | `LineNumberTable _ ->
      Version.make_bounds "'LineNumberTable' attribute" Version.Java_1_0 None
  | `LocalVariableTable _ ->
      Version.make_bounds "'LocalVariableTable' attribute" Version.Java_1_0 None
  | `LocalVariableTypeTable _ ->
      Version.make_bounds "'LocalVariableTypeTable' attribute" Version.Java_1_5 None
  | `Deprecated ->
      Version.make_bounds "'Deprecated' attribute" Version.Java_1_1 None
  | `RuntimeVisibleAnnotations _ ->
      Version.make_bounds "'RuntimeVisibleAnnotations' attribute" Version.Java_1_5 None
  | `RuntimeInvisibleAnnotations _ ->
      Version.make_bounds "'RuntimeInvisibleAnnotations' attribute" Version.Java_1_5 None
  | `RuntimeVisibleParameterAnnotations _ ->
      Version.make_bounds "'RuntimeVisibleParameterAnnotations' attribute" Version.Java_1_5 None
  | `RuntimeInvisibleParameterAnnotations _ ->
      Version.make_bounds "'RuntimeInvisibleParameterAnnotations' attribute" Version.Java_1_5 None
  | `RuntimeVisibleTypeAnnotations _ ->
      Version.make_bounds "'RuntimeVisibleTypeAnnotations' attribute" Version.Java_1_7 None
  | `RuntimeInvisibleTypeAnnotations _ ->
      Version.make_bounds "'RuntimeInvisibleTypeAnnotations' attribute" Version.Java_1_7 None
  | `AnnotationDefault _ ->
      Version.make_bounds "'AnnotationDefault' attribute" Version.Java_1_5 None
  | `StackMapTable _ ->
      Version.make_bounds "'StackMapTable' attribute" Version.Java_1_6 None
  | `BootstrapMethods _ ->
      Version.make_bounds "'BootstrapMethods' attribute" Version.Java_1_7 None
  | `Module _ ->
      Version.make_bounds "'Module' attribute" Version.Java_1_8 None
  | `ModuleRequires _ ->
      Version.make_bounds "'ModuleRequires' attribute" Version.Java_1_8 None
  | `ModulePermits _ ->
      Version.make_bounds "'ModulePermits' attribute" Version.Java_1_8 None
  | `ModuleProvides _ ->
      Version.make_bounds "'ModuleProvides' attribute" Version.Java_1_8 None
  | `Unknown _ ->
      Version.make_bounds "'Unknown' attribute" Version.Java_1_0 None


(* Common extractors *)

let rec extract_code = function
  | (`Code c) :: _ -> c
  | _ :: tl -> extract_code tl
  | [] -> raise Not_found

let rec extract_exceptions = function
  | (`Exceptions e) :: _ -> e
  | _ :: tl -> extract_exceptions tl
  | [] -> raise Not_found

let rec extract_class_signature = function
  | (`Signature (`Class s)) :: _ -> s
  | _ :: tl -> extract_class_signature tl
  | [] -> raise Not_found

let rec extract_bootstrap_info = function
  | (`BootstrapMethods bsm) :: _ -> bsm
  | _ :: tl -> extract_bootstrap_info tl
  | [] -> raise Not_found

let rec extract_field_signature = function
  | (`Signature (`Field s)) :: _ -> s
  | _ :: tl -> extract_field_signature tl
  | [] -> raise Not_found

let rec extract_method_signature = function
  | (`Signature (`Method s)) :: _ -> s
  | _ :: tl -> extract_method_signature tl
  | [] -> raise Not_found

let extract_annotations l =
  let rec extract accu = function
    | (`RuntimeVisibleAnnotations a) :: tl
    | (`RuntimeInvisibleAnnotations a) :: tl ->
        extract (accu @ a) tl
    | _ :: tl -> extract accu tl
    | [] -> accu in
  extract [] l
