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


(* Exception *)

type error =
  | Invalid_magic of u2
  | Invalid_version of u2
  | Invalid_stream
  | Array_type_waited
  | Unknown_reference
  | Invalid_class_flags of s1
  | Missing_read_function
  | Missing_write_function
  | Missing_field of UTF8.t
  | Invalid_field_type of UTF8.t

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_magic x ->
      Printf.sprintf "invalid magic (%d)" (x :> int)
  | Invalid_version x ->
      Printf.sprintf "invalid version (%d)" (x :> int)
  | Invalid_stream ->
      "invalid stream"
  | Array_type_waited ->
      "array type waited"
  | Unknown_reference ->
      "unknown reference"
  | Invalid_class_flags x ->
      Printf.sprintf "invalid class flags (%d)" (x :> int)
  | Missing_read_function ->
      "missing read function"
  | Missing_write_function ->
      "missing write function"
  | Missing_field x ->
      Printf.sprintf "missing value for field %S" (UTF8.to_string_noerr x)
  | Invalid_field_type x ->
      Printf.sprintf "invalid type for field %S" (UTF8.to_string_noerr x)

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Protocol constants *)

let stream_magic = 0xaced

let stream_version = 5

let tc_null = 0x70

let tc_reference = 0x71

let tc_classdesc = 0x72

let tc_object = 0x73

let tc_string = 0x74

let tc_array = 0x75

let tc_class = 0x76

let tc_blockdata = 0x77

let tc_endblockdata = 0x78

let tc_reset = 0x79

let tc_blockdatalong = 0x7A

let tc_exception = 0x7B

let tc_longstring =  0x7C

let tc_proxyclassdesc =  0x7D

let tc_enum = 0x7E

let base_wire_handle = 0x7E0000l

let sc_write_method = 0x01

let sc_block_data = 0x08

let sc_serializable = 0x02

let sc_externalizable = 0x04

let sc_enum = 0x10


(* Object values *)

type descriptor = {
    mutable desc_flags : s1;
    mutable desc_serial_version_uid : s8;
    mutable desc_class_name : UTF8.t;
    mutable desc_class_annotation : object_value list;
    mutable desc_super_class_desc : descriptor option;
    mutable desc_fields : (Descriptor.for_field * UTF8.t) list;
    desc_externalizable_functions : (read_method * write_method) option;
    mutable desc_proxy_interfaces : UTF8.t list;
  }
and read_method = InputStream.t -> instance -> unit
and write_method = OutputStream.t -> instance -> unit
and instance = {
    inst_descriptor : descriptor;
    mutable inst_fields : field_value list;
    mutable inst_annotations : object_value list;
  }
and array_instance =
  | Boolean_array of bool array
  | Byte_array of Utils.s1 array
  | Char_array of Utils.u2 array
  | Double_array of float array
  | Float_array of float array
  | Int_array of Utils.s4 array
  | Long_array of Utils.s8 array
  | Object_array of object_value array
  | Short_array of Utils.s2 array
and object_value =
  | Null
  | Block_data of string
  | String of UTF8.t
  | Class_desc of descriptor
  | Instance of instance
  | Array_instance of descriptor * array_instance
  | Enum of (descriptor * UTF8.t)
and field_value =
  | Boolean_value of bool
  | Byte_value of Utils.s1
  | Char_value of Utils.u2
  | Double_value of float
  | Float_value of float
  | Int_value of Utils.s4
  | Long_value of Utils.s8
  | Object_value of object_value
  | Short_value of Utils.s2

let new_descriptor () =
  { desc_flags = s1 0;
    desc_serial_version_uid = s8 0L;
    desc_class_name = UTF8.of_string "<dummy>";
    desc_class_annotation = [];
    desc_super_class_desc = None;
    desc_fields = [];
    desc_externalizable_functions = None;
    desc_proxy_interfaces = []; }

let make_descriptor class_name serial_uid annot fields super ext methods =
  let externalizable = if ext then sc_externalizable else sc_serializable in
  let write_method = match methods with Some _ -> sc_write_method | None -> 0 in
  { desc_flags = s1 (externalizable + write_method);
    desc_serial_version_uid = serial_uid;
    desc_class_name = class_name;
    desc_class_annotation = annot;
    desc_super_class_desc = super;
    desc_fields = List.map (fun (t, n) -> (t, Name.utf8_for_field n)) fields;
    desc_externalizable_functions = methods;
    desc_proxy_interfaces = [] }

let make_proxy_descriptor interfaces annot super =
  { desc_flags = s1 0;
    desc_serial_version_uid = s8 0L;
    desc_class_name = UTF8.of_string "";
    desc_class_annotation = annot;
    desc_super_class_desc = super;
    desc_fields = [];
    desc_externalizable_functions = None;
    desc_proxy_interfaces = interfaces; }

let serial_of_descriptor d =
  d.desc_serial_version_uid

let class_name_of_descriptor d =
  d.desc_class_name

let class_annotation_of_descriptor d =
  d.desc_class_annotation

let super_class_desc_of_descriptor d =
  d.desc_super_class_desc

let fields_of_descriptor d =
  List.map (fun (t, n) -> (t, Name.make_for_field n)) d.desc_fields

let methods_of_descriptor d =
  d.desc_externalizable_functions

let new_instance desc =
  { inst_descriptor = desc;
    inst_fields = [];
    inst_annotations = []; }

let make_instance desc fields annot =
  let fields = List.map (fun (x, y) -> (Name.utf8_for_field x, y)) fields in
  let fields' : field_value list =
    List.map
      (fun (t, n) ->
        let _, v =
          try
            List.find (fun (n', _) -> UTF8.equal n' n) fields
          with Not_found -> fail (Missing_field n) in
        match t, v with
        | `Boolean, (Boolean_value _)
        | `Byte, (Byte_value _)
        | `Char, (Char_value _)
        | `Double, (Double_value _)
        | `Float, (Float_value _)
        | `Int, (Int_value _)
        | `Long, (Long_value _)
        | (`Class _), (Object_value _)
        | (`Array _), (Object_value _)
        | `Short, (Short_value _) -> v
        | _ -> fail (Invalid_field_type n))
      desc.desc_fields in
  { inst_descriptor = desc;
    inst_fields = fields';
    inst_annotations = annot; }


(* Serialization and deserialization of object values *)

module HT = struct
  type t = object_value
  let rec equal x y =
    match x, y with
    | Null, Null -> true
    | (Block_data bd1), (Block_data bd2) -> bd1 = bd2
    | (String s1), (String s2) -> UTF8.equal s1 s2
    | (Class_desc d1), (Class_desc d2) -> d1 == d2
    | (Instance i1), (Instance i2) -> i1 == i2
    | (Array_instance (d1, ai1)), (Array_instance (d2, ai2)) -> d1 == d2 && (equal_array_instance ai1 ai2)
    | (Enum (d1, n1)), (Enum (d2, n2)) -> d1 == d2 && (UTF8.equal n1 n2)
    | _ -> false
  and equal_array_instance x y =
    match x, y with
    | (Boolean_array x), (Boolean_array y) -> x = y
    | (Byte_array x), (Byte_array y) -> x = y
    | (Char_array x), (Char_array y) -> x = y
    | (Double_array x), (Double_array y) -> x = y
    | (Float_array x), (Float_array y) -> x = y
    | (Int_array x), (Int_array y) -> x = y
    | (Long_array x), (Long_array y) -> x = y
    | (Object_array x), (Object_array y) ->
        let len_x = Array.length x in
        let len_y = Array.length y in
        if len_x = len_y then
          let i = ref 0 in
          while (!i < len_x) && equal x.(!i) y.(!i) do
            incr i
          done;
          !i = len_x
        else
          false
    | (Short_array x), (Short_array y) -> x = y
    | _ -> false
  let rec hash x =
    match x with
    | Null
    | Block_data _
    | Class_desc _
    | Instance _ -> Hashtbl.hash x
    | Array_instance (d, ai) -> (Hashtbl.hash d) + (hash_array_instance ai)
    | String s -> hash_utf8 s
    | Enum (d, s) -> (Hashtbl.hash d) + (hash_utf8 s)
  and hash_array_instance x =
    match x with
    | Boolean_array _
    | Byte_array _
    | Char_array _
    | Double_array _
    | Float_array _
    | Int_array _
    | Long_array _
    | Short_array _ -> Hashtbl.hash x
    | Object_array a -> Array.fold_left (fun acc elem -> acc + (hash elem)) 0 a
  and hash_utf8 x = Hashtbl.hash (UTF8.to_string x)
end

module Hashtable = Hashtbl.Make (HT)

let write_contents os seen (l : object_value list) =
  let checked_length l =
    let res = List.length l in
    if res <= max_u2 then
      u2 res
    else
      fail Invalid_stream in
  let write_tag tag =
    OutputStream.write_u1 os (u1 tag) in
  let write_utf s =
    let modified = UTF8.to_modified s in
    let data = UTF8.bytes_of_modified modified in
    let len = String.length data in
    OutputStream.write_u2 os (u2 len);
    OutputStream.write_bytes os data in
  let add x =
    let idx = Int32.add (Int32.of_int (Hashtbl.length seen)) base_wire_handle in
    Hashtbl.add seen x idx in
  let rec write_class_annotation l =
    List.iter write_object_if l;
    write_tag tc_endblockdata
  and write_values inst =
    List.iter
      (function
        | Boolean_value x -> OutputStream.write_u1 os (u1 (if x then 1 else 0))
        | Byte_value x -> OutputStream.write_s1 os x
        | Char_value x -> OutputStream.write_u2 os x
        | Double_value x -> OutputStream.write_s8 os (s8 (Int64.bits_of_float x))
        | Float_value x -> OutputStream.write_s4 os (s4 (Int32.bits_of_float x))
        | Int_value x -> OutputStream.write_s4 os x
        | Long_value x -> OutputStream.write_s8 os x
        | Object_value x -> write_object_if x
        | Short_value x -> OutputStream.write_s2 os x)
    inst.inst_fields
  and write_object = function
    | Null ->
        write_tag tc_null
    | Block_data data ->
        let len = String.length data in
        if len < 256 then begin
          write_tag tc_blockdata;
          OutputStream.write_u1 os (u1 len);
          OutputStream.write_bytes os data
        end else begin
          write_tag tc_blockdatalong;
          OutputStream.write_s4 os (s4 (Int32.of_int len));
          OutputStream.write_bytes os data
        end
    | (String s) as elem ->
        let modified = UTF8.to_modified s in
        let data = UTF8.bytes_of_modified modified in
        let len = String.length data in
        if len <= max_u2 then begin
          write_tag tc_string;
          OutputStream.write_u2 os (u2 len);
          OutputStream.write_bytes os data
        end else begin
          write_tag tc_longstring;
          OutputStream.write_s8 os (s8 (Int64.of_int len));
          OutputStream.write_bytes os data
        end;
        add elem
    | (Class_desc desc) as elem ->
        if desc.desc_proxy_interfaces = [] then begin
          write_tag tc_classdesc;
          write_utf desc.desc_class_name;
          OutputStream.write_s8 os desc.desc_serial_version_uid;
        add elem;
          OutputStream.write_s1 os desc.desc_flags;
          OutputStream.write_s2 os (s2 ((checked_length desc.desc_fields) :> int));
          List.iter
            (fun (typ, nam) ->
              let write_char ch =
                 OutputStream.write_u1 os (u1 (int_of_char ch)) in
              (match typ with
              | `Byte -> write_char 'B'; write_utf nam
              | `Char -> write_char 'C'; write_utf nam
              | `Double -> write_char 'D'; write_utf nam
              | `Float -> write_char 'I'; write_utf nam
              | `Int -> write_char 'I'; write_utf nam
              | `Long -> write_char 'J'; write_utf nam
              | `Short -> write_char 'S'; write_utf nam
              | `Boolean -> write_char 'Z'; write_utf nam
              | `Array _ -> write_char '['; write_utf nam; write_object_if (String (Descriptor.utf8_of_field typ))
              | `Class _ -> write_char 'L'; write_utf nam; write_object_if (String (Descriptor.utf8_of_field typ))))
            desc.desc_fields;
          write_class_annotation desc.desc_class_annotation;
          (match desc.desc_super_class_desc with
          | Some x -> write_object_if (Class_desc x)
          | None -> write_tag tc_null)
        end else begin
          write_tag tc_proxyclassdesc;
          add elem;
          OutputStream.write_s4 os (s4 (Int32.of_int (List.length desc.desc_proxy_interfaces)));
          List.iter
            write_utf
            desc.desc_proxy_interfaces;
          write_class_annotation desc.desc_class_annotation;
          (match desc.desc_super_class_desc with
          | Some x -> write_object_if (Class_desc x)
          | None -> write_tag tc_null)
        end
    | (Instance instance) as elem ->
        write_tag tc_object;
        let class_desc = instance.inst_descriptor in
        write_object_if (Class_desc class_desc);
        add elem;
        let is_serializable = sc_serializable land (class_desc.desc_flags :> int) <> 0 in
        let is_externalizable = sc_externalizable land (class_desc.desc_flags :> int) <> 0 in
        let is_write_method = sc_write_method land (class_desc.desc_flags :> int) <> 0 in
        let is_block_data = sc_block_data land (class_desc.desc_flags :> int) <> 0 in
        if is_serializable && not is_write_method then begin
          (* nowrclass *)
          write_values instance
        end else if is_serializable && is_write_method then begin
          (* wrclass objectAnnotation *)
          write_values instance;
          write_class_annotation instance.inst_annotations
        end else if is_externalizable && not is_block_data then begin
          (* externalContents *)
          match class_desc.desc_externalizable_functions with
          | Some (_, f) -> f os instance
          | None -> fail Missing_write_function
        end else if is_externalizable && is_block_data then begin
          (* objectAnnotation *)
          write_class_annotation instance.inst_annotations
        end else
          fail (Invalid_class_flags class_desc.desc_flags)
    | (Array_instance (desc, array_inst)) as elem ->
        write_tag tc_array;
        write_object_if (Class_desc desc);
        add elem;
        let write_array f arr =
          let size = Array.length arr in
          OutputStream.write_s4 os (s4 (Int32.of_int size));
          Array.iter f arr in
        (match array_inst with
        | Boolean_array arr ->
            write_array (fun x -> OutputStream.write_u1 os (u1 (if x then 1 else 0))) arr
        | Byte_array arr ->
            write_array (OutputStream.write_s1 os) arr
        | Char_array arr ->
            write_array (OutputStream.write_u2 os) arr
        | Double_array arr ->
            write_array (fun x -> OutputStream.write_s8 os (s8 (Int64.bits_of_float x))) arr
        | Float_array arr ->
            write_array (fun x -> OutputStream.write_s4 os (s4 (Int32.bits_of_float x))) arr
        | Int_array arr ->
            write_array (OutputStream.write_s4 os) arr
        | Long_array arr ->
            write_array (OutputStream.write_s8 os) arr
        | Short_array arr ->
            write_array (OutputStream.write_s2 os) arr
        | Object_array arr ->
            write_array (write_object_if) arr)
    | (Enum (desc, name)) as elem ->
        write_tag tc_enum;
        write_object_if (Class_desc desc);
        add elem;
        write_object_if (String name)
  and write_object_if x =
    match x with
    | Block_data _ ->
        write_object x
    | x ->
        try
          let idx = Hashtbl.find seen x in
          write_tag tc_reference;
          OutputStream.write_s4 os (s4 idx)
        with Not_found ->
          write_object x in
  List.iter write_object_if l

let encode os l =
  OutputStream.write_u2 os (u2 stream_magic);
  OutputStream.write_u2 os (u2 stream_version);
  write_contents os (Hashtbl.create 17) l

let rec read_contents is seen acc =
  let add_elem x =
    let idx = Int32.add (Int32.of_int (Hashtbl.length seen)) base_wire_handle in
    Hashtbl.add seen idx x; Some x in
  let add_elem_idx x =
    let idx = Int32.add (Int32.of_int (Hashtbl.length seen)) base_wire_handle in
    Hashtbl.add seen idx x; idx in
  let get_elem x =
    try Hashtbl.find seen x with Not_found -> fail Unknown_reference in
  let reset () = Hashtbl.clear seen in
  let get_tag : u1 option -> u1 option = function
    | Some t -> Some t
    | None ->
        try
          Some (InputStream.read_u1 is)
        with InputStream.Exception InputStream.End_of_input_stream -> None in
  let read_utf () =
    let size = InputStream.read_u2 is in
    let data = InputStream.read_bytes is (size :> int) in
    let modified = UTF8.modified_of_bytes data in
    UTF8.of_modified modified in
  let rec read_class_annotation acc =
    match get_tag None with
    | Some tag ->
        if (tag :> int) = tc_endblockdata then
          List.rev acc
        else
          let cnt = read_content (Some tag) in
          (match cnt with
          | Some x -> read_class_annotation (x :: acc)
          | None -> fail Invalid_stream)
    | None -> fail Invalid_stream
  and read_values instance class_desc =
    let values =
      List.map
        (fun (t, _) ->
          match t with
          | `Boolean -> Boolean_value (((InputStream.read_u1 is) :> int) <> 0)
          | `Byte -> Byte_value (InputStream.read_s1 is)
          | `Char -> Char_value (InputStream.read_u2 is)
          | `Double -> Double_value (Int64.float_of_bits ((InputStream.read_s8 is) :> int64))
          | `Float -> Float_value (Int32.float_of_bits ((InputStream.read_s4 is) :> int32))
          | `Int -> Int_value (InputStream.read_s4 is)
          | `Long -> Long_value (InputStream.read_s8 is)
          | `Short -> Short_value (InputStream.read_s2 is)
          | _ -> Object_value (match read_content None with Some x -> x | None -> fail Invalid_stream))
        class_desc.desc_fields in
      instance.inst_fields <- values
  and read_content t =
    match get_tag t with
    | Some tag ->
        let tag = (tag :> int) in
        if tag = tc_object then begin
          (* newObject *)
          let class_desc = match read_content None with
          | Some (Class_desc d) -> d
          | Some Null -> new_descriptor ()
          | _ -> fail Invalid_stream in
          let instance = new_instance class_desc in
          let res = add_elem (Instance instance) in
          let is_serializable = sc_serializable land (class_desc.desc_flags :> int) <> 0 in
          let is_externalizable = sc_externalizable land (class_desc.desc_flags :> int) <> 0 in
          let is_write_method = sc_write_method land (class_desc.desc_flags :> int) <> 0 in
          let is_block_data = sc_block_data land (class_desc.desc_flags :> int) <> 0 in
          if is_serializable && not is_write_method then begin
            (* nowrclass *)
            read_values instance class_desc
          end else if is_serializable && is_write_method then begin
            (* wrclass objectAnnotation *)
            read_values instance class_desc;
            instance.inst_annotations <- read_class_annotation []
          end else if is_externalizable && not is_block_data then begin
            (* externalContents *)
            match class_desc.desc_externalizable_functions with
            | Some (f, _) -> f is instance
            | None -> fail Missing_read_function
          end else if is_externalizable && is_block_data then begin
            (* objectAnnotation *)
            instance.inst_annotations <- read_class_annotation []
          end else
            fail (Invalid_class_flags class_desc.desc_flags);
          res
        end else if tag = tc_class then begin
          (* newClass *)
          let class_desc = read_content None in
          match class_desc with
          | Some ((Class_desc _) as d)
          | Some (Null as d) -> add_elem d
          | _ -> fail Invalid_stream
        end else if tag = tc_array then begin
          (* newArray *)
          let class_desc = match read_content None with
          | Some (Class_desc d) -> d
          | Some Null -> new_descriptor ()
          | _ -> fail Invalid_stream in
          let array_instance = Boolean_array [||] in
          let idx = add_elem_idx (Array_instance (class_desc, array_instance)) in
          let element_type = match Descriptor.field_of_utf8 class_desc.desc_class_name with
          | `Array x -> x
          | _ -> fail Array_type_waited in
          let size = InputStream.read_s4 is in
          let make_array f = Array.init (Int32.to_int (size :> int32)) f in
          let array_instance = match element_type with
          | `Boolean ->
              Boolean_array (make_array (fun _ -> ((InputStream.read_u1 is) :> int) <> 0))
          | `Byte ->
              Byte_array (make_array (fun _ -> InputStream.read_s1 is))
          | `Char ->
              Char_array (make_array (fun _ -> InputStream.read_u2 is))
          | `Double ->
              Double_array (make_array (fun _ -> Int64.float_of_bits ((InputStream.read_s8 is) :> int64)))
          | `Float ->
              Float_array (make_array (fun _ -> Int32.float_of_bits ((InputStream.read_s4 is) :> int32)))
          | `Int ->
              Int_array (make_array (fun _ -> InputStream.read_s4 is))
          | `Long ->
              Long_array (make_array (fun _ -> InputStream.read_s8 is))
          | `Short ->
              Short_array (make_array (fun _ -> InputStream.read_s2 is))
          | _ ->
              Object_array (make_array (fun _ -> match read_content None with Some x -> x | None -> fail Invalid_stream)) in
          let res = Array_instance (class_desc, array_instance) in
          Hashtbl.replace seen idx res;
          Some res
        end else if tag = tc_string then begin
          (* newString *)
          add_elem (String (read_utf ()))
        end else if tag = tc_longstring then begin
          (* newString *)
          let size = InputStream.read_s8 is in
          let data = InputStream.read_bytes is (Int64.to_int (size :> int64)) in
          let modified = UTF8.modified_of_bytes data in
          add_elem (String (UTF8.of_modified modified))
        end else if tag = tc_enum then begin
          (* newEnum *)
          let class_desc = match read_content None with
          | Some (Class_desc d) -> d
          | Some Null -> new_descriptor ()
          | _ -> fail Invalid_stream in
          let name = match read_content None with
          | Some (String s) -> s
          | _ -> fail Invalid_stream in
          Some (Enum (class_desc, name))
        end else if tag = tc_classdesc then begin
          (* newClassDesc *)
          let class_name = read_utf () in
          let class_name = UTF8.replace (UChar.of_char '.') (UChar.of_char '/') class_name in
          let serial_version_uid = InputStream.read_s8 is in
          let desc = new_descriptor () in
          let res = add_elem (Class_desc desc) in
          let flags = InputStream.read_s1 is in
          let field_count = InputStream.read_s2 is in
          let fields = ref [] in
          for i = 1 to (field_count :> int) do
            let field_tag = InputStream.read_u1 is in
            let add_field t n = fields := (t, n) :: !fields in
            let read_object_desc () =
              let name = read_utf () in
              match read_content None with
              | Some (String s) -> add_field (Descriptor.field_of_utf8 s) name
              | _ -> fail Invalid_stream in
            match char_of_int (field_tag :> int) with
            | 'B' -> add_field `Byte (read_utf ())
            | 'C' -> add_field `Char (read_utf ())
            | 'D' -> add_field `Double (read_utf ())
            | 'F' -> add_field `Float (read_utf ())
            | 'I' -> add_field `Int (read_utf ())
            | 'J' -> add_field `Long (read_utf ())
            | 'S' -> add_field `Short (read_utf ())
            | 'Z' -> add_field `Boolean (read_utf ())
            | '[' -> read_object_desc ()
            | 'L' -> read_object_desc ()
            | _ -> fail Invalid_stream
          done;
          let class_annotation = read_class_annotation [] in
          let super_class_desc = match read_content None with
          | Some (Class_desc d) -> Some d
          | Some Null -> None
          | _ -> fail Invalid_stream in
          desc.desc_flags <- flags;
          desc.desc_serial_version_uid <- serial_version_uid;
          desc.desc_class_name <- class_name;
          desc.desc_class_annotation <- class_annotation;
          desc.desc_super_class_desc <- super_class_desc;
          desc.desc_fields <- List.rev !fields;
          res
        end else if tag = tc_proxyclassdesc then begin
          (* newClassDesc *)
          let desc = new_descriptor () in
          let res = add_elem (Class_desc desc) in
          let count = InputStream.read_s4 is in
          let interfaces = ref [] in
          for i = 1 to Int32.to_int (count :> int32) do
            let itf = read_utf () in
            interfaces :=  itf :: !interfaces
          done;
          let class_annotation = read_class_annotation [] in
          let super_class_desc = match read_content None with
          | Some (Class_desc d) -> Some d
          | Some Null -> None
          | _ -> fail Invalid_stream in
          desc.desc_class_annotation <- class_annotation;
          desc.desc_super_class_desc <- super_class_desc;
          desc.desc_proxy_interfaces <- List.rev !interfaces;
          res
        end else if tag = tc_reference then begin
          (* prevObject *)
          let idx = InputStream.read_s4 is in
          Some (get_elem (idx :> int32))
        end else if tag = tc_null then begin
          (* nullReference *)
          Some Null
        end else if tag = tc_exception then begin
          (* exception *)
          reset ();
          match read_content None with
          | Some (Block_data _) ->
              fail Invalid_stream
          | except ->
              reset ();
              except
        end else if tag = tc_reset then begin
          (* reset *)
          reset ();
          read_content None
        end else if tag = tc_blockdata then begin
          (* blockdatashort *)
          let size = InputStream.read_u1 is in
          Some (Block_data (InputStream.read_bytes is (size :> int)))
        end else if tag = tc_blockdatalong then begin
          (* blockdatalong *)
          let size = InputStream.read_s4 is in
          Some (Block_data (InputStream.read_bytes is (Int32.to_int (size :> int32))))
        end else
          fail Invalid_stream
    | None -> None in
  match read_content None with
  | Some x -> read_contents is seen (x :: acc)
  | None -> List.rev acc

let decode is =
  let magic = InputStream.read_u2 is in
  if (magic :> int) <> stream_magic then fail (Invalid_magic magic);
  let version = InputStream.read_u2 is in
  if (version :> int) <> stream_version then fail (Invalid_version version);
  read_contents is (Hashtbl.create 17) []
