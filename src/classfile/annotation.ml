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

type location = u1 list

type intervals = (u2 * u2 * u2) list

type target =
  | Typecast of u2
  | Typecast_loc of u2 * location
  | Instance_of of u2
  | Instance_of_loc of u2 * location
  | New of u2
  | New_loc of u2 * location
  | Method_receiver
  | Local_variable of intervals
  | Local_variable_loc of intervals * location
  | Method_return_type
  | Method_return_type_loc of location
  | Method_parameter of u1
  | Method_parameter_loc of u1 * location
  | Field
  | Field_loc of location
  | Class_type_parameter_bound of u1 * u1
  | Class_type_parameter_bound_loc of u1 * u1 * location
  | Method_type_parameter_bound of u1 * u1
  | Method_type_parameter_bound_loc of u1 * u1 * location
  | Super_type of u2
  | Super_type_loc of u2 * location
  | Thrown_exception of u2
  | Type_argument_constructor_call of u2 * u1
  | Type_argument_constructor_call_loc of u2 * u1 * location
  | Type_argument_method_call of u2 * u1
  | Type_argument_method_call_loc of u2 * u1 * location
  | Wildcard_bound of target
  | Wildcard_bound_loc of target * location
  | Class_literal of u2
  | Class_literal_loc of u2 * location
  | Method_type_parameter of u1
  | Class_type_parameter of u1

type primitive_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short]

type info_element_value =
  | Primitive of primitive_type * u2
  | String of u2
  | Enum of u2 * u2
  | Class of u2
  | Annotation of info
  | Array of u2 * (info_element_value array)
and info = {
    type_index : u2;
    num_element_value_pairs : u2;
    element_value_pairs : (u2 * info_element_value) array;
  }
and extended_info = {
    ext_type_index : Utils.u2;
    ext_num_element_value_pairs : Utils.u2;
    ext_element_value_pairs : (Utils.u2 * info_element_value) array;
    ext_target : target;
  }


(* Exception *)

type error =
  | Invalid_tag of UChar.t
  | Inconsistent_primitive_value
  | Invalid_string_value of u2
  | Invalid_enum_value of u2 * u2
  | Invalid_class_value of u2
  | Invalid_annotation_type_value of u2
  | Invalid_element_name of u2
  | Invalid_list_length of int
  | Invalid_target of int

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_tag x ->
      Printf.sprintf "invalid tag (%C)" (UChar.to_char_noerr x)
  | Inconsistent_primitive_value ->
      "inconsistent primitive value"
  | Invalid_string_value x ->
      Printf.sprintf "invalid string value (index %d)" (x :> int)
  | Invalid_enum_value (x, y) ->
      Printf.sprintf "invalid enum value (indexes %d and %d)" (x :> int) (y :> int)
  | Invalid_class_value x ->
      Printf.sprintf "invalid class value (index %d)" (x :> int)
  | Invalid_annotation_type_value x ->
      Printf.sprintf "invalid annotation type value (index %d)" (x :> int)
  | Invalid_element_name x ->
      Printf.sprintf "invalid element name (index %d)" (x :> int)
  | Invalid_list_length x ->
      Printf.sprintf "invalid list length (%d)" x
  | Invalid_target x ->
      Printf.sprintf "invalid target (0x%02x)" x

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* I/O functions *)

let rec read_target st =
  let tag = InputStream.read_u1 st in
  let read_offset () = InputStream.read_u2 st in
  let read_intervals () =
    InputStream.read_elements
      st
      (fun st ->
        let start = InputStream.read_u2 st in
        let length = InputStream.read_u2 st in
        let index = InputStream.read_u2 st in
        start, length, index) in
  let read_location () =
    InputStream.read_elements
      st
      (fun st ->
        InputStream.read_u1 st) in
  let unsupported x = fail (Invalid_target x) in
  match (tag :> int) with
  | 0x00 -> Typecast (read_offset ())
  | 0x01 ->
      let ofs = read_offset () in
      let loc = read_location () in
      Typecast_loc (ofs, loc)
  | 0x02 -> Instance_of (read_offset ())
  | 0x03 ->
      let ofs = read_offset () in
      let loc = read_location () in
      Instance_of_loc (ofs, loc)
  | 0x04 -> New (read_offset ())
  | 0x05 ->
      let ofs = read_offset () in
      let loc = read_location () in
      New_loc (ofs, loc)
  | 0x06 -> Method_receiver
  | 0x07 -> unsupported 0x07
  | 0x08 -> Local_variable (read_intervals ())
  | 0x09 ->
      let itv = read_intervals () in
      let loc = read_location () in
      Local_variable_loc (itv, loc)
  | 0x0A -> Method_return_type
  | 0x0B -> Method_return_type_loc (read_location ())
  | 0x0C -> Method_parameter (InputStream.read_u1 st)
  | 0x0D ->
      let param_index = InputStream.read_u1 st in
      let loc = read_location () in
      Method_parameter_loc (param_index, loc)
  | 0x0E -> Field
  | 0x0F -> Field_loc (read_location ())
  | 0x10 ->
      let param_index = InputStream.read_u1 st in
      let bound_index = InputStream.read_u1 st in
      Class_type_parameter_bound (param_index, bound_index)
  | 0x11 ->
      let param_index = InputStream.read_u1 st in
      let bound_index = InputStream.read_u1 st in
      let loc = read_location () in
      Class_type_parameter_bound_loc (param_index, bound_index, loc)
  | 0x12 ->
      let param_index = InputStream.read_u1 st in
      let bound_index = InputStream.read_u1 st in
      Method_type_parameter_bound (param_index, bound_index)
  | 0x13 ->
      let param_index = InputStream.read_u1 st in
      let bound_index = InputStream.read_u1 st in
      let loc = read_location () in
      Method_type_parameter_bound_loc (param_index, bound_index, loc)
  | 0x14 -> Super_type (InputStream.read_u2 st)
  | 0x15 ->
      let type_index = InputStream.read_u1 st in
      let loc = read_location () in
      Super_type_loc (u2_of_u1 type_index, loc)
  | 0x16 -> Thrown_exception (InputStream.read_u2 st)
  | 0x17 -> unsupported 0x17
  | 0x18 ->
      let offset = InputStream.read_u2 st in
      let type_index = InputStream.read_u1 st in
      Type_argument_constructor_call (offset, type_index)
  | 0x19 ->
      let offset = InputStream.read_u2 st in
      let type_index = InputStream.read_u1 st in
      let loc = read_location () in
      Type_argument_constructor_call_loc (offset, type_index, loc)
  | 0x1A ->
      let offset = InputStream.read_u2 st in
      let type_index = InputStream.read_u1 st in
      Type_argument_method_call (offset, type_index)
  | 0x1B ->
      let offset = InputStream.read_u2 st in
      let type_index = InputStream.read_u1 st in
      let loc = read_location () in
      Type_argument_method_call_loc (offset, type_index, loc)
  | 0x1C -> Wildcard_bound (read_target st)
  | 0x1D ->
      let target = read_target st in
      let loc = read_location () in
      Wildcard_bound_loc (target, loc)
  | 0x1E -> Class_literal (read_offset ())
  | 0x1F ->
      let ofs = read_offset () in
      let loc = read_location () in
      Class_literal_loc (ofs, loc)
  | 0x20 -> Method_type_parameter (InputStream.read_u1 st)
  | 0x21 -> unsupported 0x21
  | 0x22 -> Class_type_parameter (InputStream.read_u1 st)
  | 0x23 -> unsupported 0x23
  | x -> fail (Invalid_target x)

let checked_length l =
  let res = List.length l in
  if res <= max_u2 then
    u2 res
  else
    fail (Invalid_list_length res)

let rec write_target st t =
  let write_tag x =
    OutputStream.write_u1 st (u1 x) in
  let write_offset x =
    OutputStream.write_u2 st x in
  let write_intervals l =
    OutputStream.write_elements
      checked_length
      st
      (fun st (x, y, z) ->
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y;
        OutputStream.write_u2 st z)
      l in
  let write_location l =
    OutputStream.write_elements
      checked_length
      st
      (fun st x ->
        OutputStream.write_u1 st x)
      l in
  match t with
  | Typecast ofs -> write_tag 0x00; write_offset ofs
  | Typecast_loc (ofs, loc) -> write_tag 0x01; write_offset ofs; write_location loc
  | Instance_of ofs -> write_tag 0x02; write_offset ofs
  | Instance_of_loc (ofs, loc) -> write_tag 0x03; write_offset ofs; write_location loc
  | New ofs -> write_tag 0x04; write_offset ofs
  | New_loc (ofs, loc) -> write_tag 0x05; write_offset ofs; write_location loc
  | Method_receiver -> write_tag 0x06
  | Local_variable itv -> write_tag 0x08; write_intervals itv
  | Local_variable_loc (itv, loc) -> write_tag 0x09; write_intervals itv; write_location loc
  | Method_return_type -> write_tag 0x0A
  | Method_return_type_loc loc -> write_tag 0x0B; write_location loc
  | Method_parameter idx -> write_tag 0x0C; OutputStream.write_u1 st idx
  | Method_parameter_loc (idx, loc) -> write_tag 0x0D; OutputStream.write_u1 st idx; write_location loc
  | Field -> write_tag 0x0E
  | Field_loc loc -> write_tag 0x0F; write_location loc
  | Class_type_parameter_bound (idx, idx') -> write_tag 0x10; OutputStream.write_u1 st idx; OutputStream.write_u1 st idx'
  | Class_type_parameter_bound_loc (idx, idx', loc) -> write_tag 0x11; OutputStream.write_u1 st idx; OutputStream.write_u1 st idx'; write_location loc
  | Method_type_parameter_bound (idx, idx') -> write_tag 0x12; OutputStream.write_u1 st idx; OutputStream.write_u1 st idx'
  | Method_type_parameter_bound_loc (idx, idx', loc) -> write_tag 0x13; OutputStream.write_u1 st idx; OutputStream.write_u1 st idx'; write_location loc
  | Super_type idx -> write_tag 0x14; OutputStream.write_u2 st idx
  | Super_type_loc (idx, loc) -> write_tag 0x15; OutputStream.write_u2 st idx; write_location loc
  | Thrown_exception idx -> write_tag 0x16; OutputStream.write_u2 st idx
  | Type_argument_constructor_call (ofs, idx) -> write_tag 0x18; write_offset ofs; OutputStream.write_u1 st idx
  | Type_argument_constructor_call_loc (ofs, idx, loc) -> write_tag 0x18; write_offset ofs; OutputStream.write_u1 st idx; write_location loc
  | Type_argument_method_call (ofs, idx) -> write_tag 0x1A; write_offset ofs; OutputStream.write_u1 st idx
  | Type_argument_method_call_loc (ofs, idx, loc) -> write_tag 0x1B; write_offset ofs; OutputStream.write_u1 st idx; write_location loc
  | Wildcard_bound t' -> write_tag 0x1C; write_target st t'
  | Wildcard_bound_loc (t', loc) -> write_tag 0x1D; write_target st t'; write_location loc
  | Class_literal ofs -> write_tag 0x1E; write_offset ofs
  | Class_literal_loc (ofs, loc) -> write_tag 0x1F; write_offset ofs; write_location loc
  | Method_type_parameter idx -> write_tag 0x20; OutputStream.write_u1 st idx
  | Class_type_parameter idx -> write_tag 0x22; OutputStream.write_u1 st idx

let rec read_info_element_value st =
  let tag = UChar.of_code ((InputStream.read_u1 st) :> int) in
  let primitive p _ =
    let index = InputStream.read_u2 st in
    Primitive (p, index) in
  switch UChar.equal
    [ capital_b, primitive `Byte;
      capital_c, primitive `Char;
      capital_d, primitive `Double;
      capital_f, primitive `Float;
      capital_i, primitive `Int;
      capital_j, primitive `Long;
      capital_s, primitive `Short;
      capital_z, primitive `Boolean;
      small_s,
      (fun _ ->
        let index = InputStream.read_u2 st in
        String index);
      small_e,
      (fun _ ->
        let type_name_index = InputStream.read_u2 st in
        let const_name_index = InputStream.read_u2 st in
        Enum (type_name_index, const_name_index));
      small_c,
      (fun _ ->
        let class_info_index = InputStream.read_u2 st in
        Class class_info_index);
      at_character,
      (fun _ ->
        let annot = read_info st in
        Annotation annot);
      opening_square_bracket,
      (fun _ ->
        let num_values = InputStream.read_u2 st in
        let values = Array.init (num_values :> int) (fun _ -> read_info_element_value st) in
        Array (num_values, values)) ]
    (fun tag -> fail (Invalid_tag tag))
    tag
and read_info st =
  let type_idx = InputStream.read_u2 st in
  let nb = InputStream.read_u2 st in
  let evp =
    Array.init
      (nb :> int)
      (fun _ ->
        let name_index = InputStream.read_u2 st in
        let el_vl = read_info_element_value st in
        (name_index, el_vl)) in
  { type_index = type_idx;
    num_element_value_pairs = nb;
    element_value_pairs = evp; }
and read_extended_info st =
  let type_idx = InputStream.read_u2 st in
  let nb = InputStream.read_u2 st in
  let evp =
    Array.init
      (nb :> int)
      (fun _ ->
        let name_index = InputStream.read_u2 st in
        let el_vl = read_info_element_value st in
        (name_index, el_vl)) in
  let t = read_target st in
  { ext_type_index = type_idx;
    ext_num_element_value_pairs = nb;
    ext_element_value_pairs = evp;
    ext_target = t; }

let rec write_info_element_value st i =
  match i with
  | Primitive (jt, idx) ->
      let ch = match jt with
      | `Boolean -> capital_z
      | `Byte -> capital_b
      | `Char -> capital_c
      | `Double -> capital_d
      | `Float -> capital_f
      | `Int -> capital_i
      | `Long -> capital_l
      | `Short -> capital_s in
      OutputStream.write_u1 st (u1 (UChar.to_code ch));
      OutputStream.write_u2 st idx
  | String idx ->
      OutputStream.write_u1 st (u1 (UChar.to_code small_s));
      OutputStream.write_u2 st idx
  | Enum (n, v) ->
      OutputStream.write_u1 st (u1 (UChar.to_code small_e));
      OutputStream.write_u2 st n;
      OutputStream.write_u2 st v
  | Class idx ->
      OutputStream.write_u1 st (u1 (UChar.to_code small_c));
      OutputStream.write_u2 st idx
  | Annotation i ->
      OutputStream.write_u1 st (u1 (UChar.to_code at_character));
      write_info st i
  | Array (len, arr) ->
      OutputStream.write_u1 st (u1 (UChar.to_code opening_square_bracket));
      OutputStream.write_u2 st len;
      Array.iter (fun x -> write_info_element_value st x) arr
and write_info st i =
  OutputStream.write_u2 st i.type_index;
  OutputStream.write_u2 st i.num_element_value_pairs;
  Array.iter
    (fun (idx, iev) ->
      OutputStream.write_u2 st idx;
      write_info_element_value st iev)
    i.element_value_pairs
and write_extended_info st i =
  OutputStream.write_u2 st i.ext_type_index;
  OutputStream.write_u2 st i.ext_num_element_value_pairs;
  Array.iter
    (fun (idx, iev) ->
      OutputStream.write_u2 st idx;
      write_info_element_value st iev)
    i.ext_element_value_pairs;
  write_target st i.ext_target


(* High-level form *)

type element_value =
  | Boolean_value of bool
  | Byte_value of int
  | Char_value of UChar.t
  | Double_value of float
  | Float_value of float
  | Int_value of int32
  | Long_value of int64
  | Short_value of int
  | String_value of UTF8.t
  | Enum_value of Name.for_class * Name.for_field
  | Class_value of Name.for_class
  | Annotation_value of t
  | Array_value of element_value list
and t = Name.for_class * ((UTF8.t * element_value) list)
and extended = Name.for_class * ((UTF8.t * element_value) list) * target


(* Conversion functions *)

let rec decode_element_value pool i =
  match i with
  | Primitive (d, idx) ->
      (match (ConstantPool.get_entry pool idx), d with
      | (ConstantPool.Integer v), `Boolean ->
          Boolean_value (v <> 0l)
      | (ConstantPool.Integer v), `Byte ->
          Byte_value (Int32.to_int v)
      | (ConstantPool.Integer v), `Char ->
          Char_value (UChar.of_code (Int32.to_int v))
      | (ConstantPool.Double (hi, lo)), `Double ->
          let d = Int64.logor
              (Int64.shift_left (Int64.of_int32 hi) 32)
              (Int64.of_int32 lo) in
          Double_value (Int64.float_of_bits d)
      | (ConstantPool.Float f), `Float ->
          Float_value (Int32.float_of_bits f)
      | (ConstantPool.Integer v), `Int ->
          Int_value v
      | (ConstantPool.Long (hi, lo)), `Long ->
          let v = Int64.logor
              (Int64.shift_left (Int64.of_int32 hi) 32)
              (Int64.of_int32 lo) in
          Long_value v
      | (ConstantPool.Integer v), `Short ->
          Short_value (Int32.to_int v)
      | _ -> fail Inconsistent_primitive_value)
  | String idx ->
      (match ConstantPool.get_entry pool idx with
      | ConstantPool.UTF8 n -> String_value n
      | _ -> fail (Invalid_string_value idx))
  | Enum (name_idx, value_idx) ->
      (match (ConstantPool.get_entry pool name_idx),
        (ConstantPool.get_entry pool value_idx) with
      | (ConstantPool.UTF8 n), (ConstantPool.UTF8 v) ->
          (match Descriptor.field_of_utf8 n with
          | `Class cn ->
              Enum_value (cn, (Name.make_for_field v))
          | _ -> fail (Invalid_enum_value (name_idx, value_idx)))
      | _ -> fail (Invalid_enum_value (name_idx, value_idx)))
  | Class idx ->
      (match ConstantPool.get_entry pool idx with
      | ConstantPool.UTF8 n ->
          (match Descriptor.java_type_of_internal_utf8 n with
          | `Class x -> Class_value x
          | _ -> fail (Invalid_class_value idx))
      | _ -> fail (Invalid_class_value idx))
  | Annotation i ->
      Annotation_value (decode pool i)
  | Array (_, arr) ->
      Array_value (List.map (decode_element_value pool) (Array.to_list arr))
and decode pool i =
    let type_desc = match ConstantPool.get_entry pool i.type_index with
    | ConstantPool.UTF8 n ->
        (match Descriptor.java_type_of_internal_utf8 n with
        | `Class c -> c
        | _ -> fail (Invalid_annotation_type_value i.type_index))
    | _ -> fail (Invalid_annotation_type_value i.type_index) in
    let pairs = List.map (fun (idx, iev) ->
      let name = match ConstantPool.get_entry pool idx with
      | ConstantPool.UTF8 n -> n
      | _ -> fail (Invalid_element_name idx) in
      (name, (decode_element_value pool iev)))
        (Array.to_list i.element_value_pairs) in
    (type_desc, pairs)

let decode_extended pool i =
  let x, y =
    decode
      pool
      { type_index = i.ext_type_index;
        num_element_value_pairs = i.ext_num_element_value_pairs;
        element_value_pairs = i.ext_element_value_pairs; } in
  x, y, i.ext_target

let rec encode_element_value pool e =
  match e with
  | Boolean_value v ->
      let idx = ConstantPool.add_integer pool (if v then 1l else 0l) in
      Primitive (`Boolean, idx)
  | Byte_value v ->
      let idx = ConstantPool.add_integer pool (Int32.of_int v) in
      Primitive (`Byte, idx)
  | Char_value v ->
      let idx = ConstantPool.add_integer pool (Int32.of_int (UChar.to_code v)) in
      Primitive (`Char, idx)
  | Double_value v ->
      let idx = ConstantPool.add_double pool v in
      Primitive (`Double, idx)
  | Float_value v ->
      let idx = ConstantPool.add_float pool v in
      Primitive (`Float, idx)
  | Int_value v ->
      let idx = ConstantPool.add_integer pool v in
      Primitive (`Int, idx)
  | Long_value v ->
      let idx = ConstantPool.add_long pool v in
      Primitive (`Long, idx)
  | Short_value v ->
      let idx = ConstantPool.add_integer pool (Int32.of_int v) in
      Primitive (`Short, idx)
  | String_value v ->
      let idx = ConstantPool.add_string pool v in
      String idx
  | Enum_value (n, v) ->
      let n' = (Descriptor.utf8_of_field (`Class n)) in
      let n_idx = ConstantPool.add_utf8 pool n' in
      let v_idx = ConstantPool.add_utf8 pool (Name.utf8_for_field v) in
      Enum (n_idx, v_idx)
  | Class_value n ->
      let desc = Descriptor.internal_utf8_of_java_type (`Class n) in
      let idx = ConstantPool.add_utf8 pool desc in
      Class idx
  | Annotation_value a ->
      Annotation (encode pool a)
  | Array_value l ->
      let arr = Array.of_list (List.map (encode_element_value pool) l) in
      Array (u2 (Array.length arr), arr)
and encode pool e =
  let (field, pairs) = e in
  let field_value = Descriptor.utf8_of_field (`Class field) in
  let field_index = ConstantPool.add_utf8 pool field_value in
  let pairs' = List.map (fun (n, v) ->
    let n_idx = ConstantPool.add_utf8 pool n in
    (n_idx, (encode_element_value pool v))) pairs in
  { type_index = field_index;
    num_element_value_pairs = checked_length pairs;
    element_value_pairs = Array.of_list pairs'; }

let encode_extended pool e =
  let x, y, z = e in
  let i = encode pool (x, y) in
  { ext_type_index = i.type_index;
    ext_num_element_value_pairs = i.num_element_value_pairs;
    ext_element_value_pairs = i.element_value_pairs;
    ext_target = z; }
