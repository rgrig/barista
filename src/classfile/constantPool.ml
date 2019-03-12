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


(* Types *)

type field_reference = Name.for_class * Name.for_field * Descriptor.for_field

type method_reference = Name.for_class * Name.for_method * Descriptor.for_method

type constructor_reference = Name.for_class * (Descriptor.for_parameter list)

type reference =
  | Reference_getField of field_reference
  | Reference_getStatic of field_reference
  | Reference_putField of field_reference
  | Reference_putStatic of field_reference
  | Reference_invokeVirtual of method_reference
  | Reference_invokeStatic of method_reference
  | Reference_invokeSpecial of method_reference
  | Reference_newInvokeSpecial of constructor_reference
  | Reference_invokeInterface of method_reference

type reference_kind =
  | REF_getField
  | REF_getStatic
  | REF_putField
  | REF_putStatic
  | REF_invokeVirtual
  | REF_invokeStatic
  | REF_invokeSpecial
  | REF_newInvokeSpecial
  | REF_invokeInterface

let int_of_reference_kind = function
  | REF_getField -> 1
  | REF_getStatic -> 2
  | REF_putField -> 3
  | REF_putStatic -> 4
  | REF_invokeVirtual -> 5
  | REF_invokeStatic -> 6
  | REF_invokeSpecial -> 7
  | REF_newInvokeSpecial -> 8
  | REF_invokeInterface -> 9

type element =
  | Class of u2
  | Fieldref of u2 * u2
  | Methodref of u2 * u2
  | InterfaceMethodref of u2 * u2
  | String of u2
  | Integer of int32
  | Float of int32
  | Long of int32 * int32
  | Double of int32 * int32
  | NameAndType of u2 * u2
  | UTF8 of UTF8.t
  | MethodHandle of reference_kind * u2
  | MethodType of u2
  | InvokeDynamic of u2 * u2
  | ModuleId of u2 * u2

let dummy_element =
  UTF8 (UTF8.of_string "Dummy-Constant-Pool-Entry")

type tag =
  | CONSTANT_Class
  | CONSTANT_Fieldref
  | CONSTANT_Methodref
  | CONSTANT_InterfaceMethodref
  | CONSTANT_String
  | CONSTANT_Integer
  | CONSTANT_Float
  | CONSTANT_Long
  | CONSTANT_Double
  | CONSTANT_NameAndType
  | CONSTANT_Utf8
  | CONSTANT_MethodHandle
  | CONSTANT_MethodType
  | CONSTANT_InvokeDynamic
  | CONSTANT_ModuleId

type t = element array


(* Exception *)

type error =
  | Invalid_reference_kind of u1
  | Invalid_tag of u1
  | Too_large of int
  | Invalid_reference
  | Reference_out_of_bounds of int * int
  | Dummy_access of u2
  | Malformed_Class_entry of u2
  | Malformed_Fieldref_entry of u2 * u2
  | Malformed_Methodref_entry of u2 * u2
  | Malformed_InterfaceMethodRef_entry of u2 * u2
  | Malformed_String_entry of u2
  | Malformed_NameAndType_entry of u2 * u2
  | Malformed_MethodHandle_entry of reference_kind * u2
  | Malformed_MethodType_entry of u2
  | Malformed_InvokeDynamic_entry of u2 * u2
  | Malformed_ModuleId_entry of u2 * u2
  | Unexpected_tag of int * int

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_reference_kind x ->
      Printf.sprintf "invalid constant pool reference kind (%d)" (x :> int)
  | Invalid_tag x ->
      Printf.sprintf "invalid constant pool tag (%d)" (x :> int)
  | Too_large x ->
      Printf.sprintf "constant pool is too large (%d)" x
  | Invalid_reference ->
      "invalid constant pool reference (0)"
  | Reference_out_of_bounds (idx, sz) ->
      Printf.sprintf "constant pool reference out of bounds (index %d, length %d)" idx sz
  | Dummy_access x ->
      Printf.sprintf "access to dummy element (index %d)" (x :> int)
  | Malformed_Class_entry x ->
      Printf.sprintf "malformed Class entry (index %d)" (x :> int)
  | Malformed_Fieldref_entry (x, y) ->
      Printf.sprintf "malformed Fieldref entry (indexes %d and %d)" (x :> int) (y :> int)
  | Malformed_Methodref_entry (x, y) ->
      Printf.sprintf "malformed Methodref entry (indexes %d and %d)" (x :> int) (y :> int)
  | Malformed_InterfaceMethodRef_entry (x, y) ->
      Printf.sprintf "malformed InterfaceMethodref entry (indexes %d and %d)" (x :> int) (y :> int)
  | Malformed_String_entry x ->
      Printf.sprintf "malformed String entry (index %d)" (x :> int)
  | Malformed_NameAndType_entry (x, y) ->
      Printf.sprintf "malformed NameAndType entry (indexes %d and %d)" (x :> int) (y :> int)
  | Malformed_MethodHandle_entry (x, y) ->
      Printf.sprintf "malformed MethodHandle entry (kind %d and index %d)" (int_of_reference_kind x) (y :> int)
  | Malformed_MethodType_entry x ->
      Printf.sprintf "malformed MethodType entry (index %d)" (x :> int)
  | Malformed_InvokeDynamic_entry (x, y) ->
      Printf.sprintf "malformed InvokeDynamic entry (indexes %d and %d)" (x :> int) (y :> int)
  | Malformed_ModuleId_entry (x, y) ->
      Printf.sprintf "malformed ModuleId entry (indexes %d and %d)" (x :> int) (y :> int)
  | Unexpected_tag (x, y) ->
      Printf.sprintf "expected tag %d, found tag %d" x y (* see tag_of_int *)

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* I/O functions *)

let element_size = function
  | Class _ -> 1
  | Fieldref _ -> 1
  | Methodref _ -> 1
  | InterfaceMethodref _ -> 1
  | String _ -> 1
  | Integer _ -> 1
  | Float _ -> 1
  | Long _ -> 2
  | Double _ -> 2
  | NameAndType _ -> 1
  | UTF8 _ -> 1
  | MethodHandle _ -> 1
  | MethodType _ -> 1
  | InvokeDynamic _ -> 1
  | ModuleId _ -> 1

let reference_kind_of_int = function
  | 1 -> REF_getField
  | 2 -> REF_getStatic
  | 3 -> REF_putField
  | 4 -> REF_putStatic
  | 5 -> REF_invokeVirtual
  | 6 -> REF_invokeStatic
  | 7 -> REF_invokeSpecial
  | 8 -> REF_newInvokeSpecial
  | 9 -> REF_invokeInterface
  | x -> fail (Invalid_reference_kind (u1 x))

let int_of_tag = function
  | CONSTANT_Class -> 7
  | CONSTANT_Fieldref -> 9
  | CONSTANT_Methodref -> 10
  | CONSTANT_InterfaceMethodref -> 11
  | CONSTANT_String -> 8
  | CONSTANT_Integer -> 3
  | CONSTANT_Float -> 4
  | CONSTANT_Long -> 5
  | CONSTANT_Double -> 6
  | CONSTANT_NameAndType -> 12
  | CONSTANT_Utf8 -> 1
  | CONSTANT_MethodHandle -> 15
  | CONSTANT_MethodType -> 16
  | CONSTANT_InvokeDynamic -> 18
  | CONSTANT_ModuleId -> 13

let u1_of_tag x =
  u1 (int_of_tag x)

let tag_of_int = function
  | 1 -> CONSTANT_Utf8
  | 3 -> CONSTANT_Integer
  | 4 -> CONSTANT_Float
  | 5 -> CONSTANT_Long
  | 6 -> CONSTANT_Double
  | 7 -> CONSTANT_Class
  | 8 -> CONSTANT_String
  | 9 -> CONSTANT_Fieldref
  | 10 -> CONSTANT_Methodref
  | 11 -> CONSTANT_InterfaceMethodref
  | 12 -> CONSTANT_NameAndType
  | 13 -> CONSTANT_ModuleId
  | 15 -> CONSTANT_MethodHandle
  | 16 -> CONSTANT_MethodType
  | 18 -> CONSTANT_InvokeDynamic
  | x -> fail (Invalid_tag (u1 x))

let tag_of_entry = function
  | Class _ -> CONSTANT_Class
  | Fieldref _ -> CONSTANT_Fieldref
  | Methodref _ -> CONSTANT_Methodref
  | InterfaceMethodref _ -> CONSTANT_InterfaceMethodref
  | String _ -> CONSTANT_String
  | Integer _ -> CONSTANT_Integer
  | Float _ -> CONSTANT_Float
  | Long _ -> CONSTANT_Long
  | Double _ -> CONSTANT_Double
  | NameAndType _ -> CONSTANT_NameAndType
  | UTF8 _ -> CONSTANT_Utf8
  | MethodHandle _ -> CONSTANT_MethodHandle
  | MethodType _ -> CONSTANT_MethodType
  | InvokeDynamic _ -> CONSTANT_InvokeDynamic
  | ModuleId _ -> CONSTANT_ModuleId

let read_element st =
  match tag_of_int ((InputStream.read_u1 st) :> int) with
  | CONSTANT_Utf8 ->
      let length = InputStream.read_u2 st in
      let bytes = InputStream.read_bytes st (length :> int) in
      UTF8 (UTF8.of_modified (UTF8.modified_of_bytes bytes))
  | CONSTANT_Integer ->
      let bytes = InputStream.read_s4 st in
      Integer (bytes :> int32)
  | CONSTANT_Float ->
      let bytes = InputStream.read_s4 st in
      Float (bytes :> int32)
  | CONSTANT_Long ->
      let high_bytes = InputStream.read_s4 st in
      let low_bytes = InputStream.read_s4 st in
      Long ((high_bytes :> int32), (low_bytes :> int32))
  | CONSTANT_Double ->
      let high_bytes = InputStream.read_s4 st in
      let low_bytes = InputStream.read_s4 st in
      Double ((high_bytes :> int32), (low_bytes :> int32))
  | CONSTANT_Class ->
      let name_index = InputStream.read_u2 st in
      Class name_index
  | CONSTANT_String ->
      let string_index = InputStream.read_u2 st in
      String string_index
  | CONSTANT_Fieldref ->
      let class_index = InputStream.read_u2 st in
      let name_and_type_index = InputStream.read_u2 st in
      Fieldref (class_index, name_and_type_index)
  | CONSTANT_Methodref ->
      let class_index = InputStream.read_u2 st in
      let name_and_type_index = InputStream.read_u2 st in
      Methodref (class_index, name_and_type_index)
  | CONSTANT_InterfaceMethodref ->
      let class_index = InputStream.read_u2 st in
      let name_and_type_index = InputStream.read_u2 st in
      InterfaceMethodref (class_index, name_and_type_index)
  | CONSTANT_NameAndType ->
      let name_index = InputStream.read_u2 st in
      let descriptor_index = InputStream.read_u2 st in
      NameAndType (name_index, descriptor_index)
  | CONSTANT_MethodHandle ->
      let reference_kind = InputStream.read_u1 st in
      let reference_index = InputStream.read_u2 st in
      MethodHandle (reference_kind_of_int (reference_kind :> int), reference_index)
  | CONSTANT_MethodType ->
      let descriptor_index = InputStream.read_u2 st in
      MethodType descriptor_index
  | CONSTANT_InvokeDynamic ->
      let bootstrap_method_attr_index = InputStream.read_u2 st in
      let name_and_type_index = InputStream.read_u2 st in
      InvokeDynamic (bootstrap_method_attr_index, name_and_type_index)
  | CONSTANT_ModuleId ->
      let name_index = InputStream.read_u2 st in
      let version_index = InputStream.read_u2 st in
      ModuleId (name_index, version_index)

let read st sz =
  let sz = (sz : u2 :> int) in
  if (sz >= 0) && (sz <= max_u2) then
    let res = Array.make sz dummy_element in
    let i = ref 1 in
    while !i < sz do
      let e = read_element st in
      res.(!i) <- e;
      i := !i + (element_size e)
    done;
    res
  else
    fail (Too_large sz)

let write_element st e =
  if e != dummy_element then
    match e with
    | Class x ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Class);
        OutputStream.write_u2 st x
    | Fieldref (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Fieldref);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y
    | Methodref (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Methodref);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y
    | InterfaceMethodref (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_InterfaceMethodref);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y
    | String x ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_String);
        OutputStream.write_u2 st x
    | Integer x ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Integer);
        OutputStream.write_s4 st (s4 x)
    | Float x ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Float);
        OutputStream.write_s4 st (s4 x)
    | Long (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Long);
        OutputStream.write_s4 st (s4 x);
        OutputStream.write_s4 st (s4 y)
    | Double (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Double);
        OutputStream.write_s4 st (s4 x);
        OutputStream.write_s4 st (s4 y)
    | NameAndType (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_NameAndType);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y
    | UTF8 x ->
        let bytes = UTF8.bytes_of_modified (UTF8.to_modified x) in
        OutputStream.write_u1 st (u1_of_tag CONSTANT_Utf8);
        OutputStream.write_u2 st (u2 (String.length bytes));
        OutputStream.write_bytes st bytes
    | MethodHandle (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_MethodHandle);
        OutputStream.write_u1 st (u1 (int_of_reference_kind x));
        OutputStream.write_u2 st y
    | MethodType x ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_MethodType);
        OutputStream.write_u2 st x
    | InvokeDynamic (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_InvokeDynamic);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y
    | ModuleId (x, y) ->
        OutputStream.write_u1 st (u1_of_tag CONSTANT_ModuleId);
        OutputStream.write_u2 st x;
        OutputStream.write_u2 st y

let write st pool =
  let len = Array.length pool in
  if len <= max_u2 then
    Array.iter (write_element st) pool
  else
    fail (Too_large len)


(* Check functions *)

let size pool =
  u2 (Array.length pool)

let get_entry pool i =
  let i = (i : u2 :> int) in
  if i = 0 then
    fail Invalid_reference
  else if (i < 0) || (i >= (Array.length pool)) then
    fail (Reference_out_of_bounds (i, Array.length pool))
  else
    let res = pool.(i) in
    if res == dummy_element then
      fail (Dummy_access (u2 i))
    else
      res

(* used only by the next few functions *)
let unexpected_tag et entry =
  let ei = int_of_tag et in
  let fi = int_of_tag (tag_of_entry entry) in
  Unexpected_tag (ei, fi)

let get_utf8_entry pool i =
  match get_entry pool i with
    | UTF8 n -> n
    | e -> fail (unexpected_tag CONSTANT_Utf8 e)

let get_class_name pool i =
  match get_entry pool i with
    | Class n -> Name.make_for_class_from_internal (get_utf8_entry pool n)
    | e -> fail (unexpected_tag CONSTANT_Class e)

let check pool =
  let get_entry = get_entry pool in
  let check_entry = function
    | Class name_index ->
        (match get_entry name_index with
        | UTF8 _ -> ()
        | _ -> fail (Malformed_Class_entry name_index))
    | Fieldref (class_index, name_and_type_index) ->
        (match (get_entry class_index), (get_entry name_and_type_index) with
        | (Class _), (NameAndType _) -> ()
        | _ -> fail (Malformed_Fieldref_entry (class_index, name_and_type_index)))
    | Methodref (class_index, name_and_type_index) ->
        (match (get_entry class_index), (get_entry name_and_type_index) with
        | (Class _), (NameAndType _) -> ()
        | _ -> fail (Malformed_Methodref_entry (class_index, name_and_type_index)))
    | InterfaceMethodref (class_index, name_and_type_index) ->
        (match (get_entry class_index), (get_entry name_and_type_index) with
        | (Class _), (NameAndType _) -> ()
        | _ -> fail (Malformed_InterfaceMethodRef_entry (class_index, name_and_type_index)))
    | String value_index ->
        (match get_entry value_index with
        | UTF8 _ -> ()
        | _ -> fail (Malformed_String_entry value_index))
    | Integer _ -> ()
    | Float _ -> ()
    | Long _ -> ()
    | Double _ -> ()
    | NameAndType (name_index, desc_index) ->
        (match (get_entry name_index), (get_entry desc_index) with
        | (UTF8 _), (UTF8 _) -> ()
        | _ -> fail (Malformed_NameAndType_entry (name_index, desc_index)))
    | UTF8 _ -> ()
    | MethodHandle (reference_kind, reference_index) ->
        (match reference_kind, (get_entry reference_index) with
        | REF_getField, (Fieldref _)
        | REF_getStatic, (Fieldref _)
        | REF_putField, (Fieldref _)
        | REF_putStatic, (Fieldref _)
        | REF_invokeVirtual, (Methodref _)
        | REF_invokeStatic, (Methodref _)
        | REF_invokeSpecial, (Methodref _)
        | REF_newInvokeSpecial, (Methodref _)
        | REF_invokeInterface, (InterfaceMethodref _) -> ()
        | _ -> fail (Malformed_MethodHandle_entry (reference_kind, reference_index)))
    | MethodType type_index ->
        (match get_entry type_index with
        | UTF8 _ -> ()
        | _ -> fail (Malformed_MethodType_entry type_index))
    | InvokeDynamic (index, name_and_type_index) ->
        (match get_entry name_and_type_index with
        | NameAndType _ -> ()
        | _ -> fail (Malformed_InvokeDynamic_entry (index, name_and_type_index)))
    | ModuleId (name_index, vers_index) ->
        (match (get_entry name_index), (get_entry vers_index) with
        | (UTF8 _), (UTF8 _) -> ()
        | _ -> fail (Malformed_ModuleId_entry (name_index, vers_index))) in
  Array.iter check_entry pool

let check_entry_for_kind cpool idx tag =
  try tag = tag_of_entry (get_entry cpool idx) with _ -> false

let version_bounds = function
  | Class _ ->
      Version.make_bounds "'Class' constant pool element" Version.Java_1_0 None
  | Fieldref _ ->
      Version.make_bounds "'Fieldref' constant pool element" Version.Java_1_0 None
  | Methodref _ ->
      Version.make_bounds "'Methodref' constant pool element" Version.Java_1_0 None
  | InterfaceMethodref _ ->
      Version.make_bounds "'InterfaceMethodref' constant pool element" Version.Java_1_0 None
  | String _ ->
      Version.make_bounds "'String' constant pool element" Version.Java_1_0 None
  | Integer _ ->
      Version.make_bounds "'Integer' constant pool element" Version.Java_1_0 None
  | Float _ ->
      Version.make_bounds "'Float' constant pool element" Version.Java_1_0 None
  | Long _ ->
      Version.make_bounds "'Long' constant pool element" Version.Java_1_0 None
  | Double _ ->
      Version.make_bounds "'Double' constant pool element" Version.Java_1_0 None
  | NameAndType _ ->
      Version.make_bounds "'NameAndType' constant pool element" Version.Java_1_0 None
  | UTF8 _ ->
      Version.make_bounds "'UTF8' constant pool element" Version.Java_1_0 None
  | MethodHandle _ ->
      Version.make_bounds "'MethodHandle' constant pool element" Version.Java_7 None
  | MethodType _ ->
      Version.make_bounds "'MethodType' constant pool element" Version.Java_7 None
  | InvokeDynamic _ ->
      Version.make_bounds "'InvokeDynamic' constant pool element" Version.Java_7 None
  | ModuleId _ ->
      Version.make_bounds "'ModuleId' constant pool element" Version.Java_8 None

let check_version v pool =
  for i = 1 to pred (Array.length pool) do
    Version.check (version_bounds pool.(i)) v
  done


(* Extendable pools *)

type extendable = element ExtendableArray.t

let make_extendable () =
  ExtendableArray.make 1 dummy_element

let make_extendable_from_pool pool =
  let error = Too_large (Array.length pool) in
  ExtendableArray.from_array (Exception error) pool

let get_extendable_entry pool i =
  let i' = (i : u2 :> int) in
  let len = ExtendableArray.length pool in
  if i' = 0 then
    fail Invalid_reference
  else if (i' < 0) || (i' >= len) then
    fail (Reference_out_of_bounds (i', len))
  else
    let res = ExtendableArray.get pool i in
    if res == dummy_element then
      fail (Dummy_access i)
    else
      res

(* NOTE: This relies on UTF8.equal being the same as =. *)
let add_if_not_found ext elem =
  try ExtendableArray.index elem ext
  with Not_found -> begin
    let e = Exception (Too_large max_u2) in
    let r = ExtendableArray.add e ext elem in
    for i = 2 to element_size elem do
      ignore (ExtendableArray.add e ext dummy_element)
    done;
    r
  end

let add_utf8 ext s =
  let elem = UTF8 s in
  add_if_not_found ext elem

let add_class ext n =
  let name_index = add_utf8 ext (Name.internal_utf8_for_class n) in
  let elem = Class name_index in
  add_if_not_found ext elem

let add_array_class ext d =
  let name_index = add_utf8 ext (Descriptor.internal_utf8_of_java_type (d :> Descriptor.java_type)) in
  let elem = Class name_index in
  add_if_not_found ext elem

let add_name_and_type ext n t =
  let name_index = add_utf8 ext n in
  let type_index = add_utf8 ext t in
  let elem = NameAndType (name_index, type_index) in
  add_if_not_found ext elem

let add_moduleid ext n v =
  let name_index = add_utf8 ext n in
  let version_index = add_utf8 ext v in
  let elem = ModuleId (name_index, version_index) in
  add_if_not_found ext elem

let add_field ext cn n t =
  let class_index = add_class ext cn in
  let d = Descriptor.utf8_of_field t in
  let name_and_type_index = add_name_and_type ext (Name.utf8_for_field n) d in
  let elem = Fieldref (class_index, name_and_type_index) in
  add_if_not_found ext elem

let add_method ext cn n t =
  let class_index = add_class ext cn in
  let d = Descriptor.utf8_of_method t in
  let name_and_type_index = add_name_and_type ext (Name.utf8_for_method n) d in
  let elem = Methodref (class_index, name_and_type_index) in
  add_if_not_found ext elem

let add_interface_method ext cn n t =
  let class_index = add_class ext cn in
  let d = Descriptor.utf8_of_method t in
  let name_and_type_index = add_name_and_type ext (Name.utf8_for_method n) d in
  let elem = InterfaceMethodref (class_index, name_and_type_index) in
  add_if_not_found ext elem

let add_array_method ext at n t =
  let class_index = add_array_class ext at in
  let d = Descriptor.utf8_of_method t in
  let name_and_type_index = add_name_and_type ext (Name.utf8_for_method n) d in
  let elem = Methodref (class_index, name_and_type_index) in
  add_if_not_found ext elem

let add_string ext s =
  let v = add_utf8 ext s in
  let elem = String v in
  add_if_not_found ext elem

let add_integer ext i =
  let elem = Integer i in
  add_if_not_found ext elem

let add_float ext f =
  let elem = Float (Int32.bits_of_float f) in
  add_if_not_found ext elem

let add_long ext l =
  let high = Int64.to_int32 (Int64.shift_right_logical l 32) in
  let low = Int64.to_int32 (Int64.logand l 0x00000000FFFFFFFFL) in
  let elem = Long (high, low) in
  add_if_not_found ext elem

let add_double ext d =
  let l = Int64.bits_of_float d in
  let high = Int64.to_int32 (Int64.shift_right_logical l 32) in
  let low = Int64.to_int32 (Int64.logand l 0x00000000FFFFFFFFL) in
  let elem = Double (high, low) in
  add_if_not_found ext elem

let add_method_handle ext r =
  let elem = match r with
  | Reference_getField (cn, fn, fd) ->
      let reference_index = add_field ext cn fn fd in
      MethodHandle (REF_getField, reference_index)
  | Reference_getStatic (cn, fn, fd) ->
      let reference_index = add_field ext cn fn fd in
      MethodHandle (REF_getStatic, reference_index)
  | Reference_putField (cn, fn, fd) ->
      let reference_index = add_field ext cn fn fd in
      MethodHandle (REF_putField, reference_index)
  | Reference_putStatic (cn, fn, fd) ->
      let reference_index = add_field ext cn fn fd in
      MethodHandle (REF_putStatic, reference_index)
  | Reference_invokeVirtual (cn, mn, mt) ->
      let reference_index = add_method ext cn mn mt in
      MethodHandle (REF_invokeVirtual, reference_index)
  | Reference_invokeStatic (cn, mn, mt) ->
      let reference_index = add_method ext cn mn mt in
      MethodHandle (REF_invokeStatic, reference_index)
  | Reference_invokeSpecial (cn, mn, mt) ->
      let reference_index = add_method ext cn mn mt in
      MethodHandle (REF_invokeSpecial, reference_index)
  | Reference_newInvokeSpecial (cn, pl) ->
      let mn = Name.make_for_method class_constructor in
      let mt = pl, (`Class cn) in
      let reference_index = add_method ext cn mn mt in
      MethodHandle (REF_newInvokeSpecial, reference_index)
  | Reference_invokeInterface (cn, mn, mt) ->
      let reference_index = add_interface_method ext cn mn mt in
      MethodHandle (REF_invokeInterface, reference_index) in
  add_if_not_found ext elem

let add_method_type ext t =
  let d = Descriptor.utf8_of_method t in
  let type_index = add_utf8 ext d in
  let elem = MethodType type_index in
  add_if_not_found ext elem

let add_invoke_dynamic ext index mn md =
  let n = Name.utf8_for_method mn in
  let t = Descriptor.utf8_of_method md in
  let name_and_type_index = add_name_and_type ext n t in
  let elem = InvokeDynamic (index, name_and_type_index) in
  add_if_not_found ext elem

let to_pool ext =
  ExtendableArray.to_array ext
