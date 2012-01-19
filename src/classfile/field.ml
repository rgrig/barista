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


(* Low-level form *)

type info = {
    access_flags : u2;
    name_index : u2;
    descriptor_index : u2;
    attributes_count : u2;
    attributes_array : Attribute.info array;
  }


(* Exception *)

BARISTA_ERROR =
  | Invalid_name of (n : UTF8.t) ->
      Printf.sprintf "invalid name %S" (UTF8.to_string_noerr n)
  | Invalid_name_value of (i : u2) ->
      Printf.sprintf "invalid name value (at index %d)" (i :> int)
  | Invalid_descriptor_value of (i : u2) ->
      Printf.sprintf "invalid descriptor value (at index %d)" (i :> int)


(* I/O functions *)

let read_info st =
  let flags = InputStream.read_u2 st in
  let name = InputStream.read_u2 st in
  let desc = InputStream.read_u2 st in
  let att_count = InputStream.read_u2 st in
  let atts =
    Array.init
      (att_count :> int)
      (fun _ -> Attribute.read_info st) in
  { access_flags = flags;
    name_index = name;
    descriptor_index = desc;
    attributes_count = att_count;
    attributes_array = atts; }

let write_info st i =
  OutputStream.write_u2 st i.access_flags;
  OutputStream.write_u2 st i.name_index;
  OutputStream.write_u2 st i.descriptor_index;
  OutputStream.write_u2 st i.attributes_count;
  Array.iter (Attribute.write_info st) i.attributes_array


(* High-level form *)

type t = {
    flags : AccessFlag.for_field list; 
    name : Name.for_field;
    descriptor : Descriptor.for_field;
    attributes : Attribute.for_field list;
  }


(* Conversion functions *)

let decode itf pool i =
  let flags =
    AccessFlag.check_field_flags
      itf
      (AccessFlag.from_u2 false i.access_flags) in
  let name = match ConstantPool.get_entry pool i.name_index with
  | ConstantPool.UTF8 n ->
      if Name.is_valid_unqualified n then
        n
      else
        fail (Invalid_name n)
  | _ -> fail (Invalid_name_value i.name_index) in
  let desc = match ConstantPool.get_entry pool i.descriptor_index with
  | ConstantPool.UTF8 d -> Descriptor.field_of_utf8 d
  | _ -> fail (Invalid_descriptor_value i.descriptor_index) in
  let bsm = Bootstrap.make_methods () in
  let attrs = map_array_to_list (Attribute.decode Attribute.Field bsm pool) i.attributes_array in
  assert (Bootstrap.is_empty bsm);
  { flags = flags;
    name = Name.make_for_field name;
    descriptor = desc;
    attributes = Attribute.check_field_attributes attrs; }

let encode pool f =
  let acc_flags = AccessFlag.list_to_u2 (f.flags :> AccessFlag.t list) in
  let name_idx = ConstantPool.add_utf8 pool (Name.utf8_for_field f.name) in
  let desc_utf8 = Descriptor.utf8_of_field f.descriptor in
  let desc_idx = ConstantPool.add_utf8 pool desc_utf8 in
  let bsm = Bootstrap.make_methods () in
  let res =
    { access_flags = acc_flags;
      name_index = name_idx;
      descriptor_index = desc_idx;
      attributes_count = u2 (List.length f.attributes);
      attributes_array = map_list_to_array (Attribute.encode bsm pool) (f.attributes :> Attribute.t list); } in
  assert (Bootstrap.is_empty bsm);
  res

let compare x y =
  let cmp =
    AccessFlag.list_compare
      (x.flags :> AccessFlag.t list)
      (y.flags :> AccessFlag.t list) in
  if cmp <> 0 then
    cmp
  else
    let cmp2 =
      UTF8.compare
        (Name.utf8_for_field x.name)
        (Name.utf8_for_field y.name) in
    if cmp2 <> 0 then
      cmp2
    else
      Pervasives.compare x y
