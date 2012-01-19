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


(* Conversion functions *)

let decode itf bsm pool i =
  let name = match ConstantPool.get_entry pool i.name_index with
  | ConstantPool.UTF8 n ->
      if Name.is_valid_for_method n then
        n
      else
        fail (Invalid_name n)
  | _ -> fail (Invalid_name_value i.name_index) in
  let descriptor = match ConstantPool.get_entry pool i.descriptor_index with
   | ConstantPool.UTF8 d -> Descriptor.method_of_utf8 d
   | _ -> fail (Invalid_descriptor_value i.descriptor_index) in
   let attributes =
     Attribute.check_method_attributes
       (map_array_to_list
          (Attribute.decode Attribute.Method bsm pool)
          i.attributes_array) in
   switch
     UTF8.equal
     [ class_initializer,
       (fun _ ->
         let flags =
           AccessFlag.check_initializer_flags (AccessFlag.from_u2 true i.access_flags) in
         Initializer { init_flags = flags; init_attributes = attributes });

       class_constructor,
       (fun _ ->
         let flags =
           AccessFlag.check_constructor_flags (AccessFlag.from_u2 true i.access_flags) in
         Constructor { cstr_flags = flags; cstr_descriptor = fst descriptor; cstr_attributes = attributes }) ]
     (fun _ ->
       let flags =
         AccessFlag.check_method_flags itf (AccessFlag.from_u2 true i.access_flags) in
       let name = Name.make_for_method name in
       Regular { flags; name; descriptor; attributes })
     name

let encode bsm pool m =
  let flags, name, desc, attrs = match m with
  | Regular r ->
      r.flags,
      r.name,
      r.descriptor,
      r.attributes
  | Constructor c ->
      (c.cstr_flags :> AccessFlag.for_method list),
      (Name.make_for_method class_constructor),
      (c.cstr_descriptor, `Void),
      c.cstr_attributes
  | Initializer i ->
      (i.init_flags :> AccessFlag.for_method list),
      (Name.make_for_method class_initializer),
      ([], `Void),
      i.init_attributes in
  let acc_flags = AccessFlag.list_to_u2 (flags :> AccessFlag.t list) in
  let name_idx = ConstantPool.add_utf8 pool (Name.utf8_for_method name) in
  let desc_utf8 = Descriptor.utf8_of_method desc in
  let desc_idx = ConstantPool.add_utf8 pool desc_utf8 in
  { access_flags = acc_flags;
    name_index = name_idx;
    descriptor_index = desc_idx;
    attributes_count = u2 (List.length attrs);
    attributes_array = map_list_to_array (Attribute.encode bsm pool) (attrs :> Attribute.t list); }

let compare m1 m2 =
  let rank = function
    | Regular _ -> 2
    | Constructor _ -> 1
    | Initializer _ -> 0 in
  let r1 = rank m1 in
  let r2 = rank m2 in
  let cmp = compare r1 r2 in
  if cmp <> 0 then
    cmp
  else
    match m1, m2 with
    | Regular { flags = fl1; name = n1; descriptor = d1; _ },
      Regular { flags = fl2; name = n2; descriptor = d2; _ } ->
        let cmp' =
          AccessFlag.list_compare
            (fl1 :> AccessFlag.t list)
            (fl2 :> AccessFlag.t list) in
        if cmp' <> 0 then
          cmp'
        else
          let cmp'' = UTF8.compare (Name.utf8_for_method n1) (Name.utf8_for_method n2) in
          if cmp'' <> 0 then
            cmp''
          else
            Descriptor.compare_for_method d1 d2
    | Constructor { cstr_flags = fl1; cstr_descriptor = d1; _},
      Constructor { cstr_flags = fl2; cstr_descriptor = d2; _} ->
        let cmp' =
          AccessFlag.list_compare
            (fl1 :> AccessFlag.t list)
            (fl2 :> AccessFlag.t list) in
        if cmp' <> 0 then
          cmp'
        else
          Descriptor.compare_for_method (d1, `Void) (d2, `Void)
    | _ -> compare m1 m2
