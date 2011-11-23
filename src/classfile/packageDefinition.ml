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


(* Type *)

type t = {
    access_flags : AccessFlag.for_package list;
    name : Name.for_package;
    attributes : Attribute.for_package list;
  }


(* Exception *)

type error =
  | Invalid_package_name
  | Too_many of string
  | Version_error of Version.error
  | Invalid_package_definition

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_package_name -> "invalid package name"
  | Too_many s -> "too many " ^ s
  | Version_error e -> Version.string_of_error e
  | Invalid_package_definition -> "invalid package definition"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Conversion functions *)

let suffix = UTF8.of_string "/package-info"

let suffix_length = UTF8.length suffix

let decode cf =
  let pool = cf.ClassFile.constant_pool in
  let get_package_name idx = match ConstantPool.get_entry pool idx with
  | ConstantPool.UTF8 n ->
      let len = UTF8.length n in
      if len >= suffix_length
          && (UTF8.equal (UTF8.substring n (len - suffix_length) (pred len)) suffix) then
        Name.make_for_package_from_internal n
      else
        fail Invalid_package_name
  | _ -> fail Invalid_package_name in
  let flags = AccessFlag.check_package_flags (AccessFlag.from_u2 false cf.ClassFile.access_flags) in
  if (cf.ClassFile.major_version :> int) >= 49
      && cf.ClassFile.super_class = u2 0
      && cf.ClassFile.interfaces_count = u2 0
      && cf.ClassFile.interfaces = [||]
      && cf.ClassFile.fields_count = u2 0
      && cf.ClassFile.fields = [||]
      && cf.ClassFile.methods_count = u2 0
      && cf.ClassFile.methods = [||] then
    let bsm = Bootstrap.make_methods () in
    let res =
      { access_flags = flags;
        name = get_package_name cf.ClassFile.this_class;
        attributes = Attribute.check_package_attributes
          (map_array_to_list (Attribute.decode Attribute.Package bsm pool)
             cf.ClassFile.attributes); } in
    assert (Bootstrap.is_empty bsm);
    res
  else
    fail Invalid_package_definition

let encode ?(version=Version.default) pd =
  let checked_length s arr =
    let res = Array.length arr in
    if res <= max_u2 then
      u2 res
    else
      fail (Too_many s) in
  (try
    List.iter
      (fun x -> Version.check (AccessFlag.version_bounds x) version)
      (pd.access_flags :> AccessFlag.t list);
    List.iter
      (fun x -> Version.check (Attribute.version_bounds x) version)
      (pd.attributes :> Attribute.t list);
  with Version.Exception e -> fail (Version_error e));
  let major, minor = Version.major_minor_of_version version in
  let pool = ConstantPool.make_extendable () in
  let name = (Name.internal_utf8_for_package pd.name) ++ suffix in
  let this_index = ConstantPool.add_string pool name in
  let bsm = Bootstrap.make_methods () in
  let atts = map_list_to_array (Attribute.encode bsm pool) (pd.attributes :> Attribute.t list) in
  assert (Bootstrap.is_empty bsm);
  let cpool = ConstantPool.to_pool pool in
  { ClassFile.magic = u4 magic_number;
    ClassFile.minor_version = minor;
    ClassFile.major_version = major;
    ClassFile.constant_pool_count = ConstantPool.size cpool;
    ClassFile.constant_pool = cpool;
    ClassFile.access_flags = AccessFlag.list_to_u2 (pd.access_flags :> AccessFlag.t list);
    ClassFile.this_class = this_index;
    ClassFile.super_class = u2 0;
    ClassFile.interfaces_count = u2 0;
    ClassFile.interfaces = [||];
    ClassFile.fields_count = u2 0;
    ClassFile.fields = [||];
    ClassFile.methods_count = u2 0;
    ClassFile.methods = [||];
    ClassFile.attributes_count = checked_length "attributes" atts;
    ClassFile.attributes = atts; }
