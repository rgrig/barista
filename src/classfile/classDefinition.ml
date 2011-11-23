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
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : Method.t list;
    attributes : Attribute.for_class list;
  }


(* Exception *)

type error =
  | Invalid_class_name
  | Bootstrap_methods_defined_twice
  | Too_many of string
  | Version_error of Version.error

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_class_name -> "invalid class name"
  | Bootstrap_methods_defined_twice -> "bootstrap methods defined twice"
  | Too_many s -> "too many " ^ s
  | Version_error e -> Version.string_of_error e

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Traversal *)

let map mapper cd =
  let flags', name', extends', implements', fields', methods', attributes' =
    mapper#class_definition
      cd.access_flags
      cd.name
      cd.extends
      cd.implements
      cd.fields
      cd.methods
      cd.attributes in
  { access_flags = flags';
    name = name';
    extends = extends';
    implements = implements';
    fields = fields';
    methods = methods';
    attributes = attributes' }

let iter iterator cd =
  iterator#class_definition
    cd.access_flags
    cd.name
    cd.extends
    cd.implements
    cd.fields
    cd.methods
    cd.attributes

let version_check_iterator v =
  let check_flags =
    List.iter (fun x -> Version.check (AccessFlag.version_bounds x) v) in
  let check_attribute x =
    Version.check (Attribute.version_bounds x) v in
  let check_attributes =
    List.iter check_attribute in
  object
    inherit Traversal.default_class_definition_iterator
    method! class_flags l =
      check_flags (l :> AccessFlag.t list)
    method! class_field f =
      check_flags (f.Field.flags :> AccessFlag.t list);
      check_attributes (f.Field.attributes :> Attribute.t list)
    method! regular_method { Method.flags = f; attributes = a; _} =
      check_flags (f :> AccessFlag.t list);
      check_attributes (a :> Attribute.t list)
    method! constructor_method { Method.cstr_flags = l; cstr_attributes = a; _ } =
      check_flags (l :> AccessFlag.t list);
      check_attributes (a :> Attribute.t list)
    method! initializer_method { Method.init_flags = l; init_attributes = a; _ } =
      check_flags (l :> AccessFlag.t list);
      check_attributes (a :> Attribute.t list)
    method! class_attribute a =
      check_attribute (a :> Attribute.t)
  end


(* Conversion functions *)

let no_super_class = u2 0

let decode cf =
  let pool = cf.ClassFile.constant_pool in
  let version = cf.ClassFile.major_version, cf.ClassFile.minor_version in
  let version = Version.version_of_major_minor version in
  ConstantPool.check_version version pool;
  let get_class_name idx = match ConstantPool.get_entry pool idx with
  | ConstantPool.Class idx' ->
      (match ConstantPool.get_entry pool idx' with
      | ConstantPool.UTF8 n ->
          Name.make_for_class_from_internal n
      | _ -> fail Invalid_class_name)
  | _ -> fail Invalid_class_name in
  let flags = AccessFlag.check_class_flags (AccessFlag.from_u2 false cf.ClassFile.access_flags) in
  let is_interface = List.mem `Interface flags in
  let super_class =
    if cf.ClassFile.super_class = no_super_class then
      None
    else
      Some (get_class_name cf.ClassFile.super_class) in
  let itfs = map_array_to_list get_class_name cf.ClassFile.interfaces in
  let flds = map_array_to_list (Field.decode is_interface pool) cf.ClassFile.fields in
  let bsm = Bootstrap.make_methods () in
  let atts = map_array_to_list (Attribute.decode Attribute.Class bsm pool) cf.ClassFile.attributes in
  let bsm_info = try Attribute.extract_bootstrap_info atts with Not_found -> [] in
  List.iter (Bootstrap.add bsm) bsm_info;
  let mths = map_array_to_list (Method.decode is_interface bsm pool) cf.ClassFile.methods in
  let res =
    { access_flags = flags;
      name = get_class_name cf.ClassFile.this_class;
      extends = super_class;
      implements = itfs;
      fields = flds;
      methods = mths;
      attributes = Attribute.check_class_attributes atts; } in
  iter (version_check_iterator version) res;
  res

let encode ?(version=Version.default) cd =
  let checked_length s sz =
    if sz <= max_u2 then
      u2 sz
    else
      fail (Too_many s) in
  let checked_length_array s arr =
    let res = Array.length arr in
    checked_length s res in
  (try
    iter (version_check_iterator version) cd
  with Version.Exception e -> fail (Version_error e));
  let major, minor = Version.major_minor_of_version version in
  let pool = ConstantPool.make_extendable () in
  let this_index = ConstantPool.add_class pool cd.name in
  let super_index = match cd.extends with
  | Some n -> ConstantPool.add_class pool n
  | None -> no_super_class in
  let itfs = map_list_to_array (fun s -> ConstantPool.add_class pool s) cd.implements in
  let flds = map_list_to_array (Field.encode pool) cd.fields in
  let bsm = Bootstrap.make_methods () in
  let mths = map_list_to_array (Method.encode bsm pool) cd.methods in
  let bsm_length = ExtendableArray.length bsm in
  let atts = (cd.attributes :> Attribute.t list) in
  let bsm_meths = Array.to_list (ExtendableArray.to_array bsm) in
  let bsm_class =
    try
      Attribute.extract_bootstrap_info atts
    with Not_found ->
      [] in
  let bsm_atts = match bsm_meths, bsm_class with
  | [], [] -> []
  | _ :: _, [] -> bsm_meths
  | [], _ :: _ -> bsm_class
  | _ :: _,  _ :: _ -> fail Bootstrap_methods_defined_twice in
  let atts =
    if bsm_atts = [] then
      atts
    else
      atts @ [`BootstrapMethods bsm_atts] in
  let atts = map_list_to_array (Attribute.encode bsm pool) atts in
  assert (bsm_length = ExtendableArray.length bsm);
  let cpool = ConstantPool.to_pool pool in
  ConstantPool.check_version version cpool;
  { ClassFile.magic = u4 magic_number;
    ClassFile.minor_version = minor;
    ClassFile.major_version = major;
    ClassFile.constant_pool_count = ConstantPool.size cpool;
    ClassFile.constant_pool = cpool;
    ClassFile.access_flags = AccessFlag.list_to_u2 (cd.access_flags :> AccessFlag.t list);
    ClassFile.this_class = this_index;
    ClassFile.super_class = super_index;
    ClassFile.interfaces_count = checked_length_array "interfaces" itfs;
    ClassFile.interfaces = itfs;
    ClassFile.fields_count = checked_length_array "fields" flds;
    ClassFile.fields = flds;
    ClassFile.methods_count = checked_length_array "methods" mths;
    ClassFile.methods = mths;
    ClassFile.attributes_count = checked_length_array "attributes" atts;
    ClassFile.attributes = atts; }
