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
    magic : u4;
    minor_version : u2;
    major_version : u2;
    constant_pool_count : u2;
    constant_pool : ConstantPool.t;
    access_flags : u2;
    this_class : u2;
    super_class : u2;
    interfaces_count : u2;
    interfaces : u2 array;
    fields_count : u2;
    fields : Field.info array;
    methods_count : u2;
    methods : Method.info array;
    attributes_count : u2;
    attributes : Attribute.info array;
  }


(* Exception *)

type error =
  | Invalid_magic_number of u4
  | Unsupported_version of u2 * u2
  | Invalid_this
  | Invalid_super
  | Invalid_super_interface
  | Invalid_parent_interface

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_magic_number m ->
      Printf.sprintf "invalid magic number (0x%08LX instead of 0x%08LX)" (m :> int64) magic_number
  | Unsupported_version (mj, mn) ->
      Printf.sprintf "unsupported class file version %d.%d" (mj :> int) (mn :> int)
  | Invalid_this -> "invalid 'this' entry for class"
  | Invalid_super -> "invalid 'super' entry for class"
  | Invalid_super_interface -> "invalid 'super' entry for interface"
  | Invalid_parent_interface -> "invalid parent interface"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* I/O functions *)

let check_entry_for_class_name cpool idx name =
  try
    match ConstantPool.get_entry cpool idx with
    | ConstantPool.Class idx' ->
        (match ConstantPool.get_entry cpool idx' with
        | ConstantPool.UTF8 s -> UTF8.equal s name
        | _ -> false)
    | _ -> false
  with _ -> false

let read st =
  let mgc = InputStream.read_u4 st in
  if (mgc :> int64) <> magic_number then
    fail (Invalid_magic_number mgc);
  let minor = InputStream.read_u2 st in
  let major = InputStream.read_u2 st in
  let version = (major, minor) in
  if (version < Version.min_supported) || (version > Version.max_supported) then
    fail (Unsupported_version (major, minor));
  let cpool_size = InputStream.read_u2 st in
  let cpool = ConstantPool.read st cpool_size in
  ConstantPool.check cpool;
  let access = InputStream.read_u2 st in
  let is_interface = List.mem `Interface (AccessFlag.from_u2 false access) in
  let this = InputStream.read_u2 st in
  if not (ConstantPool.check_entry_for_kind cpool this ConstantPool.CONSTANT_Class) then
    fail Invalid_this;
  let super = InputStream.read_u2 st in
  if (super :> int) = 0 then begin
    if not (check_entry_for_class_name cpool this java_lang_Object) then
      fail Invalid_super
  end else begin
    if not (ConstantPool.check_entry_for_kind cpool super ConstantPool.CONSTANT_Class) then
      fail Invalid_super;
    if is_interface && not (check_entry_for_class_name cpool super java_lang_Object) then
      fail Invalid_super_interface;
  end;
  let itf_count = InputStream.read_u2 st in
  let itfs =
    Array.init
      (itf_count :> int)
      (fun _ ->
        let res = InputStream.read_u2 st in
        if ConstantPool.check_entry_for_kind cpool res ConstantPool.CONSTANT_Class then
          res
        else
          fail Invalid_parent_interface) in
  let fld_count = InputStream.read_u2 st in
  let flds = Array.init (fld_count :> int) (fun _ -> Field.read_info st) in
  let mth_count = InputStream.read_u2 st in
  let mths = Array.init (mth_count :> int) (fun _ -> Method.read_info st) in
  let att_count = InputStream.read_u2 st in
  let atts = Array.init (att_count :> int) (fun _ -> Attribute.read_info st) in
  { magic = mgc;
    minor_version = minor;
    major_version = major;
    constant_pool_count = cpool_size;
    constant_pool = cpool;
    access_flags = access;
    this_class = this;
    super_class = super;
    interfaces_count = itf_count;
    interfaces = itfs;
    fields_count = fld_count;
    fields = flds;
    methods_count = mth_count;
    methods = mths;
    attributes_count = att_count;
    attributes = atts; }

let write cf st =
  OutputStream.write_u4 st cf.magic;
  OutputStream.write_u2 st cf.minor_version;
  OutputStream.write_u2 st cf.major_version;
  OutputStream.write_u2 st cf.constant_pool_count;
  ConstantPool.write st cf.constant_pool;
  OutputStream.write_u2 st cf.access_flags;
  OutputStream.write_u2 st cf.this_class;
  OutputStream.write_u2 st cf.super_class;
  OutputStream.write_u2 st cf.interfaces_count;
  Array.iter (OutputStream.write_u2 st) cf.interfaces;
  OutputStream.write_u2 st cf.fields_count;
  Array.iter (Field.write_info st) cf.fields;
  OutputStream.write_u2 st cf.methods_count;
  Array.iter (Method.write_info st) cf.methods;
  OutputStream.write_u2 st cf.attributes_count;
  Array.iter (Attribute.write_info st) cf.attributes
