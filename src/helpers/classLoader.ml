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


type t = {
    class_path : ClassPath.t;
    loaded_classes : ClassDefinition.t UTF8Hashtbl.t;
    loaded_packages : PackageDefinition.t UTF8Hashtbl.t;
    loaded_modules : ModuleDefinition.t UTF8Hashtbl.t;
  }

type error =
  | Unable_to_load of UTF8.t * string
  | Already_defined of UTF8.t

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unable_to_load (s, os) ->
      Printf.sprintf "unable to load class %S (%s)" (UTF8.to_string_noerr s) os
  | Already_defined s ->
      Printf.sprintf "class %S is already defined" (UTF8.to_string_noerr s)

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)

let make cp =
  { class_path = cp;
    loaded_classes = UTF8Hashtbl.create 17;
    loaded_packages = UTF8Hashtbl.create 17;
    loaded_modules = UTF8Hashtbl.create 17; }

let create_functions
    (get : t -> 'a Utils.UTF8Hashtbl.t)
    (suffix : string)
    (decode : ClassFile.t -> 'a)
    (name : 'a -> UTF8.t) =
  let find cl cn =
    try
      UTF8Hashtbl.find (get cl) cn
    with Not_found ->
      let stream = ClassPath.open_stream cl.class_path ((UTF8.to_string cn) ^ suffix) in
      try
        let cf = ClassFile.read stream in
        let cd = decode cf in
        UTF8Hashtbl.add (get cl) cn cd;
        cd
      with (ClassFile.Exception cause) ->
        InputStream.close_noerr stream;
        fail (Unable_to_load (cn, ClassFile.string_of_error cause))
      | e ->
          InputStream.close_noerr stream;
          fail (Unable_to_load (cn, Printexc.to_string e)) in
  let add cl cd =
    let cn = name cd in
    if UTF8Hashtbl.mem (get cl) cn then
      fail (Already_defined cn)
    else
      UTF8Hashtbl.add (get cl) cn cd in
  let mem cl cn =
    UTF8Hashtbl.mem (get cl) cn in
  let remove cl cn =
    UTF8Hashtbl.remove (get cl) cn in
  let replace cl cd =
    let cn = name cd in
    UTF8Hashtbl.replace (get cl) cn cd in
  find, add, mem, remove, replace

let find_class, add_class, mem_class, remove_class, replace_class =
  create_functions
    (fun cl -> cl.loaded_classes)
    ""
    ClassDefinition.decode
    (fun cd -> Name.external_utf8_for_class cd.ClassDefinition.name)

let find_package, add_package, mem_package, remove_package, replace_package =
  create_functions
    (fun cl -> cl.loaded_packages)
    "/package-info"
    PackageDefinition.decode
    (fun cd -> Name.external_utf8_for_package cd.PackageDefinition.name)

let find_module, add_module, mem_module, remove_module, replace_module =
  create_functions
    (fun cl -> cl.loaded_modules)
    "/module-info"
    ModuleDefinition.decode
    (fun cd -> Name.external_utf8_for_module cd.ModuleDefinition.name)
