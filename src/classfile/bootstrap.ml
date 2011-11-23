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


(* Base types and function *)

type method_handle =
  [ `getField of ConstantPool.field_reference
  | `getStatic of ConstantPool.field_reference
  | `putField of ConstantPool.field_reference
  | `putStatic of ConstantPool.field_reference
  | `invokeVirtual of ConstantPool.method_reference
  | `invokeStatic of ConstantPool.method_reference
  | `invokeSpecial of ConstantPool.method_reference
  | `newInvokeSpecial of ConstantPool.constructor_reference
  | `invokeInterface of ConstantPool.method_reference ]

let equal_method_handle x y =
  let eq_field_ref (cn1, fn1, fd1) (cn2, fn2, fd2) =
    (Name.equal_for_class cn1 cn2)
      && (Name.equal_for_field fn1 fn2)
      && (Descriptor.equal_java_type
            (fd1 :> Descriptor.java_type)
            (fd2 :> Descriptor.java_type)) in
  let eq_method_ref (cn1, mn1, md1) (cn2, mn2, md2) =
    (Name.equal_for_class cn1 cn2)
      && (Name.equal_for_method mn1 mn2)
      && (Descriptor.equal_for_method
            (md1 :> Descriptor.for_method)
            (md2 :> Descriptor.for_method)) in
  let eq_constructor_ref (cn1, pl1) (cn2, pl2) =
    (Name.equal_for_class cn1 cn2)
      && (list_equal
            ~eq:Descriptor.equal_java_type
            (pl1 :> Descriptor.java_type list)
            (pl2 :> Descriptor.java_type list)) in
  match x, y with
  | `getField fr1, `getField fr2
  | `getStatic fr1, `getStatic fr2
  | `putField fr1, `putField fr2
  | `putStatic fr1, `putStatic fr2 -> eq_field_ref fr1 fr2
  | `invokeVirtual mr1, `invokeVirtual mr2
  | `invokeStatic mr1, `invokeStatic mr2
  | `invokeSpecial mr1, `invokeSpecial mr2
  | `invokeInterface mr1, `invokeInterface mr2 -> eq_method_ref mr1 mr2
  | `newInvokeSpecial cr1, `newInvokeSpecial cr2 -> eq_constructor_ref cr1 cr2
  | _ -> false

type method_argument =
  [ `String of UTF8.t
  | `Class of Name.for_class
  | `Integer of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `MethodHandle of method_handle
  | `MethodType of Descriptor.for_method ]

let equal_method_argument x y =
  match x, y with
  | `String s1, `String s2 -> UTF8.equal s1 s2
  | `Class cn1, `Class cn2 -> Name.equal_for_class cn1 cn2
  | `Integer i1, `Integer i2 -> i1 = i2
  | `Long l1, `Long l2 -> l1 = l2
  | `Float f1, `Float f2 -> f1 = f2
  | `Double d1, `Double d2 -> d1 = d2
  | `MethodHandle mh1, `MethodHandle mh2 -> equal_method_handle mh1 mh2
  | `MethodType mt1, `MethodType mt2 -> Descriptor.equal_for_method mt1 mt2
  | _ -> false

type method_specifier = method_handle * (method_argument list)

let equal_method_specifier (mh1, mal1) (mh2, mal2) =
  (equal_method_handle mh1 mh2)
    && (list_equal ~eq:equal_method_argument mal1 mal2)


(* Structure used for encoding *)

type methods = method_specifier ExtendableArray.t

type error =
  | Too_large of int

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Too_large x ->
      Printf.sprintf "bootstrap array is too large (%d)" x

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)

let dummy_element =
  `getField (Name.make_for_class_from_external (UTF8.of_string "dummy_package.DummyClass"),
             Name.make_for_field (UTF8.of_string "dummyField"),
             `Boolean),
  []

let make_methods () =
  ExtendableArray.make 0 128 dummy_element

let is_empty m =
  (ExtendableArray.length m) = 0

let add_method_specifier m ms =
  ExtendableArray.add_if_not_found
    (Exception (Too_large (ExtendableArray.length m)))
    (fun x ->
      (x != dummy_element) && (equal_method_specifier ms x))
    m
    ms
    dummy_element
    false

let add m ms =
  let _ =
    ExtendableArray.add
      (Exception (Too_large (ExtendableArray.length m)))
      m
      ms
      dummy_element
      false in
  ()
