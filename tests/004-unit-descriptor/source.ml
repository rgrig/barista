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

open BaristaLibrary
open Descriptor

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let fail_if' x y =
  if not (Utils.UTF8.equal x y) then fail ()

let u = Utils.UTF8.of_string

let primitive_tests () =
  print_string "primitive tests ... ";
  if is_primitive (`Array `Byte) then fail ();
  if is_primitive (`Class (Name.make_for_class_from_external (u "a.b.Cls"))) then fail ();
  if not (is_primitive `Long) then fail ();
  print_endline "OK"

let util_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (internal_utf8_of_java_type (java_type_of_internal_utf8 s')) in
  print_string "util tests ... ";
  ignore (filter_void Void_not_allowed `Char);
  (try
    ignore (filter_void Void_not_allowed `Void);
    fail ();
  with Exception _ -> ());
  id "La/b/Cls;";
  id "Z";
  id "[[I";
  fail_if' (u "[C") (internal_utf8_of_java_type (`Array `Char));
  fail_if' (u "La/b/Cls;") (internal_utf8_of_java_type (`Class (Name.make_for_class_from_external (u "a.b.Cls"))));
  print_endline "OK"

let field_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (utf8_of_field (field_of_utf8 s')) in
  print_string "field tests ... ";
  id "La/b/Cls;";
  id "Z";
  id "[[I";
  (try id "V"; fail () with Exception _ -> ());
  print_endline "OK"

let method_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (utf8_of_method (method_of_utf8 s')) in
  print_string "method tests ... ";
  id "(IFLa/b/Cls;D)V";
  id "(I[F)I";
  id "([[I[[I)[[I";
  (try id "(FV)I"; fail () with Exception _ -> ());
  print_endline "OK"

let () =
  primitive_tests ();
  util_tests ();
  field_tests ();
  method_tests ()
