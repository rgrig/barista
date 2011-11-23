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
open Signature

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let fail_if' x y =
  if not (Utils.UTF8.equal x y) then fail ()

let u = Utils.UTF8.of_string

let class_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (utf8_of_class_signature (class_signature_of_utf8 s')) in
  print_string "class tests ... ";
  id "<A:La/b/Cls;:La/b/Itf;>La/b/Cls;La/b/Itf;";
  id "<A:La/b/Cls;:La/b/Itf;B:Ljava/lang/Object;>La/b/Cls;La/b/Itf;La/b/Itf2;";
  print_endline "OK"

let field_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (utf8_of_field_type_signature (field_type_signature_of_utf8 s')) in
  print_string "field tests ... ";
  id "TX;";
  id "[TX;";
  id "La/b/Cls.A<*>;";
  print_endline "OK"

let method_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (utf8_of_method_signature (method_signature_of_utf8 s')) in
  print_string "method tests ... ";
  id "<A:Ljava/lang/Object;>(TA;[[TA;I)V^La/b/Exn;";
  id "<A:La/b/Cls;>(TA;[[TA;I)V^La/b/Exn;";
  id "<A:Ljava/lang/Object;B:La/b/Cls2;>(TA;TB;F)La/b/Cls;^La/b/Exn;";
  print_endline "OK"

let () =
  class_tests ();
  field_tests ();
  method_tests ()
