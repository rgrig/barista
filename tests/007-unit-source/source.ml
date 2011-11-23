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

let type_tests () =
  let id s =
    let s' = u s in
    fail_if' s' (external_utf8_of_java_type (java_type_of_external_utf8 s'));
    fail_if' s' (external_utf8_of_java_type ((java_type_of_external_utf8_no_void s') :> java_type)) in
  print_string "type tests ... ";
  id "int";
  id "a.b.Cls";
  id "float[][]";
  (try
    ignore (java_type_of_external_utf8_no_void (u "void"));
    fail ()
  with Exception _ -> ());
  print_endline "OK"

let flag_tests () =
  print_string "flag tests ... ";
  fail_if `Public (AccessFlag.of_utf8 (u "public"));
  fail_if `Volatile (AccessFlag.of_utf8 (u "volatile"));
  fail_if' (u "public static final ") (AccessFlag.list_to_utf8 [`Public; `Static; `Final]);
  fail_if' (u "private transient ") (AccessFlag.list_to_utf8 [`Transient; `Private]);
  print_endline "OK"

let () =
  type_tests ();
  flag_tests ()
