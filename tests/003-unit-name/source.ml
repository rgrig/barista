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

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let fail_if' x y =
  if not (Utils.UTF8.equal x y) then fail ()

let u = Utils.UTF8.of_string

let util_tests () =
  print_string "util tests ... ";
  fail_if' (u "a/b/c/e") (Name.replace_dot_with_slash (u "a.b.c/e"));
  fail_if' (u "a.b.c.e") (Name.replace_slash_with_dot (u "a/b/c.e"));
  if Name.is_valid_unqualified (u "") then fail ();
  if Name.is_valid_unqualified (u "abc.d") then fail ();
  if not (Name.is_valid_unqualified (u "abc")) then fail ();
  if Name.is_valid_for_method (u "") then fail ();
  if Name.is_valid_for_method (u "abc.d") then fail ();
  if Name.is_valid_for_method (u "abc<d") then fail ();
  if not (Name.is_valid_for_method (u "abc")) then fail ();
  print_endline "OK"

let class_tests () =
  print_string "class tests ... ";
  let c1 = Name.make_for_class_from_internal (u "pack/Class$Inner") in
  let c2 = Name.make_for_class_from_external (u "pack2.Class2$Inner2") in
  fail_if'
    (u "pack.Class.Inner")
    (Name.printable_utf8_for_class c1);
  fail_if'
    (u "pack2.Class2.Inner2")
    (Name.printable_utf8_for_class c2);
  fail_if'
    (u "pack.Class$Inner")
    (Name.external_utf8_for_class c1);
  fail_if'
    (u "pack2.Class2$Inner2")
    (Name.external_utf8_for_class c2);
  fail_if'
    (u "pack/Class$Inner")
    (Name.internal_utf8_for_class c1);
  fail_if'
    (u "pack2/Class2$Inner2")
    (Name.internal_utf8_for_class c2);
  try
    ignore (Name.make_for_class_from_internal (u "abc.Cls$i/j"));
    fail ()
  with Name.Exception _ -> ();
  try
    ignore (Name.make_for_class_from_external (u "abc.Cls$i/j"));
    fail ()
  with Name.Exception _ -> ();
  print_endline "OK"

let field_tests () =
  print_string "field tests ... ";
  fail_if'
    (u "abc")
    (Name.utf8_for_field (Name.make_for_field (u "abc")));
  try
    ignore (Name.make_for_field (u "a.b"));
    fail ()
  with Name.Exception _ -> ();
  print_endline "OK"

let method_tests () =
  print_string "method tests ... ";
  fail_if'
    (u "abc")
    (Name.utf8_for_method (Name.make_for_method (u "abc")));
  try
    ignore (Name.make_for_method (u "a.b"));
    fail ()
  with Name.Exception _ -> ();
  print_endline "OK"

let () =
  util_tests ();
  class_tests ();
  field_tests ();
  method_tests ()
