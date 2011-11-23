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
open Lexer

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let fail_if' x y =
  if not (Utils.UTF8.equal x y) then fail ()

let u = Utils.UTF8.of_string

let tokens_tests () =
  let id s l =
    let tokens = tokens_of_line (u s) in
    if List.exists2 (fun t t' -> not (equal t t')) tokens l then
      fail () in
  print_string "tokens tests ... ";
  id ".direct @attr" [Directive "direct"; Attribute "attr"];
  id "3.1_4 0xFF -1_2" [Float 3.14; Int 255L; Int (-12L)];
  id "lbl0123: \"s\\ntr\"" [Label (u "lbl0123:"); String (u "s\ntr")];
  id "pack.MyClass int[][] float => ident"
    [Class_name (Name.make_for_class_from_external (u "pack.MyClass"));
     Array_type (u "int[][]");
     Primitive_type (`Float);
     Arrow;
     Identifier (u "ident")];
  id "pack.MyClass.field: long"
    [Field ((Name.make_for_class_from_external (u "pack.MyClass")),
            (Name.make_for_field (u "field")),
            `Long)];
  id "meth(int[],int): boolean"
    [Dynamic_method ((Name.make_for_method (u "meth")),
                    ([`Array `Int; `Int], `Boolean))];
  id "pack.MyClass.meth(int[], int) :boolean[]"
    [Method ((Name.make_for_class_from_external (u "pack.MyClass")),
             (Name.make_for_method (u "meth")),
             ([`Array `Int; `Int], `Array `Boolean))];
  id "pack.MyClass[].meth(int[], int) :boolean[]"
    [Array_method ((`Array (`Class (Name.make_for_class_from_external (u "pack.MyClass")))),
                   (Name.make_for_method (u "meth")),
                   ([`Array `Int; `Int], `Array `Boolean))];
  id "meth(java.lang.String, int, int)"
    [Method_signature ((Name.make_for_method (u "meth")),
                       [`Class (Name.make_for_class_from_external (u "java.lang.String")); `Int; `Int])];
  print_endline "OK"

let () =
  tokens_tests ()
