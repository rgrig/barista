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
open AccessFlag

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let util_tests () =
  let id meth l =
    let l' = from_u2 meth (list_to_u2 l) in
    let sort = Sort.list (fun x y -> compare x y <= 0) in
    fail_if (sort l) (sort l') in
  print_string "util tests ... ";
  id false [`Public; `Final; `Synthetic];
  id true [`Protected; `Static; `Varargs];
  id false [`Private; `Transient; `Volatile];
  print_endline "OK"

let class_tests () =
  print_string "class tests ... ";
  ignore (check_class_flags [`Public; `Super]);
  (try ignore (check_class_flags [`Public; `Volatile]); fail () with Exception _ -> ());
  print_endline "OK"

let inner_class_tests () =
  print_string "inner_class tests ... ";
  ignore (check_inner_class_flags [`Public; `Super]);
  (try ignore (check_inner_class_flags [`Public; `Volatile]); fail () with Exception _ -> ());
  print_endline "OK"

let field_tests () =
  print_string "field tests ... ";
  ignore (check_field_flags false [`Public; `Final]);
  (try ignore (check_field_flags false [`Public; `Varargs]); fail () with Exception _ -> ());
  print_endline "OK"

let method_tests () =
  print_string "method tests ... ";
  ignore (check_method_flags false [`Public; `Final; `Synthetic; `Synchronized]);
  (try ignore (check_method_flags false [`Public; `Volatile]); fail () with Exception _ -> ());
  print_endline "OK"

let constructor_tests () =
  print_string "constructor tests ... ";
  ignore (check_constructor_flags [`Public; `Synthetic]);
  (try ignore (check_constructor_flags [`Public; `Volatile]); fail () with Exception _ -> ());
  print_endline "OK"

let () =
  util_tests ();
  class_tests ();
  inner_class_tests ();
  field_tests ();
  method_tests ();
  constructor_tests ()
