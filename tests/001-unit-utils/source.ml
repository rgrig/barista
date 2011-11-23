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

open BaristaLibrary.Utils

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let char_tests () =
  print_string "char tests ... ";
  for i = 0 to 255 do
    let ch = Char.chr i in
    fail_if (UChar.to_char (UChar.of_char ch)) ch
  done;
  print_endline "OK"

let modified_utf8_tests () =
  print_string "modified utf8 tests ... ";
  let s = String.create 256 in
  for i = 0 to 255 do
    String.set s i (Char.chr i)
  done;
  let s2 = UTF8.to_string (UTF8.of_string s) in
  fail_if s s2;
  let b = UTF8.bytes_of_modified (UTF8.to_modified (UTF8.of_string s)) in
  let s' = UTF8.to_string (UTF8.of_modified (UTF8.modified_of_bytes b)) in
  fail_if s s';
  print_endline "OK"

let utf8_tests () =
  let sgn x = if x = 0 then 0 else x / (abs x) in
  print_string "utf8 tests ... ";
  let s = UTF8.of_string "azerty" in
  fail_if (UTF8.length s) 6;
  if not (UChar.equal (UTF8.get s 2) (UChar.of_char 'e')) then fail ();
  if not (UTF8.equal s (UTF8.of_string "azerty")) then fail ();
  if UTF8.equal s (UTF8.of_string "azertt") then fail ();
  fail_if
    (sgn (compare "aqw" "bfe"))
    (sgn (UTF8.compare (UTF8.of_string "aqw") (UTF8.of_string "bfe")));
  fail_if
    (UTF8.index_from (UTF8.of_string "azerta") 1 (UChar.of_char 'a'))
    5;
  (try
    ignore (UTF8.index_from (UTF8.of_string "azerta") 1 (UChar.of_char 'h'));
    fail ()
  with Not_found -> ());
  fail_if
    (UTF8.rindex_from (UTF8.of_string "azerta") 4 (UChar.of_char 't'))
    4;
  (try
    ignore (UTF8.rindex_from (UTF8.of_string "azerta") 1 (UChar.of_char 't'));
    fail ()
  with Not_found -> ());
  if not (UTF8.equal (UTF8.of_string "zert") (UTF8.substring s 1 4)) then fail ();
  fail_if
    (UTF8.length (UTF8.substring s 5 4))
    0;
  if not (UTF8.equal s ((UTF8.of_string "aze") ++ (UTF8.of_string "rty"))) then fail ();
  if not (UTF8.equal s (UTF8.concat [(UTF8.of_string "aze"); (UTF8.of_string "rty")])) then fail ();
  print_endline "OK"

let () =
  char_tests ();
  modified_utf8_tests ();
  utf8_tests ()
