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


(* Utility functions *)

let replace_dot_with_slash =
  UTF8.replace dot slash

let replace_slash_with_dot s =
  UTF8.replace dollar dot (UTF8.replace slash dot s)

let is_valid_unqualified s =
  ((UTF8.length s) > 0)
    && not (UTF8.contains dot s)
    && not (UTF8.contains semi_colon s)
    && not (UTF8.contains opening_square_bracket s)
    && not (UTF8.contains slash s)

let is_valid_for_method s =
  (UTF8.equal s class_constructor)
|| (UTF8.equal s class_initializer)
|| ((is_valid_unqualified s)
      && not (UTF8.contains lower_than s)
      && not (UTF8.contains greater_than s))


(* Exception *)

let string_of_error_kind_name kind name =
  Printf.sprintf "invalid %s name (%S)"
    kind
    (UTF8.to_string_noerr name)

BARISTA_ERROR =
  | Invalid_class_name of (n : UTF8.t) -> string_of_error_kind_name "class" n
  | Invalid_field_name of (n : UTF8.t) -> string_of_error_kind_name "field" n
  | Invalid_method_name of (n : UTF8.t) -> string_of_error_kind_name "method" n
  | Invalid_package_name of (n : UTF8.t) -> string_of_error_kind_name "package" n
  | Invalid_module_name of (n : UTF8.t) -> string_of_error_kind_name "module" n


(* Name types and conversion functions *)

type for_class = (UTF8.t list) * (UTF8.t list)

type for_field = UTF8.t

type for_method = UTF8.t

type for_package = UTF8.t list

type for_module = UTF8.t list

let make_for_class ch s =
  let check x =
    if is_valid_unqualified x then
      x
    else
      fail (Invalid_class_name s) in
  try
    let idx = UTF8.index_from s 0 dollar in
    let cls = UTF8.substring s 0 (pred idx) in
    let inner = UTF8.substring s (succ idx) (pred (UTF8.length s)) in
    (List.map check (UTF8.split ch cls)),
    (List.map check (UTF8.split dollar inner))
  with Not_found -> (List.map check (UTF8.split ch s)), []

let make_for_class_from_internal = make_for_class slash

let make_for_class_from_external = make_for_class dot

let make_for_field s =
  if is_valid_unqualified s then
    s
  else
    fail (Invalid_field_name s)

let make_for_method s =
  if is_valid_for_method s then
    s
  else
    fail (Invalid_method_name s)

let make_for_pkg_or_mdl ch e s =
  let check x =
    if is_valid_unqualified x then
      x
    else
      fail e in
  List.map check (UTF8.split ch s)

let make_for_package_from_internal s =
  make_for_pkg_or_mdl slash (Invalid_package_name s) s

let make_for_package_from_external s =
  make_for_pkg_or_mdl dot (Invalid_package_name s) s

let make_for_module_from_internal s =
  make_for_pkg_or_mdl slash (Invalid_module_name s) s

let make_for_module_from_external s =
  make_for_pkg_or_mdl dot (Invalid_module_name s) s

let utf8_for_class sep1 sep2 c =
  let sep1 = UTF8.of_uchar sep1 in
  let sep2 = UTF8.of_uchar sep2 in
  let cls, inner = c in
  if inner = [] then
    UTF8.concat_sep sep1 cls
  else
    (UTF8.concat_sep sep1 cls) ++ sep2 ++ (UTF8.concat_sep sep2 inner)

let printable_utf8_for_class = utf8_for_class dot dot

let external_utf8_for_class = utf8_for_class dot dollar

let internal_utf8_for_class = utf8_for_class slash dollar

let utf8_for_field f = f

let utf8_for_method m = m

let utf8_for_pkg_or_mdl sep n =
  let sep = UTF8.of_uchar sep in
  UTF8.concat_sep sep n

let external_utf8_for_package n = utf8_for_pkg_or_mdl dot n

let internal_utf8_for_package n = utf8_for_pkg_or_mdl slash n

let external_utf8_for_module n = utf8_for_pkg_or_mdl dot n

let internal_utf8_for_module n = utf8_for_pkg_or_mdl slash n

let equal_for_list l1 l2 =
  list_equal ~eq:UTF8.equal l1 l2

let equal_for_class (c1, i1) (c2, i2) =
  (equal_for_list c1 c2) && (equal_for_list i1 i2)

let equal_for_field = UTF8.equal

let equal_for_method = UTF8.equal

let equal_for_package = equal_for_list

let equal_for_module = equal_for_list
