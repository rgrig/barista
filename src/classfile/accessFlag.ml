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

(* Types *)

type t =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Super
  | `Synchronized
  | `Bridge
  | `Volatile
  | `Transient
  | `Varargs
  | `Native
  | `Interface
  | `Abstract
  | `Strict
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]

let to_string = function
  | `Public -> "public"
  | `Private -> "private"
  | `Protected -> "protected"
  | `Static -> "static"
  | `Final -> "final"
  | `Super -> "super"
  | `Synchronized -> "synchronized"
  | `Bridge -> "bridge"
  | `Volatile -> "volatile"
  | `Transient -> "transient"
  | `Varargs -> "varargs"
  | `Native -> "native"
  | `Interface -> "interface"
  | `Abstract -> "abstract"
  | `Strict -> "strict"
  | `Synthetic -> "synthetic"
  | `Annotation -> "annotation"
  | `Enum -> "enum"
  | `Module -> "module"

type for_class =
  [ `Public
  | `Final
  | `Super
  | `Interface
  | `Abstract
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]

type for_inner_class =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Super
  | `Interface
  | `Abstract
  | `Synthetic
  | `Annotation
  | `Enum
  | `Module ]

type for_field =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Volatile
  | `Transient
  | `Synthetic
  | `Enum
  | `Module ]

type for_method =
  [ `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Synchronized
  | `Bridge
  | `Varargs
  | `Native
  | `Abstract
  | `Strict
  | `Synthetic
  | `Module ]

type for_constructor =
  [ `Public
  | `Private
  | `Protected
  | `Strict
  | `Varargs
  | `Synthetic
  | `Module ]

type for_initializer =
  [ `Static
  | `Strict ]

type for_package =
  [ `Interface
  | `Abstract
  | `Synthetic ]

type for_module =
  [ `Interface
  | `Abstract
  | `Synthetic ]


(* Exception *)

type error =
  | Invalid_class_flags of t option
  | Invalid_inner_class_flags of t option
  | Invalid_field_flags of t option
  | Invalid_method_flags of t option
  | Invalid_constructor_flags of t option
  | Invalid_initializer_flags of t option
  | Invalid_package_flags of t option
  | Invalid_module_flags of t option
  | Several_visibility_flags
  | Unknown_flag of string

exception Exception of error

let fail e = raise (Exception e)

let string_of_error e =
  let soe kind = function
    | Some x ->
        Printf.sprintf "invalid flags for %s (%S)" kind (to_string x)
    | None ->
        Printf.sprintf "invalid flags for %s (invalid list)" kind in
  match e with
  | Invalid_class_flags f -> soe "class" f
  | Invalid_inner_class_flags f -> soe "inner class" f
  | Invalid_field_flags f -> soe "field" f
  | Invalid_method_flags f -> soe "method" f
  | Invalid_constructor_flags f -> soe "constructor" f
  | Invalid_initializer_flags f -> soe "initializer" f
  | Invalid_package_flags f -> soe "package" f
  | Invalid_module_flags f -> soe "module" f
  | Several_visibility_flags -> "several visibility flags"
  | Unknown_flag f -> Printf.sprintf "unknown flag %S" f

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Functions *)

let all = [
  `Public ;
  `Private ;
  `Protected ;
  `Static ;
  `Final ;
  `Super ;
  `Volatile ;
  `Transient ;
  `Native ;
  `Interface ;
  `Abstract ;
  `Strict ;
  `Synthetic ;
  `Annotation ;
  `Enum ;
  `Module
]

let all_for_method = [
  `Public ;
  `Private ;
  `Protected ;
  `Static ;
  `Final ;
  `Synchronized ;
  `Bridge ;
  `Varargs ;
  `Native ;
  `Interface ;
  `Abstract ;
  `Strict ;
  `Synthetic ;
  `Annotation ;
  `Enum ;
  `Module
]

let to_int = function
  | `Public -> 0x0001
  | `Private -> 0x0002
  | `Protected -> 0x0004
  | `Static -> 0x0008
  | `Final -> 0x0010
  | `Super -> 0x0020
  | `Synchronized -> 0x0020
  | `Bridge -> 0x0040
  | `Volatile -> 0x0040
  | `Transient -> 0x0080
  | `Varargs -> 0x0080
  | `Native -> 0x0100
  | `Interface -> 0x0200
  | `Abstract -> 0x0400
  | `Strict -> 0x0800
  | `Synthetic -> 0x1000
  | `Annotation -> 0x2000
  | `Enum -> 0x4000
  | `Module -> 0x8000

let to_u2 x =
  u2 (to_int x)

let list_to_u2 l =
  let res =
    List.fold_left
      (fun acc elem ->
        acc + (to_int elem))
      0
      l in
  u2 res

let from_u2 meth mask =
  let mask = (mask : u2 :> int) in
  List.fold_left
    (fun l e -> if mask land (to_int e) <> 0 then e :: l else l)
    []
    (if meth then all_for_method else all)

let of_string = function
  | "public" -> `Public
  | "private" -> `Private
  | "protected" -> `Protected
  | "static" -> `Static
  | "final" -> `Final
  | "super" -> `Super
  | "synchonized" -> `Synchronized
  | "bridge" -> `Bridge
  | "volatile" -> `Volatile
  | "transient" -> `Transient
  | "varargs" -> `Varargs
  | "native" -> `Native
  | "interface" -> `Interface
  | "abstract" -> `Abstract
  | "strictfp" -> `Strict
  | "synthetic" -> `Synthetic
  | "annotation" -> `Annotation
  | "enum" -> `Enum
  | "module" -> `Module
  | f -> fail (Unknown_flag f)

let to_utf8 f =
  UTF8.of_string (to_string f)

let of_utf8 f =
  of_string (UTF8.to_string_noerr f)

let check_visibility is_non_inner_class fl =
  let succ_if_zero x = if x = 0 then succ x else x in
  let pub, pro, pri, mdl =
    List.fold_left
      (fun ((pub, pro, pri, mdl) as acc) flag ->
        match flag with
        | `Public -> (succ_if_zero pub, pro, pri, mdl)
        | `Protected -> (pub, succ_if_zero pro, pri, mdl)
        | `Private -> (pub, pro, succ_if_zero pri, mdl)
        | `Module -> (pub, pro, pri, succ_if_zero mdl)
        | _ -> acc)
      (0, 0, 0, 0)
      fl in
  let at_most_one = (pub + pro + pri + mdl) <= 1 in
  if is_non_inner_class then
    (if not (at_most_one && pro = 0 && pri = 0) then fail Several_visibility_flags)
  else
    (if not at_most_one then fail Several_visibility_flags)

let implies l =
  fun x y -> not (List.mem x l) || (List.mem y l)

let implies_not l =
  fun x y -> not (List.mem x l) || not (List.mem y l)

let check_class_flags fl =
  let ( ==> ) = implies fl in
  let ( =/> ) = implies_not fl in
  check_visibility true fl;
  if (`Interface ==> `Abstract)
      && (`Interface =/> `Final)
      && (`Interface =/> `Super)
      && (`Interface =/> `Enum)
      && (`Annotation ==> `Interface)
      && (`Final =/> `Abstract)
      && (`Abstract =/> `Final) then
    List.map
      (function
        | #for_class as x -> x
        | y -> fail (Invalid_class_flags (Some y)))
      fl
  else
    fail (Invalid_class_flags None)

let check_inner_class_flags fl =
  let ( ==> ) = implies fl in
  let ( =/> ) = implies_not fl in
  check_visibility false fl;
  if (`Interface ==> `Abstract)
      && (`Interface =/> `Final)
      && (`Interface =/> `Super)
      && (`Interface =/> `Enum)
      && (`Annotation ==> `Interface)
      && (`Final =/> `Abstract)
      && (`Abstract =/> `Final) then
    List.map
      (function
        | #for_inner_class as x -> x
        | y -> fail (Invalid_inner_class_flags (Some y)))
      fl
  else
    fail (Invalid_inner_class_flags None)

let check_field_flags interface fl =
  let ( =/> ) = implies_not fl in
  check_visibility false fl;
  if (`Final =/> `Volatile)
      && (`Volatile =/> `Final)
      && ((not interface)
        || (((List.mem `Public fl) || (List.mem `Module fl))
              && (List.mem `Static fl)
              && (List.mem `Final fl)
              && (List.for_all (fun x -> List.mem x [`Public;
                                                     `Module;
                                                     `Static;
                                                     `Final;
                                                     `Synthetic]) fl))) then
    List.map
      (function
        | #for_field as x -> x
        | y -> fail (Invalid_field_flags (Some y)))
      fl
  else
    fail (Invalid_field_flags None)

let check_method_flags interface fl =
  let ( =/> ) = implies_not fl in
  check_visibility false fl;
  if ((not interface)
        || (((List.mem `Public fl) || (List.mem `Module fl))
              && (List.mem `Abstract fl)
              && (List.for_all (fun x -> List.mem x [`Public;
                                                     `Module;
                                                     `Abstract;
                                                     `Varargs;
                                                     `Bridge;
                                                     `Synthetic]) fl)))
      && (interface
        || ((`Abstract =/> `Final)
              && (`Abstract =/> `Native)
              && (`Abstract =/> `Private)
              && (`Abstract =/> `Static)
              && (`Abstract =/> `Strict)
              && (`Abstract =/> `Synchronized))) then
    List.map
      (function
        | #for_method as x -> x
        | y -> fail (Invalid_method_flags (Some y)))
      fl
  else
    fail (Invalid_method_flags None)

let check_constructor_flags fl =
  check_visibility false fl;
  List.map
    (function
      | #for_constructor as x -> x
      | y -> fail (Invalid_constructor_flags (Some y)))
    fl

let check_initializer_flags fl =
  List.map
    (function
      | #for_initializer as x -> x
      | y -> fail (Invalid_initializer_flags (Some y)))
    fl

let check_package_flags fl =
  List.map
    (function
      | #for_package as x -> x
      | y -> fail (Invalid_package_flags (Some y)))
    fl

let check_module_flags fl =
  List.map
    (function
      | #for_module as x -> x
      | y -> fail (Invalid_module_flags (Some y)))
    fl

let compare x y =
  let rank = function
  | `Public
  | `Private
  | `Protected
  | `Module -> 1
  | `Static -> 2
  | `Final -> 3
  | `Synchronized -> 4
  | `Volatile -> 4
  | `Transient -> 4
  | `Native -> 4
  | `Abstract -> 2
  | `Strict -> 4
  | `Super
  | `Bridge
  | `Varargs
  | `Interface
  | `Synthetic
  | `Annotation
  | `Enum -> 5 in
  let cmp = compare (rank x) (rank y) in
  if cmp = 0 then compare x y else cmp

let list_compare l1 l2 =
  let s1 = List.mem `Static l1 in
  let s2 = List.mem `Static l2 in
  let pub1 = List.mem `Public l1 in
  let pub2 = List.mem `Public l2 in
  let pro1 = List.mem `Protected l1 in
  let pro2 = List.mem `Protected l2 in
  let pri1 = List.mem `Private l1 in
  let pri2 = List.mem `Private l2 in
  Pervasives.compare (pub2, pro2, pri2, s2, l2) (pub1, pro1, pri1, s1, l1)

let version_bounds = function
  | `Public ->
      Version.make_bounds "'public' flag" Version.Java_1_0 None
  | `Private ->
      Version.make_bounds "'private' flag" Version.Java_1_0 None
  | `Protected ->
      Version.make_bounds "'protected' flag" Version.Java_1_0 None
  | `Static ->
      Version.make_bounds "'static' flag" Version.Java_1_0 None
  | `Final ->
      Version.make_bounds "'final' flag" Version.Java_1_0 None
  | `Super ->
      Version.make_bounds "'super' flag" Version.Java_1_0 None
  | `Synchronized ->
      Version.make_bounds "'synchronized' flag" Version.Java_1_0 None
  | `Bridge ->
      Version.make_bounds "'bridge' flag" Version.Java_1_5 None
  | `Volatile ->
      Version.make_bounds "'volatile' flag" Version.Java_1_0 None
  | `Transient ->
      Version.make_bounds "'transient' flag" Version.Java_1_0 None
  | `Varargs ->
      Version.make_bounds "'varargs' flag" Version.Java_1_5 None
  | `Native ->
      Version.make_bounds "'native' flag" Version.Java_1_0 None
  | `Interface ->
      Version.make_bounds "'interface' flag" Version.Java_1_0 None
  | `Abstract ->
      Version.make_bounds "'abstract' flag" Version.Java_1_0 None
  | `Strict ->
      Version.make_bounds "'strict' flag" Version.Java_1_1 None
  | `Synthetic ->
      Version.make_bounds "'synthetic' flag" Version.Java_1_5 None
  | `Annotation ->
      Version.make_bounds "'annotation' flag" Version.Java_1_5 None
  | `Enum ->
      Version.make_bounds "'enum' flag" Version.Java_1_5 None
  | `Module ->
      Version.make_bounds "'module' flag" Version.Java_1_8 None

let list_to_utf8 = function
  | (_ :: _) as l ->
      let space = UTF8.of_char ' ' in
      (UTF8.concat_sep
         space
         (List.map to_utf8 (List.sort compare l)))
        ++ space
  | [] -> empty_utf8
