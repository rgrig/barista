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

(* Type *)

type t =
  | Java_1_0
  | Java_1_1
  | Java_1_2
  | Java_1_3
  | Java_1_4
  | Java_1_5
  | Java_1_6
  | Java_1_7
  | Java_1_8

let all = [
  Java_1_0 ;
  Java_1_1 ;
  Java_1_2 ;
  Java_1_3 ;
  Java_1_4 ;
  Java_1_5 ;
  Java_1_6 ;
  Java_1_7 ;
  Java_1_8
]

let default = Java_1_6

let to_string = function
  | Java_1_0 -> "1.0"
  | Java_1_1 -> "1.1"
  | Java_1_2 -> "1.2"
  | Java_1_3 -> "1.3"
  | Java_1_4 -> "1.4"
  | Java_1_5 -> "1.5"
  | Java_1_6 -> "1.6"
  | Java_1_7 -> "1.7"
  | Java_1_8 -> "1.8"

type bound = {
    bound_version : t;
    bound_feature : string;
  }

type bounds = bound * bound option

let copy_bound b =
  { bound_version = b.bound_version;
    bound_feature = b.bound_feature; }

let make_bounds f lo hi =
  { bound_version = lo; bound_feature = f; },
  (match hi with
  | Some v -> Some { bound_version = v; bound_feature = f; }
  | None -> None)

let empty_bounds b =
  match b with
  | _, None -> false
  | x, Some y -> x.bound_version > y.bound_version


(* Exception *)

type error =
  | Invalid_version
  | Unsupported_feature of (u2 * u2) * string
  | Deprecated_feature of (u2 * u2) * string

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_version -> "invalid version"
  | Unsupported_feature ((mj, mn), f) ->
      Printf.sprintf "%s is not supported by class file version %d.%d" f (mj :> int) (mn :> int)
  | Deprecated_feature ((mj, mn), f) ->
      Printf.sprintf "%s is deprecated after class file version %d.%d" f (mj :> int) (mn :> int)

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Constants *)

let min_supported = u2 45, u2 0

let max_supported = u2 52, u2 0


(* Conversion functions *)

let major_minor_of_version = function
  | Java_1_0 -> u2 45, u2 0
  | Java_1_1 -> u2 45, u2 3
  | Java_1_2 -> u2 46, u2 0
  | Java_1_3 -> u2 47, u2 0
  | Java_1_4 -> u2 48, u2 0
  | Java_1_5 -> u2 49, u2 0
  | Java_1_6 -> u2 50, u2 0
  | Java_1_7 -> u2 51, u2 0
  | Java_1_8 -> u2 52, u2 0

let version_of_major_minor (x, y) =
  match (x : u2 :> int), (y : u2 :> int) with
  | 45, y -> if y >= 0 && y < 3 then Java_1_0 else Java_1_1
  | 46, 0 -> Java_1_2
  | 46, _ | 47, 0 -> Java_1_3
  | 47, _ | 48, 0 -> Java_1_4
  | 48, _ | 49, 0 -> Java_1_5
  | 49, _ | 50, 0 -> Java_1_6
  | 50, _ | 51, 0 -> Java_1_7
  | 51, _ | 52, 0 -> Java_1_8
  | _ -> fail Invalid_version


(* Utility functions *)

let at_least f v x =
  if x < v then
    let v' = major_minor_of_version v in
    fail (Unsupported_feature (v', f))

let at_most f v x =
  if x > v then
    let v' = major_minor_of_version v in
    fail (Deprecated_feature (v', f))

let check (inf, sup) x =
  at_least inf.bound_feature inf.bound_version x;
  match sup with
  | Some s -> at_most s.bound_feature s.bound_version x
  | None -> ()

let intersect (min_v1, max_v1) (min_v2, max_v2) =
  let min_res = (* max of min_v1 and min_v2 *)
    if min_v1.bound_version >= min_v2.bound_version then
      copy_bound min_v1
    else
      copy_bound min_v2 in
  let max_res =
    match max_v1, max_v2 with
    | None, None -> None
    | Some x, None -> Some (copy_bound x)
    | None, Some x -> Some (copy_bound x)
    | Some x, Some y -> (* min of max_v1 and max_v2 *)
        if x.bound_version <= y.bound_version then
          Some (copy_bound x)
        else
          Some (copy_bound y) in
  min_res, max_res

let intersect_list = function
  | hd :: tl -> List.fold_left intersect hd tl
  | [] -> invalid_arg "BaristaLibrary.Version.intersect_list"
