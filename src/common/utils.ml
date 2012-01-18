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

open Format

(* Integer types *)

type integer_error = {
    type_name : string;
    lower_bound : int64;
    upper_bound : int64;
    value : int64;
  }

exception Integer_exception of integer_error

let fail_integer n l u v =
  let ie = {
    type_name = n;
    lower_bound = l;
    upper_bound = u;
    value = v;
  } in
  raise (Integer_exception ie)

let string_of_integer_error ie =
  Printf.sprintf "%s value %Ld is out of bounds (%Ld..%Ld)"
    ie.type_name
    ie.value
    ie.lower_bound
    ie.upper_bound

let () =
  Printexc.register_printer
    (function
      | Integer_exception e -> Some (string_of_integer_error e)
      | _ -> None)

type u1 = int

let u1 x =
  if x >= 0 && x <= 255 then
    x
  else
    fail_integer "u1" 0L 255L (Int64.of_int x)

let max_u1 = 255

type s1 = int

let s1 x =
  if x >= -128 && x <= 127 then
    x
  else
    fail_integer "s1" (-128L) 127L (Int64.of_int x)

type u2 = int

let max_u2 = 65535

let u2 x =
  if x >= 0 && x <= max_u2 then
    x
  else
    fail_integer "u2" 0L 65535L (Int64.of_int x)

type s2 = int

let min_s2 = -32768

let max_s2 = 32767

let s2 x =
  if x >= min_s2 && x <= max_s2 then
    x
  else
    fail_integer "s2" (-32768L) 32767L (Int64.of_int x)

type u4 = int64

let u4 x =
  if x >= 0L && x <= 4294967295L then
    x
  else
    fail_integer "u4" 0L 4294967295L x

type s4 = int32

external s4 : int32 -> int32 = "%identity"

type s8 = int64

external s8 : int64 -> int64 = "%identity"

let u1_succ x =
  if x < 255 then
    succ x
  else
    fail_integer "u1" 0L 255L (Int64.of_int (succ x))

let s1_neg x =
  if x <> -128 then
    ~-x
  else
    fail_integer "s1" (-128L) 127L (Int64.of_int (~-x))

let s2_neg x =
  if x <> -32768 then
    ~-x
  else
    fail_integer "s2" (-32768L) 32767L (Int64.of_int (~-x))

let s4_pred x =
  if x <> Int32.min_int then
    Int32.pred x
  else
    fail_integer
      "s4"
      (Int64.of_int32 (Int32.min_int))
      (Int64.of_int32 (Int32.max_int))
      (Int64.pred (Int64.of_int32 x))

external u2_of_u1 : u1 -> u2 = "%identity"

external s4_of_s2 : s2 -> s4 = "%int32_of_int"

let fits_u k x =
  let bits = 8 * k in
  assert (0 <= bits && bits < 32);
  0 <= x && x < (1 lsl bits)

let fits_s k x =
  let bits = 8 * (k - 1) in
  assert (0 <= bits && bits < 32);
  let m = 1 lsl bits in -m <= x && x < m

(* Unicode support *)

module UChar = UCharImpl

module UTF8 = UTF8Impl

let (++) = UTF8Impl.(++)

module UTF8HashedType = struct

  type t = UTF8.t

  let equal = UTF8.equal

  let hash x =
    let res = ref 0 in
    CamomileLibrary.UTF8.iter
      (fun ch ->
        res := !res lxor (CamomileLibrary.UChar.uint_code ch))
      (UTF8.to_camomile x);
    !res

end

module UTF8Hashtbl = Hashtbl.Make (UTF8HashedType)

module UTF8Buffer = UTF8BufferImpl

type lexer_state_error = UTF8LexerStateImpl.error =
  | End_of_lexer
  | Invalid_consume of char * char

let string_of_lexer_state_error = UTF8LexerStateImpl.string_of_error

exception Lexer_state_exception = UTF8LexerStateImpl.Exception

class lexer_state = UTF8LexerStateImpl.t

let switch eq matches default x =
  let func =
    try
      snd (List.find (fun (c, _) -> eq c x) matches)
    with Not_found -> default in
  func x

let lexer_switch matches default ls =
  let next_char = ls#peek in
  switch UCharImpl.equal matches default next_char


(* Miscellaneous *)

let rec fix_point eq f x =
  let y = f x in
  if eq x y then
    y
  else
    fix_point eq f y

let compose_list l =
  fun x -> List.fold_left (fun acc f -> f acc) x l

let try_finally x f h =
  let res =
    try
      f x
    with e ->
      h x;
      raise e in
  h x;
  res

let rec map_partial f = function
  | hd :: tl ->
      (match f hd with
      | Some x -> x :: (map_partial f tl)
      | None -> map_partial f tl)
  | [] -> []

let map_list_to_array f l =
  match l with
  | hd :: tl ->
      let len = List.length l in
      let res = Array.make len (f hd) in
      let rec iter i = function
        | hd :: tl ->
            res.(i) <- f hd;
            iter (succ i) tl
        | [] -> res in
      iter 1 tl
  | [] -> [||]

let map_array_to_list f a =
  let res = ref [] in
  for i = pred (Array.length a) downto 0 do
    res := (f a.(i)) :: !res;
  done;
  !res

let identity x = x

let rec list_equal ?(eq = (=)) l1 l2 =
  (l1 == l2) ||
  (match l1, l2 with
  | (hd1 :: tl1), (hd2 :: tl2) ->
      if eq hd1 hd2 then list_equal tl1 tl2 else false
  | (_ :: _), [] -> false
  | [], (_ :: _) -> false
  | [], [] -> true)

let map2 f (x, y) = (f x, f y)
let map3 f (x, y, z) = (f x, f y, f z)
let map4 f (x, y, z, u) = (f x, f y, f z, f u)

module IntMap = Map.Make (struct type t = int let compare = compare end)

let pp_list pe f = List.iter (fun e -> fprintf f "@ %a" pe e)
