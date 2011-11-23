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


type 'a t = {
    mutable current : 'a array;
    mutable next : int;
  }

let default_capacity = 128

let make len cap init =
  if (cap < 0) || (len < 0) then
    invalid_arg "BaristaLibrary.ExtendableArray.make";
  let cap = max cap len in
  if cap <= (max_u2 + 1) then
    { current = Array.make cap init;
      next = len; }
  else
    invalid_arg "BaristaLibrary.ExtendableArray.make"

let from_array e a x =
  let len = Array.length a in
  let size = ref default_capacity in
  while (!size <= max_u2) && (!size < len) do
    size := 2 * !size
  done;
  let size = min max_u2 !size in
  if (size >= len) then
    { current =
      Array.init
        size
        (fun i -> if i < len then a.(i) else x);
      next = len; }
  else
    raise e

let to_array a =
  Array.sub a.current 0 a.next

let length a =
  a.next

let capacity a =
  Array.length a.current

let get a i =
  let i = (i : u2 :> int) in
  let len = a.next in
  if (i < 0) || (i >= len) then
    invalid_arg "BaristaLibrary.ExtendableArray.get"
  else
    a.current.(i)

let set a i x =
  let i = (i : u2 :> int) in
  let len = a.next in
  if (i < 0) || (i >= len) then
    invalid_arg "BaristaLibrary.ExtendableArray.set"
  else
    a.current.(i) <- x

let find p a =
  let len = Array.length a.current in
  let idx = ref 0 in
  while (!idx < len) && not (p a.current.(!idx)) do
    incr idx
  done;
  if (!idx < len) then
    u2 !idx
  else
    raise Not_found

let rec add e a x z addit =
  let len = Array.length a.current in
  let size = if addit then 2 else 1 in
  let next = a.next in
  if next + size <= len then begin
    a.current.(a.next) <- x;
    if addit then a.current.(succ a.next) <- z;
    a.next <- next + size;
    u2 next
  end else begin
    if len >= max_u2 then raise e;
    let new_array =
      Array.init
        (min max_u2 (2 * len))
        (fun i -> if i < len then a.current.(i) else z) in
    a.current <- new_array;
    add e a x z addit
  end

let add_if_not_found e p a x z addit =
  try
    find p a
  with Not_found ->
    add e a x z addit
