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

(* TODO(rgrig): Turn into a functor of Hashtbl.HashedType. *)

type 'a t =
  { mutable current : 'a array
  ; inverse : ('a, IntSet.t) Hashtbl.t
  ; mutable length : int }

let check_index s i =
  if not (0 <= i && i < s.length) then
    invalid_arg "Barista.ExtendableArray invalid index"

let add_index x i h =
  let ii = try Hashtbl.find h x with Not_found -> IntSet.empty in
  Hashtbl.replace h x (IntSet.add i ii)

let del_index x i h =
  Hashtbl.replace h x (IntSet.remove i (Hashtbl.find h x))

let get_index x h =
  IntSet.min_elt (Hashtbl.find h x)

let grab_array current =
  let length = Array.length current in
  let inverse = Hashtbl.create length in
  for i = 0 to length - 1 do add_index current.(i) i inverse done;
  { current; inverse; length }

let expand s x =
  let n = Array.length s.current in
  s.current <-
    Array.init (1 + n + n / 2) (fun i -> if i < n then s.current.(i) else x)

let make n x =
  grab_array (Array.make n x)

let from_array e a =
  let length = Array.length a in
  if not (0 <= length && length <= max_u2) then raise e;
  grab_array (Array.copy a)

let to_array s =
  Array.sub s.current 0 s.length

let length s =
  s.length

let get s i =
  let i = (i : u2 :> int) in
  check_index s i;
  s.current.(i)

let set s i x =
  let i = (i : u2 :> int) in
  check_index s i;
  del_index s.current.(i) i s.inverse;
  add_index x i s.inverse;
  s.current.(i) <- x

let add e s x =
  if s.length = max_u2 then raise e;
  (if s.length = Array.length s.current
  then expand s x
  else s.current.(s.length) <- x);
  add_index x s.length s.inverse;
  s.length <- succ s.length;
  u2 (pred s.length)

let index x s =
  u2 (get_index x s.inverse)
