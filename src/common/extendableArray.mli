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

(** A growing array with quick lookup for values.  Indices have the type
  [Utils.u2]. *)

type 'a t
(** The type of extendable arrays. *)

val make : int -> 'a -> 'a t

val from_array : exn -> 'a array -> 'a t

val to_array : 'a t -> 'a array

val length : 'a t -> int

val get : 'a t -> Utils.u2 -> 'a

val set : 'a t -> Utils.u2 -> 'a -> unit
(** Does not grow the array: See [add]. *)

val add : exn -> 'a t -> 'a -> Utils.u2

val index : 'a -> 'a t -> Utils.u2
(** [index x a] returns the lowest index [i] of [a] such that
    [x = (get a i)].
    Raises [Not_found] if such an index does not exist. *)
