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

(** Extendable (also called dynamic) arrays, growing as necessary.

    Extendable arrays are commonly defined by two dimensions: their
    length and capacity. The first dimension tells how many elements are
    currently actually stored in the array, while the second dimension
    tells how many elements could be stored in the array without having
    to redimension (and hence reallocate) it.

    Extendable arrays store elements in the same way as array
    ({i i. e.} at the lowest indexes), as opposed to sparse arrays.

    The present implementation uses [Utils.u2] indexes, thus having a
    maximum length of [65536]. *)


type 'a t
(** The type of extendable arrays. *)

val make : int -> int -> 'a -> 'a t
(** [make len cap init] returns an extendable array with an initial
    length of [len] and capacity [cap], each element being initialized to
    [init].
    Raises [Invalid_argument] if the passed length or capacity is too
    large, or negative. *)

val from_array : exn -> 'a array -> 'a -> 'a t
(** Constructs an extendable array from a bare one.
    The second element is used to fill the part of the array between its
    length and its capacity, if any.
    Raises [Invalid_argument] if the passed array is too large, or negative. *)

val to_array : 'a t -> 'a array
(** Constructs a bare array from an extendable one. *)

val length : 'a t -> int
(** Returns the length of the passed array. *)

val capacity : 'a t -> int
(** Returns the capacity of the passed array. *)

val get : 'a t -> Utils.u2 -> 'a
(** [get a i] returns the element of [a] at index [i].
    Raises [Invalid_argument] if [i] is out of bounds. *)

val set : 'a t -> Utils.u2 -> 'a -> unit
(** [set a i x] changes the element of [a] at index [i] to [x].
    {b The array is not extended by this operation.}
    Raises [Invalid_argument] if [i] is out of bounds. *)

val find : ('a -> bool) -> 'a t -> Utils.u2
(** [index p a] returns the lowest index [i] of [a] such that
    [p (get a i)] is [true].
    Raises [Not_found] if such an index does not exist. *)

val add : exn -> 'a t -> 'a -> 'a -> bool -> Utils.u2
(** [add e a x z addit] extends array [a] by an element set to [x].
    Returns the index of the added element. [addit] indicates whether an
    unused element [z] should be added to the array. The [z] value is
    also used to fill the part of the array between its length and its
    capacity, if any.
    Raises [e] if the array cannot be extended. *)

val add_if_not_found : exn -> ('a -> bool) -> 'a t -> 'a -> 'a -> bool -> Utils.u2
(** [add_if_not_found e p a x z add] returns the lowest index [i] of [a]
    such that [p (get a i)] is [true]. If such an index does not exist in
    [a], a new element [x] is added to the array.
    Equivalent to [try find p a with Not_found -> add e a x z addit].
    Raises [e] if the array cannot be extended. *)
