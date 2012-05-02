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

(** Utility functions and modules used throughout the Barista project.

    In particular, it defines types and functions for UTF8 characters
    and strings. *)


(** {6 Integer types} *)

(** The following aliases are defined to keep the type expressions as
    close as possible to the Java specification. Moreover, these types
    are declared private and should be constructed by the functions
    with the same sames. *)

type integer_error = {
    type_name : string; (** Integer type name. *)
    lower_bound : int64; (** Lower bound of integer type. *)
    upper_bound : int64; (** Upper bound of integer type. *)
    value : int64; (** Acutal value passed to constructor function. *)
  }
(** Error describing an integer exception. *)

exception Integer_exception of integer_error
(** Raised when an attempt made to construct an integer out of bounds. *)

val string_of_integer_error : integer_error -> string
(** Converts the passed error into a string. *)

type u1 = private int
(** The type of unsigned 1-byte integers. *)

val u1 : int -> u1
(** Constructs a value of type [u1], raising [Integer_exception] if value
    is out of bounds. *)

val max_u1 : int
(** The greatest [u1] value. *)

type s1 = private int
(** The type of signed 1-byte integers. *)

val s1 : int -> s1
(** Constructs a value of type [s1], raising [Integer_exception] if value
    is out of bounds. *)

type u2 = private int
(** The type of unsigned 2-byte integers. *)

val max_u2 : int
(** The greatest [u2] value. *)

val u2 : int -> u2
(** Constructs a value of type [u2], raising [Integer_exception] if value
    is out of bounds. *)

type s2 = private int
(** The type of signed 2-byte integers. *)

val min_s2 : int
(** The lowes [s2] value. *)

val max_s2 : int
(** The greatest [s2] value. *)

val s2 : int -> s2
(** Constructs a value of type [s2], raising [Integer_exception] if value
    is out of bounds. *)

type u4 = private int64
(** The type of unsigned 4-byte integers. *)

val u4 : int64 -> u4
(** Constructs a value of type [u4], raising [Integer_exception] if value
    is out of bounds. *)

type s4 = private int32
(** The type of signed 4-byte integers. *)

external s4 : int32 -> s4 = "%identity"
(** Identity function. *)

type s8 = private int64
(** The type of signed 8-byte integers. *)

external s8 : int64 -> s8 = "%identity"
(** Identity function. *)

val i64_of_2i32 : int32 -> int32 -> int64

val s8_of_2s4 : s4 -> s4 -> s8

val u1_succ : u1 -> u1
(** Increments the passed value, raising [Integer_exception] if result
    would be out of bounds. *)

val s1_neg : s1 -> s1
(** Unary negation,
    raises [Integer_exception] if result would be out of bounds. *)

val s2_neg : s2 -> s2
(** Unary negation,
    raises [Integer_exception] if result would be out of bounds. *)

val s4_pred : s4 -> s4
(** Decrements the passed value, raising [Integer_exception] if result
    would be out of bounds. *)

external u2_of_u1 : u1 -> u2 = "%identity"
(** Identity function. *)

external s4_of_s2 : s2 -> s4 = "%int32_of_int"
(** Identity function. *)

val fits_s : int -> int -> bool
(** [fits_s k v] says if [v] fits in an [sk]. *)

val fits_u : int -> int -> bool
(** [fits_u k v] says if [v] fits in an [uk]. *)

(** {6 UTF8 support} *)

module UChar : module type of UCharImpl with type t = UCharImpl.t
(** Implementation of Unicode characters. *)

module UTF8 : module type of UTF8Impl with type t = UTF8Impl.t
(** Implementation of UTF8 strings. *)

val (++) : UTF8.t -> UTF8.t -> UTF8.t
(** Concatenation of UTF8 strings. *)

module UTF8Hashtbl : Hashtbl.S with type key = UTF8.t
(** Hashtables with UTF8 strings as keys. *)

module UTF8Buffer : module type of UTF8BufferImpl with type t = UTF8BufferImpl.t
(** Implementation of UTF8 buffers. *)

type lexer_state_error = UTF8LexerStateImpl.error =
  | End_of_lexer
  | Invalid_consume of char * char

val string_of_lexer_state_error : lexer_state_error -> string

exception Lexer_state_exception of lexer_state_error

class lexer_state : UTF8.t -> UTF8LexerStateImpl.t
(** State class with classical utility functions used to implement lexers. *)

val lexer_switch : (UCharImpl.t * (UCharImpl.t -> 'a)) list -> (UCharImpl.t -> 'a) -> UTF8LexerStateImpl.t -> 'a
(** [lexer_switch matches default ls char] search the association list
    [matches] for a key equal to [char]. If such a key exists, the return
    value is equal to the application of the associated function to
    [char]. Otherwise, the return value is equal to the application of
    [default] to [char]. *)


(* {6 Fixed-point magic} *)

val fix_point : ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a
(** [fix_point eq f x] returns a fix point of [f] seeding with value [x],
    and using [eq] as the equality function. *)

val k_y : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
val k_successive : (('a -> 'b) -> 'c -> 'd) -> ('c * 'a -> 'b) -> 'e * 'c -> 'd
val k_map : ('a -> 'b) -> ('c -> 'b -> 'd) -> 'c -> 'a -> 'd
val k_log : ('a -> 'b) -> ('c -> 'a -> 'd) -> 'c -> 'a -> 'd

(** {6 Miscellaneous} *)

val compose_list : ('a -> 'a) list -> 'a -> 'a
(** [compose_list [f1; f2; ...; fn] x] returns [f1 (f2 ... (fn x))]. *)

val try_finally : 'a -> ('a -> 'b) -> ('a -> unit) -> 'b
(** [try_finally x f h] implements the try/finally logic.
    [f] is the body of the try clause, while [h] is the finally handler.
    Both [f] and [h] receive [x] as their parameter. *)

val map_partial : ('a -> 'b option) -> 'a list -> 'b list
(** Similar to [List.map] except that the returned list contains
    elements mapped to [Some x], and ignores those mapped to [None]. *)

val map_list_to_array : ('a -> 'b) -> 'a list -> 'b array
(** [map_list_to_array f l] is equivalent to [Array.map f (Array.of_list l)]. *)

val map_array_to_list : ('a -> 'b) -> 'a array -> 'b list
(** [map_array_to_list f a] is equivalent to [Array.to_list (List.map f l)]. *)

val map2 : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
val map3 : ('a -> 'b) -> ('a * 'a * 'a) -> ('b * 'b * 'b)
val map4 : ('a -> 'b) -> ('a * 'a * 'a * 'a) -> ('b * 'b * 'b * 'b)

val from_some : 'a option -> 'a

module IntMap : Map.S with type key = int
module IntSet : Set.S with type elt = int

val identity : 'a -> 'a
(** The identity function. *)

val switch : ('a -> 'a -> bool) -> ('a * ('a -> 'b)) list -> ('a -> 'b) -> 'a -> 'b
(** [switch eq matches default x] search the association list [matches]
    for a key equal to [x] using equality function [eq]. If such a key
    exists, the return value is equal to the application of the
    associated function to [x]. Otherwise, the return value is equal to
    the application of [default] to [x]. *)

val list_equal : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list -> bool
(** Equality over lists, [eq] (defaulting to [(=)]) being the predicate
    used to compare list elements. *)

val pp_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val string_of_pp : (Format.formatter -> 'a -> unit) -> 'a -> string

val fresh : unit -> unit -> int64
(** [fresh ()] returns a function that, in turn, generates 0L, 1L, 2L, ... *)
