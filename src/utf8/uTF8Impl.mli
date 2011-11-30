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

(** Implementation of UTF8 strings based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [Utils.UTF8] module instead of this one.} *)


type t
(** The type of UTF8 strings. *)

type modified
(** The type of UTF8 strings in {i modified} format, as they appear in
    Java class files (cf. the documentation of the {i java.io.DataInput
    class}). *)

type bytes = string
(** A bare alias for [string],
    used to underline its use as an array of bytes. *)

type error =
  | Unable_to_convert_to_modified_utf8 of t
  | Unable_to_convert_from_modified_utf8 of modified
  | Unable_to_convert_to_utf8 of string
  | Unable_to_convert_from_utf8 of t
  | Invalid_index of int * int
  | Index_out_of_bounds of int * int
  | Invalid_escaped_string of t

exception Exception of error

val string_of_error : error -> string

val make : UCharImpl.t list -> t
(** Constructs a UTF8 string from a list of Unicode characters. *)

val modified_of_bytes : bytes -> modified
(** Conversion from bytes into modified UTF8 string. *)

val bytes_of_modified : modified -> bytes
(** Conversion from modified UTF8 string into bytes. *)

val to_modified : t -> modified
(** Conversion from UTF8 string into modified UTF8 string.
    Raises [Exception] if conversion fails. *)

val of_modified : modified -> t
(** Conversion from modified UTF8 string into UTF8 string.
    Raises [Exception] if conversion fails. *)

val to_string : t -> string
(** Conversion from UTF8 string into standard string.
    Raises [Exception] if conversion fails. *)

val to_string_noerr : t -> string
(** Equivalent to [to_string], except that any exception is discarded and
    ["..."] is returned. *)

val of_string : string -> t
(** Conversion from standard string into UTF8 string.
    Raises [Exception] if conversion fails. *)

val to_bytes : t -> bytes
(** Conversion from UTF8 string into bytes.
    Raises [Exception] if conversion fails. *)

val of_bytes : bytes -> t
(** Conversion from bytes into UTF8 string.
    Raises [Exception] if conversion fails. *)

val of_char : char -> t
(** Conversion from standard character into UTF8 string.
    Raises [Exception] if conversion fails. *)

val of_uchar : UCharImpl.t -> t
(** Conversion from Unicode character into UTF8 string. *)

val length : t -> int
(** Returns the length of the passed string (in characters, not bytes). *)

val get : t -> int -> UCharImpl.t
(** [get s i] returns the character of [s] at index [i].
    Raises [Exception] if index is not valid. *)

val equal : t -> t -> bool
(** Equality over UTF8 strings. *)

val hash : t -> int
(** Hash of UTF8 strings. *)

val compare : t -> t -> int
(** Comparison over UTF8 strings. *)

val index_from : t -> int -> UCharImpl.t -> int
(** [index_from s i c] returns the lowest index above or equal to [i]
    where string [s] contains a character that equals [c].
    Raises [Not_found] if such an index does not exist. *)

val rindex_from : t -> int -> UCharImpl.t -> int
(** [rindex_from s i c] returns the highest index below or equal to [i]
    where string [s] contains a character that equals [c].
    Raises [Not_found] if such an index does not exist. *)

val substring : t -> int -> int -> t
(** [substring s start end] returns a string whose characters are those
    of [s] from index [first] to index [last] (both inclusive).
    Returns an empty string iff [last < first].
    Raises [Exception] if [first] or [last] is not a valid index. *)

val (++) : t -> t -> t
(** Concatenation of UTF8 strings. *)

val concat : t list -> t
(** [concat l] returns the concatenation of all strings in [l]. *)

val concat_sep : t -> t list -> t
(** [concat_sep sep l] returns the concatenation of all strings in [l],
    separator [sep] being inserted between two strings. *)

val concat_sep_map : t -> ('a -> t) -> 'a list -> t
(** [concat_sep_map sep f l] returns the concatenation of all strings in
    [l'], separator [sep] being inserted between two strings.
    [l'] is defined as [List.map f l]. *)

val replace : UCharImpl.t -> UCharImpl.t -> t -> t
(** [replace c1 c2 s] returns a copy of [s] where every character equal
    to [c1] has been replaced by [c2]. *)

val contains : UCharImpl.t -> t -> bool
(** [contains c s] returns [true] iff string [s] contains a character
    equal to [c]. *)

val split : UCharImpl.t -> t -> t list
(** [split c s] returns the string list obtained by splitting [s] using
    delimiter [c]. *)

val escape : t -> t
(** [escape s] returns the literal constant string corresponding to the
    passed string. A leading and a trailing double quote are added and
    every quote inside the passed string is escaped by a backslash. *)

val unescape : t -> t
(** [unescape s] returns the string corresponding to the passed literal
    constant string. Both leading and trailing quotes are removed and
    escaped sequences are converted. *)

val escape_char : UCharImpl.t -> t
(** [escape_char c] returns the literal constant string corresponding to
    the passed character. A leading and a trailing simple quote are added. *)


(**/**)

external to_camomile : t -> CamomileLibrary.UTF8.t = "%identity"
(** {b FOR INTERNAL USE}

    Converts a string from its abstract representation into its
    Camomile representation. *)

external of_camomile : CamomileLibrary.UTF8.t -> t = "%identity"
(** {b FOR INTERNAL USE}

    Converts a string from its Camomile representation into its
    abstract representation. *)
