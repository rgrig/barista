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

(** Implementation of Unicode characters based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [Utils.UChar] module instead of this one.} *)


type t
(** The type of Unicode characters. *)

BARISTA_ERROR =
  | Unrepresentable_character of t
  | Invalid_character_code of int

val of_char : char -> t
(** Conversion from standard character into Unicode character. *)

val to_char : t -> char
(** Conversion from Unicode character into standard character.
    Raises [Exception] if conversion fails. *)

val to_char_noerr : t -> char
(** Equivalent to [to_char], except that any exception is discarded and
    ['?'] is returned. *)

val of_code : int -> t
(** Conversion from Unicode code value into Unicode character.
    Raises [Exception] if conversion fails. *)

val to_code : t -> int
(** Conversion from Unicode character into Unicode code value.
    Raises [Exception] if conversion fails. *)

val equal : t -> t -> bool
(** Equality over Unicode characters. *)

val compare : t -> t -> int
(** Comparison over Unicode characters. *)

val is_letter : t -> bool
(** Predicate testing whether the passed character is a letter. *)

val is_digit : t -> bool
(** Predicate testing whether the passed character is a digit. *)

val is_letter_or_digit : t -> bool
(** Predicate testing whether the passed character is a letter, a digit,
    {b or an underscore}. *)


(**/**)

external to_camomile : t -> CamomileLibrary.UChar.t = "%identity"
(** {b FOR INTERNAL USE}

    Converts a character from its abstract representation into its
    Camomile representation. *)

external of_camomile : CamomileLibrary.UChar.t -> t = "%identity"
(** {b FOR INTERNAL USE}

    Converts a character from its Camomile representation into its
    abstract representation. *)
