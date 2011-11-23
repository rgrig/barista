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

(** Implementation of lexer states over UTF8 strings based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [Utils.lexer_state] class instead of this one.} *)


type error =
  | End_of_lexer
  | Invalid_consume of char * char

exception Exception of error
(** Exception to be thrown when a try is made to consume an invalid
    character from a lexer or when end of lexer is unexpectedly reached. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)

class t : UTF8Impl.t -> object

  method is_available : bool
      (** Returns [true] iff there is at least one character left on the
          lexer. *)

  method peek : UCharImpl.t
      (** Returns [true] the next character if one is available.
          Raises [Exception] if end of string is encountered. *)

  method look_ahead_list : UCharImpl.t list -> bool
      (** Returns [true] iff the next character of the lexer is equal to
          one of the passed list.
          Raises [Exception] if end of string is encountered. *)

  method look_ahead : UCharImpl.t -> bool
      (** Returns [true] iff the next character of the lexer is equal to
          the passed one.
          Raises [Exception] if end of string is encountered. *)

  method consume_char : UCharImpl.t
      (** Consumes the next character and returns it.
          Raises [Exception] if end of string is encountered. *)

  method consume : unit
      (** Consumes (that is {i skips}) the next character.
          Raises [Exception] if end of string is encountered. *)

  method consume_only : UCharImpl.t -> unit
      (** Consumes (that is skips) the next character only if it is equal
          to the passed character.
          Raises [Exception] if end of string is encountered or if
          passed character is not equal to the next one of the lexer. *)

  method consume_until_list : UCharImpl.t list -> UTF8Impl.t
      (** Consumes characters until a character equal to one of the
          passed list is read from the lexer, and then returns the string
          of consumed characters (none of the passed characters is consumed).
          Raises [Exception] if end of string is encountered. *)

  method consume_until : UCharImpl.t -> UTF8Impl.t
      (** Consumes characters until a character equal to the passed one is
          read from the lexer and then returns the string of consumed
          characters (the passed character is not consumed).
          Raises [Exception] if end of string is encountered. *)

end
(** This class encapsulates the state of a lexer over the UTF8 string that
    is passed at instance creation (essentially an index over the string).
    The next character being read is the first one of the UTF8 string passed
    at instance creation (unless it is empty). *)
