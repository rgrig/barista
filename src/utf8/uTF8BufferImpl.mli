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

(** Implementation of UTF8 buffers based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [Utils.UTF8Buffer] module instead of this one.} *)


type t
(** The type of UTF8 buffers. *)

val default_size : int
(** The size of buffers created through [make]. *)

val make : unit -> t
(** Constructs an empty buffer, of size [default_size]. *)

val make_of_size : int -> t
(** [make_of_size sz] constructs an empty buffer, of size [sz].
    Raises [Invalid_argument] if [sz] is negative. *)

val add_char : t -> UCharImpl.t -> unit
(** [add_char b c] appends char [c] to buffer [b]. *)

val add_string : t -> UTF8Impl.t -> unit
(** [add_char b s] appends string [s] to buffer [b]. *)

val add_endline : t -> UTF8Impl.t -> unit
(** [add_char b s] appends string [s] to buffer [b],
    and then also appends and end-of-line character. *)

val contents : t -> UTF8Impl.t
(** [contents b] returns the contents of buffer [b] as an UTF8 string. *)
