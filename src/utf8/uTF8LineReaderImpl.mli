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

(** Implementation of UTF8 line readers based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [LineUtils.UTF8LineReader] module
    instead of this one.} *)


type t
(** The type of UTF8 line-oriented readers. *)

val make : InputStream.t -> t
(** Constructs a line reader from an input stream. *)

val get : t -> UTF8Impl.t
(** [get r] returns the next line read from [r].
    Raises [End_of_file] if end of file is encountered.
    Raises [InputStream.Exception] if an error occurs. *)

val close : t -> unit
(** Closes the passed reader.
    Raises [InputStream.Exception] if an error occurs. *)

val close_noerr : t -> unit
(** Closes the passed reader, errors being silently discarded. *)
