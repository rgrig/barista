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

(** Implementation of UTF8 line writers based on the Camomile library
    (available at http://camomile.sourceforge.net).

    {b One is advised to use the [LineUtils.UTF8LineWriter] module
    instead of this one.} *)


type t
(** The type of UTF8 line-oriented writers. *)

val make : OutputStream.t -> t
(** Constructs a line writer from an output stream. *)

val put : t -> UTF8Impl.t -> unit
(** [put w s] outputs string [s] onto writer [w].
    Raises [OutputStream.Exception] if an error occurs. *)

val flush : t -> unit
(** Flushes the passed writer.
    Raises [OutputStream.Exception] if an error occurs. *)

val close : t -> unit
(** Closes the passed writer.
    Raises [OutputStream.Exception] if an error occurs. *)

val close_noerr : t -> unit
(** Closes the passed writer, errors being silently discarded. *)
