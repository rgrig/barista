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

(** UTF8 line-oriented processing. *)


module UTF8LineReader : module type of UTF8LineReaderImpl with type t = UTF8LineReaderImpl.t
(** Implementation of line-oriented UTF8 readers. *)

module UTF8LineWriter : module type of UTF8LineWriterImpl with type t = UTF8LineWriterImpl.t
(** Implementation of line-oriented UTF8 writers. *)

val output : OutputStream.t -> Utils.UTF8Buffer.t -> unit
(** Outputs the content of the passed buffer onto the passed stream,
    using a [UTF8LineWriter] instance. An exception is raised if an error
    occurs during the write operation. *)
