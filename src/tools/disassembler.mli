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

(** Disassembling of class contents. *)


val disassemble_to_buffer : Utils.UTF8Buffer.t -> ClassPath.t -> Utils.UTF8.t -> unit
(** [disassemble_to_buffer buff cp cn] disassembles the class whose name
    is [cn] in classpath [cp]. The result is appended to [buff].
    Raises [ClassLoader.Exception] if the class cannot be loaded. *)

val disassemble_to_stream : OutputStream.t -> ClassPath.t -> Utils.UTF8.t -> unit
(** [disassemble_to_stream chan cp cn] disassembles the class whose name
    is [cn] in classpath [cp]. The result is printed onto [chan].
    Raises [ClassLoader.Exception] if the class cannot be loaded. *)

val disassemble : ClassPath.t -> Utils.UTF8.t -> unit
(** [disassemble cp cn] disassembles the class whose name is [cn] in
    classpath [cp]. The result is printed on the standard output.
    Raises [ClassLoader.Exception] if the class cannot be loaded. *)
