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

(** Printing of method control flow. *)

BARISTA_ERROR =
  | Invalid_desciptor of Utils.UTF8.t
  | Method_not_found
  | Method_has_no_code

val print_to_buffer : Utils.UTF8Buffer.t -> ClassLoader.t -> Utils.UTF8.t -> unit
(** [print_to_stream buff cl cn] appends to the passed buffer [buff] the
    control flow of the method whose descriptor is [mn] in classloader
    [cp].
    Raises [ClassLoader.Exception] if the class cannot be loaded.
    Raises [Exception] if method name is invalid, or method has no code. *)

val print_to_stream : OutputStream.t -> ClassLoader.t -> Utils.UTF8.t -> unit
(** [print_to_stream st cl cn] prints onto the passed stream the control
    flow of the method whose descriptor is [mn] in classloader [cp].
    Raises [ClassLoader.Exception] if the class cannot be loaded.
    Raises [Exception] if method name is invalid, or method has no code. *)

val print : ClassLoader.t -> Utils.UTF8.t -> unit
(** [print cl mn] prints onto the standard output the control flow of the
    method whose descriptor is [mn] in classloader [cl].
    Raises [ClassLoader.Exception] if the class cannot be loaded.
    Raises [Exception] if method name is invalid, or method has no code. *)
