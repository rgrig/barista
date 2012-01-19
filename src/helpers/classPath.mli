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

(** Abstraction over class path elements. *)


type t
(** The type of class paths.*)

BARISTA_ERROR =
  | Unable_to_open_archive of string
  | Does_not_exist of string
  | Class_not_found of string

val make_of_string : ?separator:string -> string -> t
(** Constructs a class path from a string, using the passed separator
    between class path elements (defaulting to colon).
    Raises [Exception] if a class path element does not exists or if an
    archive cannot be opened. *)

val make_of_list : string list -> t
(** Constructs a class path from the passed elements.
    Raises [Exception] if a class path element does not exists or if an
    archive cannot be opened. *)

val make : unit -> t
(** Constructs a class path from the ["CLASSPATH"] environment variable,
    defaulting to ["."] if the variable is not set.
    Raises [Exception] if a class path element does not exists or if an
    archive cannot be opened. *)

val append : ?separator:string -> string -> t -> t
(** [append p cp] returns a class path that is [cp] plus [p] added at its
    end. *)

val prepend : ?separator:string -> string -> t -> t
(** [prepend p cp] returns a class path that is [cp] plus [p] added at its
    begin. *)

val open_stream : t -> string -> InputStream.t
(** [open_stream cp cn] returns the stream for class whose fully qualified
    name is [cn], search class path [cp].
    Raises [Exception] if the passed class is not found in the given class
    path. *)

val close : t -> unit
(** Closes all underlying archives, subsequent tries to read from such
    archives will hence fail. Silently fails if an archive fails to
    close. *)
