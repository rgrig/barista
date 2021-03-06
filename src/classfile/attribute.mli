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

(** Attributes in both low- and high-level forms.
    It also provides conversion functions between levels as well as i/o
    functions for low-level. *)


(** {6 Low-level form} *)

type info = {
    name_index : Utils.u2;
    length : Utils.u4; (* TODO(rgrig): Remove this redundant field. *)
    data : string;
  }
(** Represents an attribute as defined in the class file format specification. *)


(** {6 I/O functions} *)

val read_info : InputStream.t -> info
(** [read_info st] reads an attribute from [st].
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write_info : OutputStream.t -> info -> unit
(** [write_info st a] writes an attribute [a] onto [st].
    Raises [OutputStream.Exception] if an i/o error occurs. *)

