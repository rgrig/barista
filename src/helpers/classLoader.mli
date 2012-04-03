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

(** Class loaders that allows easy access and loading of classes
    and packages.  Modules are not supported. *)


type t
(** Represents a class loader, that is barely a map from names
    (in external form) to their definitions. *)

type error =
  | Unable_to_load of Utils.UTF8.t * string
  | Already_defined of Utils.UTF8.t

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)

val make : ClassPath.t -> t
(** Constructs a class loader from a class path that will be used to load
    elements. The returned class loader contains no definition. *)

val find_class : t -> Utils.UTF8.t -> HighTypes.class_
(** [find_class cl cn] returns the class definition for the class named
    [cn] (in external form) using class loader [cl].
    Raises [Exception] if such a class has not alredy been loaded and
    cannot be loaded. *)

val add_class : t -> HighTypes.class_ -> unit
(** [add_class cl cd] adds the class [cd] to the class loader [cl].
    Raises [Exception] if a class with the same name has already been
    loaded. *)

val mem_class : t -> Utils.UTF8.t -> bool
(** [mem_class cl cn] tests whether the class named [cn] has been loaded
    by the class loader [cl]. *)

val remove_class : t -> Utils.UTF8.t -> unit
(** [remove_class cl cn] removes the definition for class [cn] from
    class loader [cl]. *)

val replace_class : t -> HighTypes.class_ -> unit
(** Equivalent to [add_class] except that a previous definition is
    replaced. *)

