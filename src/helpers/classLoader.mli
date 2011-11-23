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

(** Class loaders that allows easy access and loading of classes,
    packages, and modules. *)


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

val find_class : t -> Utils.UTF8.t -> ClassDefinition.t
(** [find_class cl cn] returns the class definition for the class named
    [cn] (in external form) using class loader [cl].
    Raises [Exception] if such a class has not alredy been loaded and
    cannot be loaded. *)

val find_package : t -> Utils.UTF8.t -> PackageDefinition.t
(** [find_package cl pn] returns the package definition for the package
    named [pn] (in external form) using class loader [cl].
    Raises [Exception] if such a package has not alredy been loaded and
    cannot be loaded. *)

val find_module : t -> Utils.UTF8.t -> ModuleDefinition.t
(** [find_module cl pn] returns the module definition for the module
    named [mn] (in external form) using class loader [cl].
    Raises [Exception] if such a module has not alredy been loaded and
    cannot be loaded. *)

val add_class : t -> ClassDefinition.t -> unit
(** [add_class cl cd] adds the class [cd] to the class loader [cl].
    Raises [Exception] if a class with the same name has already been
    loaded. *)

val add_package : t -> PackageDefinition.t -> unit
(** [add_package cl pd] adds the package [pd] to the class loader [cl].
    Raises [Exception] if a package with the same name has already been
    loaded. *)

val add_module : t -> ModuleDefinition.t -> unit
(** [add_module cl md] adds the module [md] to the class loader [cl].
    Raises [Exception] if a module with the same name has already been
    loaded. *)

val mem_class : t -> Utils.UTF8.t -> bool
(** [mem_class cl cn] tests whether the class named [cn] has been loaded
    by the class loader [cl]. *)

val mem_package : t -> Utils.UTF8.t -> bool
(** [mem_package cl pn] tests whether the package named [pn] has been
    loaded by the class loader [cl]. *)

val mem_module : t -> Utils.UTF8.t -> bool
(** [mem_module cl mn] tests whether the module named [mn] has been
    loaded by the class loader [cl]. *)

val remove_class : t -> Utils.UTF8.t -> unit
(** [remove_class cl cn] removes the definition for class [cn] from
    class loader [cl]. *)

val remove_package : t -> Utils.UTF8.t -> unit
(** [remove_package cl pn] removes the definition for package [pn] from
    class loader [cl]. *)

val remove_module : t -> Utils.UTF8.t -> unit
(** [remove_module cl mn] removes the definition for module [mn] from
    class loader [cl]. *)

val replace_class : t -> ClassDefinition.t -> unit
(** Equivalent to [add_class] except that a previous definition is
    replaced. *)

val replace_package : t -> PackageDefinition.t -> unit
(** Equivalent to [add_package] except that a previous definition is
    replaced. *)

val replace_module : t -> ModuleDefinition.t -> unit
(** Equivalent to [add_module] except that a previous definition is
    replaced. *)
