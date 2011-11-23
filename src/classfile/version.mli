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

(** Definition of the various versions of the class file format. *)


(** {6 Type} *)

type t =
  | Java_1_0 (** JDK 1.0 (circa 1996). *)
  | Java_1_1 (** JDK 1.1 (circa 1997). *)
  | Java_1_2 (** J2SE 1.2 (circa 1998). *)
  | Java_1_3 (** J2SE 1.3 (circa 2000). *)
  | Java_1_4 (** J2SE 1.4 (circa 2002). *)
  | Java_1_5 (** J2SE 5.0 (circa 2004). *)
  | Java_1_6 (** Java SE 6 (circa 2006). *)
  | Java_1_7 (** Java SE 7 (in DP - circa 2011?). *)
  | Java_1_8 (** Java SE 8 (to be released). *)
(* The type representing the various Java versions. *)

val all : t list
(** The list of versions, in ascending order. *)

val default : t
(** The default version used by the library (currently [Java_1_6]). *)

val to_string : t -> string
(** Converts the passed version into a string. *)

type bound = {
    bound_version : t; (** Bound version. *)
    bound_feature : string; (** Feature description for bound. *)
  }
(** The type representing version bounds for a given feature. *)

type bounds = bound * bound option
(** The type representing version intervals.
    The first component is lower bound, while the second one is the
    (optional) higher bound. *)

val make_bounds : string -> t -> t option -> bounds
(** [make_bounds f lo hi] returns a version interval for feature [f],
    with lower version [lo], and optional higher version [hi].
    Both bounds are inclusive. The string [f] is copied. *)

val empty_bounds : bounds -> bool
(** Checks whether the passed bounds consists in an empty interval. *)


(** {6 Exception} *)

type error =
  | Invalid_version
  | Unsupported_feature of (Utils.u2 * Utils.u2) * string
  | Deprecated_feature of (Utils.u2 * Utils.u2) * string

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Constants} *)

val min_supported : Utils.u2 * Utils.u2
(** This value represents the minimum supported version of the class file
    format in [(major, minor)] fomat. *)

val max_supported : Utils.u2 * Utils.u2
(** This value represents the maximum supported version of the class file
    format in [(major, minor)] fomat. *)


(** {6 Conversion functions} *)

val major_minor_of_version : t -> Utils.u2 * Utils.u2
(** Converts the passed version into [(major, minor)] format. *)

val version_of_major_minor : Utils.u2 * Utils.u2 -> t
(** Converts the passed version from [(major, minor)] format. *)


(** {6 Utility functions} *)

val at_least : string -> t -> t -> unit
(** [at_least f v x] raises [Exception] if [x] is below [v].
    [f] is the textual description of the feature tested for version. *)

val at_most : string -> t -> t -> unit
(** [at_most f v x] raises [Exception] if [x] is above [v].
    [f] is the textual description of the feature tested for version. *)

val check : bounds -> t -> unit
(** [check b v] checks that version [v] is in bounds [b], raising
    [Exception] if not. *)

val intersect : bounds -> bounds -> bounds
(** [intersect v1 v2] compute the intersection of the intervals
    represented by bounds [v1] and [v2]. The returned bounds may
    represent an empty interval (where the higher bound is below
    the lower bound. *)

val intersect_list : bounds list -> bounds
(** [intersect_list l] applies [intersect] to [l].
    Raises [Invalid_argument] if [l] is empty. *)
