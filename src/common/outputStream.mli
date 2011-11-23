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

(** Output streams.
    As stated in the class file format definition, {i multibyte data items are
    always stored in big-endian order, where the high bytes come first}. *)


(** {6 Type} *)

type t
(** The type of output streams, whatever the stream destination is. *)


(** {6 Exception} *)

type error =
  | Unable_to_write_data
  | Unable_to_close_stream

exception Exception of error
(** Raised when an attempt made to write data fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 Constructors} *)

val make_of_buffer : Buffer.t -> t
(** Creates an output stream whose destination is a buffer. *)

val make_of_channel : out_channel -> t
(** Creates an output stream whose destination is a channel. *)

val make_of_descr : Unix.file_descr -> t
(** Creates an input stream whose source is a file descriptor. *)


(** {6 Functions} *)

val write_u1 : t -> Utils.u1 -> unit
(** Writes an unsigned 1-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_u2 : t -> Utils.u2 -> unit
(** Writes an unsigned 2-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_u4 : t -> Utils.u4 -> unit
(** Writes an unsigned 4-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_s1 : t -> Utils.s1 -> unit
(** Writes a signed 1-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_s2 : t -> Utils.s2 -> unit
(** Writes a signed 2-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_s4 : t -> Utils.s4 -> unit
(** Writes a signed 4-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_s8 : t -> Utils.s8 -> unit
(** Writes a signed 8-byte integer to the passed stream.
    Raises [Exception] if data cannot be written. *)

val write_bytes_from : t -> string -> int -> int -> unit
(** [write_bytes_from s pos len st] writes [len] bytes from [s] starting at
    [pos] to [st]. Raises [Exception] if data cannot be written. *)

val write_bytes : t -> string -> unit
(** [write_bytes s st] writes [s] to [st].
    Raises [Exception] if data cannot be written. *)

val write_elements : ('a list -> Utils.u2) -> t -> (t -> 'a -> unit) -> 'a list -> unit
(** [write_elements length st f l] will first write to [st] the
    length of [l] as a [Utils.u2] value. Then, it will write the elements
    from [l] using [f]. The parameter [length] is used to determine the
    length of [l], and should be used to ensure that the length fits in a
    [Utils.u2] value.
    Raises [Exception] if data cannot be written. *)

val flush : t -> unit
(** Flushes the passed stream.
    Raises [Exception] if an error occurs while flushing the stream. *)

val close : t -> unit
(** Closes the passed stream, any subsequent write will fail raising an exception.
    Raises [Exception] if an error occurs while closing the stream. *)

val close_noerr : t -> unit
(** Same as [close] but raised exceptions are silently discarded. *)

val try_with : t -> (t -> 'a) -> 'a
(** [try_with st f] is equivalent to [Utils.try_finally st f close_noerr]. *)


(** {6 Predefined streams} *)

val stdout : t
(** Predefined stream for standard output. *)

val stderr : t
(** Predefined stream for standard error. *)
