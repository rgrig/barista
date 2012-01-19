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

(** Input streams.
    As stated in the class file format definition, {i multibyte data items are
    always stored in big-endian order, where the high bytes come first}. *)


(** {6 Type} *)

type t
(** The type of input streams, whatever the stream source is. *)


(** {6 Exception} *)

BARISTA_ERROR =
  | End_of_input_stream
  | Unable_to_read_data
  | Unable_to_close_stream
  | Data_is_too_large


(** {6 Constructors} *)

val make_of_string : string -> t
(** Creates an input stream whose source is a string. *)

val make_of_buffer : Buffer.t -> t
(** Creates an input stream whose source is a buffer. *)

val make_of_channel : in_channel -> t
(** Creates an input stream whose source is a channel. *)

val make_of_descr : Unix.file_descr -> t
(** Creates an input stream whose source is a file descriptor. *)

val make : read_byte:(unit -> int) ->
    ?read_bytes:(int -> string) ->
    ?read_bytes_into:(int -> string -> int -> unit) ->
    ?read_available_bytes:(int -> string -> int -> int) ->
    close:(unit -> unit) -> t
(** Creates an input stream from passed functions:
    - [read_byte] is used to read one byte (see [Pervasives.input_byte]);
    - [read_bytes nb] (optional) is used to read [nb] bytes;
    - [read_bytes_into nb dst idx] (optional) is used to read [nb] bytes and
      store them into [dst], starting at index [idx];
    - [read_available_bytes nb dst idx] (optional) is similar to
      [read_bytes_into] except that it should not fail if reaching end of file,
      and should return the number of bytes actually read;
    - [close] is used to close the stream. *)


(** {6 Functions} *)

val read_u1 : t -> Utils.u1
(** Reads an unsigned 1-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_u2 : t -> Utils.u2
(** Reads an unsigned 2-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_u4 : t -> Utils.u4
(** Reads an unsigned 4-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_s1 : t -> Utils.s1
(** Reads a signed 1-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_s2 : t -> Utils.s2
(** Reads a signed 2-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_s4 : t -> Utils.s4
(** Reads a signed 4-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_s8 : t -> Utils.s8
(** Reads a signed 8-byte integer from the passed stream.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_bytes : t -> int -> string
(** [read_bytes st nb] reads [nb] bytes from [st] returning them as a string.
    Raises [Exception] if end of stream is encountered or an i/o error occurs. *)

val read_bytes_into : t -> int -> string -> int -> unit
(** [read_bytes_into st nb dst idx] reads [nb] bytes from [st] and stores them
    into [dst], starting at index [idx].
    Raises [Exception] if end of stream is encountered or an i/o error occurs.
    Raises [Exception] if read data does not fit into [dst]. *)

val read_available_bytes : t -> int -> string -> int -> int
(** [read_available_bytes st nb dst idx] is similar to [read_bytes_into],
    except that is does not raise [Exception] if end of file is reached
    but rather returns the number of bytes actually read. *)

val read_elements : t -> (t -> 'a) -> 'a list
(** [read_elements st f] will first read from [st] a [Utils.u2] value
    indicating the number of elements to actually read. Then, it will
    read the elements using [f] and return the list of read elements.
    Raises [Exception] if end of stream is encountered or an i/o error
    occurs. *)

val close : t -> unit
(** Closes the passed stream, any subsequent read will fail raising an exception
    indicating that end of stream has been reached.
    Raises [Exception] if an error occurs while closing the stream. *)

val close_noerr : t -> unit
(** Same as [close] but raised exceptions are silently discarded. *)

val try_with : t -> (t -> 'a) -> 'a
(** [try_with st f] is equivalent to [Utils.try_finally st f close_noerr]. *)


(** {6 Predefined stream} *)

val stdin : t
(** Predefined stream for standard input. *)
