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

(* Type *)

type t = {
    write_u1 : int -> unit;
    write_bytes_from : string -> int -> int -> unit;
    flush : unit -> unit;
    close : unit -> unit;
  }


(* Exception *)

type error =
  | Unable_to_write_data
  | Unable_to_close_stream

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unable_to_write_data -> "unable to write data"
  | Unable_to_close_stream -> "unable to close stream"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Constructors *)

external coerce : 'a -> 'b = "%identity" (* to avoid bound checking *)

let make_of_buffer buf =
  let o = ref true in
  let write_u1 x =
    if !o then
      Buffer.add_char buf (char_of_int (x land 0x000000FF))
    else
      fail Unable_to_write_data in
  let write_bytes_from s pos len =
    if !o then
      Buffer.add_substring buf s pos len
    else
      fail Unable_to_write_data in
  let close () =
    o := false in
  { write_u1 = write_u1;
    write_bytes_from = write_bytes_from;
    flush = (fun () -> ());
    close = close; }

let make_of_channel ch =
  set_binary_mode_out ch true;
  let write_u1 x =
    try output_byte ch x with _ -> fail Unable_to_write_data in
  let write_bytes_from s pos len =
    try output_substring ch s pos len with _ -> fail Unable_to_write_data in
  let flush () =
    try flush ch with _ -> fail Unable_to_write_data in
  let close () =
    try close_out ch with _ -> fail Unable_to_close_stream in
  { write_u1 = write_u1;
    write_bytes_from = write_bytes_from;
    flush = flush;
    close = close; }

let make_of_descr d =
  make_of_channel (Unix.out_channel_of_descr d)


(* Functions *)

let write_u1 st x =
  let x = coerce x in
  st.write_u1 x

let write_u2 st x =
  let x = coerce x in
  let x1 = (x lsr 8) land 0x000000FF in
  let x2 = x land 0x000000FF in
  write_u1 st x1;
  write_u1 st x2

let write_u4 st x =
  let x = coerce x in
  let open Int64 in
  let x1 = logand (shift_right_logical x 24) 0x000000FFL in
  let x2 = logand (shift_right_logical x 16) 0x000000FFL in
  let x3 = logand (shift_right_logical x 8) 0x000000FFL in
  let x4 = logand x 0x000000FFL in
  write_u1 st (to_int x1);
  write_u1 st (to_int x2);
  write_u1 st (to_int x3);
  write_u1 st (to_int x4)

let write_s1 st x =
  let x = coerce x in
  let x' = x land 0x000000FF in
  write_u1 st x'

let write_s2 = write_u2

let write_s4 st x =
  let x = coerce x in
  let open Int32 in
  let x1 = logand (shift_right_logical x 24) 0x000000FFl in
  let x2 = logand (shift_right_logical x 16) 0x000000FFl in
  let x3 = logand (shift_right_logical x 8) 0x000000FFl in
  let x4 = logand x 0x000000FFl in
  write_u1 st (to_int x1);
  write_u1 st (to_int x2);
  write_u1 st (to_int x3);
  write_u1 st (to_int x4)

let write_s8 st x =
  let x = coerce x in
  let open Int64 in
  let x1 = logand (shift_right_logical x 56) 0x00000000000000FFL in
  let x2 = logand (shift_right_logical x 48) 0x00000000000000FFL in
  let x3 = logand (shift_right_logical x 40) 0x00000000000000FFL in
  let x4 = logand (shift_right_logical x 32) 0x00000000000000FFL in
  let x5 = logand (shift_right_logical x 24) 0x00000000000000FFL in
  let x6 = logand (shift_right_logical x 16) 0x00000000000000FFL in
  let x7 = logand (shift_right_logical x 8) 0x00000000000000FFL in
  let x8 = logand x 0x00000000000000FFL in
  write_u1 st (to_int x1);
  write_u1 st (to_int x2);
  write_u1 st (to_int x3);
  write_u1 st (to_int x4);
  write_u1 st (to_int x5);
  write_u1 st (to_int x6);
  write_u1 st (to_int x7);
  write_u1 st (to_int x8)

let write_bytes_from st s pos len =
  st.write_bytes_from s pos len

let write_bytes st s =
  write_bytes_from st s 0 (String.length s)

let write_elements length st f l =
  write_u2 st (length l);
  List.iter (f st) l

let flush st =
  st.flush ()

let close st =
  st.close ()

let close_noerr st =
  try
    close st
  with _ -> ()

let try_with st f =
  Utils.try_finally st f close_noerr


(* Predefined streams *)

let stdout = make_of_channel stdout

let stderr = make_of_channel stderr
