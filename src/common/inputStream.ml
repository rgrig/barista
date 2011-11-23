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


(* Types *)

type t = {
    read_u1 : unit -> int;
    read_bytes : int -> string;
    read_bytes_into : int -> string -> int -> unit;
    read_available_bytes : int -> string -> int -> int;
    close : unit -> unit;
  }


(* Exception *)

type error =
  | End_of_input_stream
  | Unable_to_read_data
  | Unable_to_close_stream
  | Data_is_too_large

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | End_of_input_stream -> "end of input stream"
  | Unable_to_read_data -> "unable to read data"
  | Unable_to_close_stream -> "unable to close stream"
  | Data_is_too_large -> "data is too large"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Constructors *)

external coerce : 'a -> 'b = "%identity" (* to avoid bound checking *)

let make_of_string str =
  let ptr = ref 0 in
  let read_u1 () =
    let tmp = !ptr in
    if (tmp < String.length str) then
      let res = int_of_char str.[tmp] in
      ptr := tmp + 1;
      res
    else
      fail End_of_input_stream in
  let read_bytes nb =
    try
      let tmp = !ptr in
      let res = String.sub str tmp nb in
      ptr := tmp + nb;
      res
    with Invalid_argument _ -> fail End_of_input_stream in
  let read_bytes_into nb dst idx =
    try
      let tmp = !ptr in
      String.blit str tmp dst idx nb;
      ptr := tmp + nb
    with Invalid_argument _ -> fail End_of_input_stream in
  let read_available_bytes nb dst idx =
    try
      let tmp = !ptr in
      let len = String.length str in
      let nb = min nb (len - tmp) in
      String.blit str tmp dst idx nb;
      ptr := tmp + nb;
      nb
    with _ -> fail Unable_to_read_data in
  let close () =
    ptr := String.length str in
  { read_u1 = read_u1;
    read_bytes = read_bytes;
    read_bytes_into = read_bytes_into;
    read_available_bytes = read_available_bytes;
    close = close; }

let make_of_buffer buf =
  let ptr = ref 0 in
  let read_u1 () =
    let tmp = !ptr in
    if (tmp < Buffer.length buf) then
      let res = int_of_char (Buffer.nth buf tmp) in
      ptr := tmp + 1;
      res
    else
      fail End_of_input_stream in
  let read_bytes nb =
    try
      let tmp = !ptr in
      let res = Buffer.sub buf tmp nb in
      ptr := tmp + nb;
      res
    with Invalid_argument _ -> fail End_of_input_stream in
  let read_bytes_into nb dst idx =
    try
      let tmp = !ptr in
      let res = Buffer.sub buf tmp nb in
      String.blit res 0 dst idx nb;
      ptr := tmp + nb
    with Invalid_argument _ -> fail End_of_input_stream in
  let read_available_bytes nb dst idx =
    try
      let tmp = !ptr in
      let len = Buffer.length buf in
      let res = Buffer.sub buf tmp nb in
      let nb = min nb (len - tmp) in
      String.blit res 0 dst idx nb;
      ptr := tmp + nb;
      nb
    with _ -> fail Unable_to_read_data in
  let close () =
    ptr := Buffer.length buf in
  { read_u1 = read_u1;
    read_bytes = read_bytes;
    read_bytes_into = read_bytes_into;
    read_available_bytes = read_available_bytes;
    close = close; }

let make_of_channel ch =
  set_binary_mode_in ch true;
  let read_u1 () =
    try
      input_byte ch
    with
    | End_of_file -> fail End_of_input_stream
    | _ -> fail Unable_to_read_data in
  let read_bytes nb =
    try
      let res = String.create nb in
      really_input ch res 0 nb;
      res
    with
    | Invalid_argument _ -> fail End_of_input_stream
    | End_of_file -> fail End_of_input_stream
    | _ -> fail Unable_to_read_data in
  let read_bytes_into nb dst idx =
    try
      really_input ch dst idx nb
    with
    | Invalid_argument _ -> fail End_of_input_stream
    | End_of_file -> fail End_of_input_stream
    | _ -> fail Unable_to_read_data in
  let read_available_bytes nb dst idx =
    try
      input ch dst idx nb
    with _ -> fail Unable_to_read_data in
  let close () =
    try close_in ch with _ -> fail Unable_to_close_stream in
  { read_u1 = read_u1;
    read_bytes = read_bytes;
    read_bytes_into = read_bytes_into;
    read_available_bytes = read_available_bytes;
    close = close; }

let make_of_descr d =
  make_of_channel (Unix.in_channel_of_descr d)


(* Functions *)

let read_u1 st =
  coerce (st.read_u1 ())

let read_u2 st =
  let a = st.read_u1 () in
  let b = st.read_u1 () in
  coerce ((a lsl 8) lor b)

let read_u4 st =
  let open Int64 in
  let a = of_int (st.read_u1 ()) in
  let b = of_int (st.read_u1 ()) in
  let c = of_int (st.read_u1 ()) in
  let d = of_int (st.read_u1 ()) in
  coerce (logor
            (shift_left a 24)
            (logor
               (shift_left b 16)
               (logor (shift_left c 8) d)))
        
let read_s1 st =
  let x = read_u1 st in
  coerce (if x < 128 then x else x - 256)

let read_s2 st =
  let x = read_u2 st in
  coerce (if x < 32768 then x else x - 65536)

let read_s4 st =
  let open Int32 in
  let a = of_int (st.read_u1 ()) in
  let b = of_int (st.read_u1 ()) in
  let c = of_int (st.read_u1 ()) in
  let d = of_int (st.read_u1 ()) in
  coerce (logor
            (shift_left a 24)
            (logor
               (shift_left b 16)
               (logor (shift_left c 8) d)))

let read_s8 st =
  let open Int64 in
  let a = of_int (st.read_u1 ()) in
  let b = of_int (st.read_u1 ()) in
  let c = of_int (st.read_u1 ()) in
  let d = of_int (st.read_u1 ()) in
  let e = of_int (st.read_u1 ()) in
  let f = of_int (st.read_u1 ()) in
  let g = of_int (st.read_u1 ()) in
  let h = of_int (st.read_u1 ()) in
  coerce (logor
            (shift_left a 56)
            (logor
               (shift_left b 48)
               (logor
                  (shift_left c 40)
                  (logor
                     (shift_left d 32)
                     (logor
                        (shift_left e 24)
                        (logor
                           (shift_left f 16)
                           (logor
                              (shift_left g 8) h)))))))

let read_bytes st nb =
  st.read_bytes nb

let read_bytes_into st nb dst idx =
  st.read_bytes_into nb dst idx

let read_available_bytes st nb dst idx =
  st.read_available_bytes nb dst idx

let read_elements st f =
  let nb = read_u2 st in
  let res = ref [] in
  for i = 1 to (nb :> int) do
    let elem = f st in
    res := elem :: !res
  done;
  List.rev !res

let close st =
  st.close ()

let close_noerr st =
  try
    close st
  with _ -> ()

let try_with st f =
  Utils.try_finally st f close_noerr


(* Predefined stream *)

let stdin = make_of_channel stdin
