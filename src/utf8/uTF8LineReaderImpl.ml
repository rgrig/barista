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

open CamomileLibrary

module Encoding = CharEncoding.Configure (BaristaCamomile.Config)

module UTF8Line = ULine.Make (UTF8)

type t = UTF8Line.input_line

class input_channel is = object

    method input b ofs len =
      let res = InputStream.read_available_bytes is len b ofs in
      if res > 0 then
        res
      else
        raise End_of_file

    method close_in () =
      InputStream.close is

end

let make is =
  let ic = new input_channel is in
  let ch = new Encoding.uchar_input_channel_of Encoding.utf8 ic in
  new UTF8Line.input_line ch

let get lr =
  try
    UTF8Impl.of_camomile (lr#get ())
  with
  | End_of_file as eof -> raise eof
  | _ -> raise (InputStream.Exception InputStream.Unable_to_read_data)

let close lr =
  try
    lr#close_in ()
  with _ -> raise (InputStream.Exception InputStream.Unable_to_close_stream)

let close_noerr lr =
  try close lr with _ -> ()
