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

module Encoding = CamomileLibrary.CharEncoding.Configure (CamomileLibraryDefault.Config)

module UTF8Line = CamomileLibrary.ULine.Make (CamomileLibrary.UTF8)

type t = UTF8Line.output_line

class output_channel os = object

  method output b ofs len =
    OutputStream.write_bytes_from os b ofs len;
    len

  method flush () =
    OutputStream.flush os

  method close_out () =
    OutputStream.close os

end

let make os =
  let oc = new output_channel os in
  let ch = new Encoding.uchar_output_channel_of Encoding.utf8 oc in
  new UTF8Line.output_line ch

let put lw s =
  try
    lw#put (UTF8Impl.to_camomile s)
  with _ -> raise (OutputStream.Exception OutputStream.Unable_to_write_data)

let flush lw =
  try
    lw#flush ()
  with _ -> raise (OutputStream.Exception OutputStream.Unable_to_write_data)

let close lw =
  try
    lw#close_out ()
  with _ -> raise (OutputStream.Exception OutputStream.Unable_to_close_stream)

let close_noerr lw =
  try close lw with _ -> ()
