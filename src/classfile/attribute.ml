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

open Utils
open Consts


(* Low-level form *)

type info = {
    name_index : u2;
    length : u4;
    data : string;
  }


(* I/O functions *)

let read_info st =
  let name = InputStream.read_u2 st in
  let len = InputStream.read_u4 st in
  if (len :> int64) > (Int64.of_int max_int) then
    raise (InputStream.Exception InputStream.Data_is_too_large)
  else
    let dat = InputStream.read_bytes st (Int64.to_int (len :> int64)) in
    { name_index = name;
      length = len;
      data = dat; }

let write_info st i =
  OutputStream.write_u2 st i.name_index;
  OutputStream.write_u4 st i.length;
  OutputStream.write_bytes st i.data
