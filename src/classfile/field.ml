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


(* Low-level form *)

type info = {
    access_flags : u2;
    name_index : u2;
    descriptor_index : u2;
    attributes_count : u2;
    attributes_array : Attribute.info array;
  }


(* I/O functions *)

let read_info st =
  let flags = InputStream.read_u2 st in
  let name = InputStream.read_u2 st in
  let desc = InputStream.read_u2 st in
  let att_count = InputStream.read_u2 st in
  let atts =
    Array.init
      (att_count :> int)
      (fun _ -> Attribute.read_info st) in
  { access_flags = flags;
    name_index = name;
    descriptor_index = desc;
    attributes_count = att_count;
    attributes_array = atts; }

let write_info st i =
  OutputStream.write_u2 st i.access_flags;
  OutputStream.write_u2 st i.name_index;
  OutputStream.write_u2 st i.descriptor_index;
  OutputStream.write_u2 st i.attributes_count;
  Array.iter (Attribute.write_info st) i.attributes_array

