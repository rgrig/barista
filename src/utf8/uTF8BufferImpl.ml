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

type t = CamomileLibrary.UTF8.Buf.buf

let default_size = 1024

let make () =
  CamomileLibrary.UTF8.Buf.create default_size

let make_of_size sz =
  if sz < 0 then invalid_arg "BaristaLibrary.UTF8BufferImpl.make_of_size";
  CamomileLibrary.UTF8.Buf.create sz

let add_char b c =
  CamomileLibrary.UTF8.Buf.add_char b (UCharImpl.to_camomile c)

let add_string b s =
  CamomileLibrary.UTF8.Buf.add_string b (UTF8Impl.to_camomile s)

let eol = UCharImpl.of_char '\n'

let add_endline b s =
  add_string b s;
  add_char b eol

let contents b =
  UTF8Impl.of_camomile (CamomileLibrary.UTF8.Buf.contents b)
