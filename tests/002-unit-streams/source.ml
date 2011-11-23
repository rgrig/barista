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

open BaristaLibrary
open Utils

let fail () =
  print_string "KO";
  exit 1

let fail_if x y =
  if x <> y then fail ()

let io_tests () =
  print_string "io tests ... ";
  let buffer = Buffer.create 16 in
  let os = OutputStream.make_of_buffer buffer in
  for i = 0 to 0xFF do
    OutputStream.write_u1 os (u1 i)
  done;
  for i = 0 to 0xFFFF do
    OutputStream.write_u2 os (u2 i)
  done;
  let i32 = ref 0xFFFFFF00L in
  while !i32 <> 0xFFFFFFFFL do
    OutputStream.write_u4 os (u4 !i32);
    i32 := Int64.add !i32 Int64.one
  done;
  for i = -128 to 127 do
    OutputStream.write_s1 os (s1 i)
  done;
  for i = -32768 to 32767 do
    OutputStream.write_s2 os (s2 i)
  done;
  for i = -128 to 127 do
    OutputStream.write_s4 os (s4 (Int32.of_int i))
  done;
  for i = -128 to 127 do
    OutputStream.write_s8 os (s8 (Int64.of_int i))
  done;
  OutputStream.write_bytes os "test";
  OutputStream.close os;
  let is = InputStream.make_of_buffer buffer in
  for i = 0 to 0xFF do
    fail_if (InputStream.read_u1 is) (u1 i)
  done;
  for i = 0 to 0xFFFF do
    fail_if (InputStream.read_u2 is) (u2 i)
  done;
  let i32 = ref 0xFFFFFF00L in
  while !i32 <> 0xFFFFFFFFL do
    fail_if (InputStream.read_u4 is) (u4 !i32);
    i32 := Int64.add !i32 Int64.one
  done;
  for i = -128 to 127 do
    fail_if (InputStream.read_s1 is) (s1 i)
  done;
  for i = -32768 to 32767 do
    fail_if (InputStream.read_s2 is) (s2 i)
  done;
  for i = -128 to 127 do
    fail_if (InputStream.read_s4 is) (s4 (Int32.of_int i))
  done;
  for i = -128 to 127 do
    fail_if (InputStream.read_s8 is) (s8 (Int64.of_int i))
  done;
  fail_if (InputStream.read_bytes is 4) "test";
  InputStream.close is;
  print_endline "OK"

let () =
  io_tests ()
