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

type error =
  | End_of_lexer
  | Invalid_consume of char * char

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | End_of_lexer -> "end of lexer"
  | Invalid_consume (w, f) -> Printf.sprintf "invalid consume (%C waited but %C found)" w f

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)

class t s =
  let s = UTF8Impl.to_camomile s in
  object (self)

    val mutable next = UTF8.first s

    method is_available =
      not (UTF8.out_of_range s next)

    method private check_available =
      if UTF8.out_of_range s next then
        fail End_of_lexer

    method peek =
      self#check_available;
      UCharImpl.of_camomile (UTF8.look s next)

    method look_ahead_list l =
      self#check_available;
      let next_char = UCharImpl.of_camomile (UTF8.look s next) in
      List.exists (UCharImpl.equal next_char) l

    method look_ahead ch =
      self#look_ahead_list [ch]

    method consume_char =
      self#check_available;
      let res = UTF8.look s next in
      next <- UTF8.next s next;
      UCharImpl.of_camomile res

    method consume =
      ignore (self#consume_char)

    method consume_only ch =
      let next_char = self#consume_char in
      if not (UCharImpl.equal ch next_char) then
        let ch = UCharImpl.to_char ch in
        let next_char = UCharImpl.to_char next_char in
        fail (Invalid_consume (ch, next_char))

    method consume_until_list l =
      let b = UTF8.Buf.create 256 in
      while not (self#look_ahead_list l) do
        UTF8.Buf.add_char b (UTF8.look s next);
        next <- UTF8.next s next
      done;
      UTF8Impl.of_camomile (UTF8.Buf.contents b)

    method consume_until ch =
      self#consume_until_list [ch]

  end
