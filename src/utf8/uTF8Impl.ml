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

type t = UTF8.t

type modified = string

type bytes = string

type error =
  | Unable_to_convert_to_modified_utf8 of t
  | Unable_to_convert_from_modified_utf8 of modified
  | Unable_to_convert_to_utf8 of string
  | Unable_to_convert_from_utf8 of t
  | Invalid_index of int * int
  | Index_out_of_bounds of int * int
  | Invalid_escaped_string of t

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unable_to_convert_to_modified_utf8 s ->
      Printf.sprintf "unable to convert to modified UTF8 (%S)" s
  | Unable_to_convert_from_modified_utf8 s ->
      Printf.sprintf "unable to convert from modified UTF8 (%S)" s
  | Unable_to_convert_to_utf8 s ->
      Printf.sprintf "unable to convert to UTF8 (%S)" s
  | Unable_to_convert_from_utf8 s ->
      Printf.sprintf "unable to convert from UTF8 (%S)" s
  | Invalid_index (x, y) ->
      Printf.sprintf "invalid index (%d, %d)" x y
  | Index_out_of_bounds (x, y) ->
      Printf.sprintf "index out of bounds (%d, %d)" x y
  | Invalid_escaped_string s ->
      Printf.sprintf "invalid escaped string (%S)" s

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)

let make l =
  let res = UTF8.Buf.create (List.length l) in
  List.iter (fun x -> UTF8.Buf.add_char res (UCharImpl.to_camomile x)) l;
  UTF8.Buf.contents res

let modified_of_bytes x = x

let bytes_of_modified x = x

let to_modified u =
  try
    UTF8.validate u;
    let b = Buffer.create (UTF8.length u) in
    UTF8.iter
      (fun ch ->
        let add_char x =
          Buffer.add_char b (char_of_int x) in
        let code = UChar.uint_code ch in
        if (code >= 0x0001) && (code <= 0x007F) then
          add_char code
        else if (code = 0x0000) || ((code >= 0x0080) && (code <= 0x07FF)) then begin
          add_char ((0x00C0 lor (code lsr 6)));
          add_char ((0x0080 lor (code land 0x003F)))
        end else if (code >= 0x0800) && (code <= 0xFFFF) then begin
          add_char ((0x00E0 lor (code lsr 12)));
          add_char ((0x0080 lor ((code lsr 6) land 0x003F)));
          add_char ((0x0080 lor (code land 0x003F)))
        end else begin
          add_char 0x00ED;
          add_char ((0x00A0 lor (((code lsr 16) - 1) land 0x000F)));
          add_char ((0x0080 lor ((code lsr 10) land 0x003F)));
          add_char 0x00ED;
          add_char ((0x00B0 lor ((code lsr 6) land 0x000F)));
          add_char ((0x0080 lor (code land 0x003F)))
        end)
      u;
    Buffer.contents b
  with _ -> fail (Unable_to_convert_to_modified_utf8 u)

let of_modified m =
  let (+=) r n = r := !r + n in
  try
    let len = String.length m in
    let b = UTF8.Buf.create len in
    let add_char x =
      UTF8.Buf.add_char b (UChar.chr x) in
    let i = ref 0 in
    while !i < len do
      let code = int_of_char m.[!i] in
      i += 1;
      if (code land 0x0080) = 0 then
        add_char code
      else if (code land 0x00E0) = 0x00C0 then begin
        let code'1 = int_of_char m.[!i] in
        i += 1;
        assert ((code'1 land 0x00C0) = 0x0080);
        let v = ((code land 0x001F) lsl 6) lor (code'1 land 0x003F) in
        add_char v
      end else if (code land 0x00F0) = 0x00E0 then begin
        let code'1 = int_of_char m.[!i] in
        let code'2 = int_of_char m.[!i + 1] in
        i += 2;
        assert ((code'1 land 0x00C0) = 0x0080);
        assert ((code'2 land 0x00C0) = 0x0080);
        let v = ((code land 0x000F) lsl 12)
            lor ((code'1 land 0x003F) lsl 6)
            lor (code'2 land 0x003F) in
        add_char v
      end else begin
        let code'1 = int_of_char m.[!i] in
        let code'2 = int_of_char m.[!i + 1] in
        let code'3 = int_of_char m.[!i + 2] in
        let code'4 = int_of_char m.[!i + 3] in
        let code'5 = int_of_char m.[!i + 4] in
        i += 5;
        assert (code = 0x00ED);
        assert ((code'1 land 0x00F0) = 0x00A0);
        assert ((code'2 land 0x00C0) = 0x0080);
        assert (code'3 = 0x00ED);
        assert ((code'4 land 0x00F0) = 0x00B0);
        assert ((code'5 land 0x00C0) = 0x0080);
        let v = 0x010000
            lor ((code'1 land 0x000F) lsl 16)
            lor ((code'2 land 0x003F) lsl 10)
            lor ((code'4 land 0x000F) lsl 6)
            lor (code'5 land 0x003F) in
        add_char v
      end
    done;
    let res = UTF8.Buf.contents b in
    UTF8.validate res;
    res
  with _ -> fail (Unable_to_convert_from_modified_utf8 m)

let to_string s =
  try
    let res = Buffer.create (String.length s) in
    UTF8.iter (fun x -> Buffer.add_char res (UChar.char_of x)) s;
    Buffer.contents res
  with _ -> fail (Unable_to_convert_from_utf8 s)

let to_string_noerr s =
  try
    to_string s
  with _ -> "..."

let of_string s =
  try
    let len = String.length s in
    UTF8.init len (fun i -> UChar.of_char s.[i])
  with _ -> fail (Unable_to_convert_to_utf8 s)

let to_bytes s =
  bytes_of_modified (to_modified s)

let of_bytes b =
  modified_of_bytes (of_modified b)

let of_char ch =
  try
    let res = UTF8.Buf.create 4 in
    UTF8.Buf.add_char res (UCharImpl.to_camomile (UCharImpl.of_char ch));
    UTF8.Buf.contents res
  with _ -> fail (Unable_to_convert_to_utf8 (Char.escaped ch))

let of_uchar ch =
  UTF8.init 1 (fun _ -> UCharImpl.to_camomile ch)

let length = UTF8.length

let get s i =
  try
    UCharImpl.of_camomile (UTF8.get s i)
  with _ -> fail (Invalid_index (i, length s))

let equal x y =
  (x == y) || ((UTF8.compare x y) = 0)

let compare x y =
  if x == y then
    0
  else
    UTF8.compare x y

let gen_index_from update_utf8_index update_int_index =
  fun s i c ->
    let c = UCharImpl.to_camomile c in
    let idx = ref (UTF8.move s (UTF8.first s) i) in
    let res = ref i in
    while (not (UTF8.out_of_range s !idx))
        && (not (UChar.eq c (UTF8.look s !idx))) do
      idx := update_utf8_index s !idx;
      update_int_index res
    done;
    if UTF8.out_of_range s !idx then
      raise Not_found
    else
      !res

let index_from = gen_index_from UTF8.next incr

let rindex_from = gen_index_from UTF8.prev decr

let substring s first last =
  let len = max 0 (last - first + 1) in
  try
    let res = UTF8.Buf.create len in
    let idx = ref (UTF8.move s (UTF8.first s) first) in
    let i = ref 0 in
    while !i < len do
      if UTF8.out_of_range s !idx then fail (Index_out_of_bounds (!idx, len));
      UTF8.Buf.add_char res (UTF8.look s !idx);
      idx := UTF8.next s !idx;
      incr i
    done;
    UTF8.Buf.contents res
  with _ -> fail (Index_out_of_bounds (0, len))

let (++) x y =
  let res = UTF8.Buf.create ((UTF8.length x) + (UTF8.length y)) in
  UTF8.Buf.add_string res x;
  UTF8.Buf.add_string res y;
  UTF8.Buf.contents res

let concat l =
  let len =
    List.fold_left
      (fun acc elem -> acc + (UTF8.length elem))
      0
      l in
  let res = UTF8.Buf.create len in
  List.iter (UTF8.Buf.add_string res) l;
  UTF8.Buf.contents res

let concat_sep sep l =
  let sep_len = UTF8.length sep in
  let len =
    List.fold_left
      (fun acc elem -> acc + sep_len + (UTF8.length elem))
      0
      l in
  let res = UTF8.Buf.create (len - sep_len) in
  (match l with
  | hd :: tl ->
      UTF8.Buf.add_string res hd;
      List.iter
        (fun x ->
          UTF8.Buf.add_string res sep;
          UTF8.Buf.add_string res x)
        tl
  | [] -> ());
  UTF8.Buf.contents res

let concat_sep_map sep f l =
  let l' = List.map f l in
  concat_sep sep l'

let replace c1 c2 s =
  let c1 = UCharImpl.to_camomile c1 in
  let c2 = UCharImpl.to_camomile c2 in
  let res = UTF8.Buf.create (UTF8.length s) in
  UTF8.iter
    (fun x ->
      UTF8.Buf.add_char
        res
        (if UChar.eq x c1 then
          c2
        else
          x)) s;
  UTF8.Buf.contents res

let contains c s =
  try
    ignore (index_from s 0 c);
    true
  with _ -> false

let split c s =
  let res = ref [] in
  let curr = UTF8.Buf.create (UTF8.length s) in
  let c = UCharImpl.to_camomile c in
  UTF8.iter
    (fun x ->
      if UChar.eq c x then begin
        res := (UTF8.Buf.contents curr) :: !res;
        UTF8.Buf.clear curr
      end else
        UTF8.Buf.add_char curr x)
    s;
  let last = UTF8.Buf.contents curr in
  if UTF8.length last > 0 then
    res := last :: !res;
  List.rev !res

external is_printable : char -> bool = "caml_is_printable"

let double_quote = UCharImpl.of_char '"'

let simple_quote = UCharImpl.of_char '\''

let back_slash = UCharImpl.of_char '\\'

let lowercase_n = UCharImpl.of_char 'n'

let lowercase_t = UCharImpl.of_char 't'

let lowercase_u = UCharImpl.of_char 'u'

let new_line = UChar.of_char '\n'

let tabulation = UChar.of_char '\t'

let escape_delim delim s =
  let res = UTF8.Buf.create ((UTF8.length s) * 2) in
  let add_char x =
    UTF8.Buf.add_char res (UCharImpl.to_camomile x) in
  let add_string x =
    UTF8.Buf.add_string res (of_string x) in
  add_char delim;
  UTF8.iter
    (fun c ->
      let code = UChar.uint_code c in
      if code > 0x7F || code < 0 then
        let code'1 = code land 0x0000FFFF in (* from Camomile's UPervasives module *)
        let code'2 = code lsr 16 in
        if code'2 = 0 then
          add_string (Printf.sprintf "\\u%04X" code'1)
        else
          add_string (Printf.sprintf "\\U%04X%04X" code'2 code'1)
      else
        (* from stdlib's Char/String modules *)
        match UCharImpl.to_char (UCharImpl.of_camomile c) with
        | '"' ->
            add_char back_slash;
            add_char double_quote
        | '\\' ->
            add_char back_slash;
            add_char back_slash
        | '\n' ->
            add_char back_slash;
            add_char lowercase_n
        | '\t' ->
            add_char back_slash;
            add_char lowercase_t
        | ch ->
            if is_printable ch then
              add_char (UCharImpl.of_char ch)
            else
              let cc = Char.code ch in
              add_char back_slash;
              add_char (UCharImpl.of_char (Char.chr (48 + cc / 100)));
              add_char (UCharImpl.of_char (Char.chr (48 + (cc / 10) mod 10)));
              add_char (UCharImpl.of_char (Char.chr (48 + cc mod 10))))
    s;
  add_char delim;
  UTF8.Buf.contents res

let escape = escape_delim double_quote

let escape_char c =
  escape_delim simple_quote (of_uchar c)

let unescape s =
  let len = UTF8.length s in
  let double_quote' = UCharImpl.to_camomile double_quote in
  let back_slash' = UCharImpl.to_camomile back_slash in
  if (len < 2)
  || (not (UChar.eq (UTF8.get s 0) double_quote'))
  || (not (UChar.eq (UTF8.get s (pred len)) double_quote')) then
    fail (Invalid_escaped_string s)
  else
    let res = UTF8.Buf.create len in
    let idx = ref (UTF8.nth s 1) in
    let last = UTF8.last s in
    let read_digit hex =
      if UTF8.compare_index s !idx last >= 0 then
        fail (Invalid_escaped_string s)
      else
        let d = Char.uppercase (UCharImpl.to_char (UCharImpl.of_camomile (UTF8.look s !idx))) in
        if (d >= '0' && d <= '9') then
          (Char.code d) - (Char.code '0')
        else if (hex && (d >= 'A' && d <= 'F')) then
          (Char.code d) - (Char.code 'A')
        else fail (Invalid_escaped_string s) in
    while UTF8.compare_index s !idx last < 0 do
      let c = UTF8.look s !idx in
      if UCharImpl.equal back_slash (UCharImpl.of_camomile c) then begin
        idx := UTF8.next s !idx;
        if UTF8.compare_index s !idx last >= 0 then
          fail (Invalid_escaped_string s)
        else
          let c' = UCharImpl.of_camomile (UTF8.look s !idx) in
          if UCharImpl.equal double_quote c' then begin
            UTF8.Buf.add_char res double_quote';
            idx := UTF8.next s !idx
          end else if UCharImpl.equal back_slash c' then begin
            UTF8.Buf.add_char res back_slash';
            idx := UTF8.next s !idx
          end else if UCharImpl.equal lowercase_n c' then begin
            UTF8.Buf.add_char res new_line;
            idx := UTF8.next s !idx
          end else if UCharImpl.equal lowercase_t c' then begin
            UTF8.Buf.add_char res tabulation;
            idx := UTF8.next s !idx
          end else if UCharImpl.equal lowercase_u c' then
            let digit'1 = read_digit true in
            let digit'2 = read_digit true in
            let digit'3 = read_digit true in
            let digit'4 = read_digit true in
            let code =
              (digit'1 lsl 12)
                + (digit'2 lsl 8)
                + (digit'3 lsl 4)
                + digit'4 in
            UTF8.Buf.add_char res (UChar.chr_of_uint code)
          else if UCharImpl.equal (UCharImpl.of_char 'U') c' then
            let digit'1 = read_digit true in
            let digit'2 = read_digit true in
            let digit'3 = read_digit true in
            let digit'4 = read_digit true in
            let digit'5 = read_digit true in
            let digit'6 = read_digit true in
            let digit'7 = read_digit true in
            let digit'8 = read_digit true in
            let code =
              (digit'1 lsl 28)
                + (digit'2 lsl 24)
                + (digit'3 lsl 20)
                + (digit'4 lsl 16)
                + (digit'5 lsl 12)
                + (digit'6 lsl 8)
                + (digit'7 lsl 4)
                + digit'8 in
            UTF8.Buf.add_char res (UChar.chr_of_uint code)
          else (* digits *)
            let digit'1 = Char.code (UCharImpl.to_char c') in
            idx := UTF8.next s !idx;
            let digit'2 = read_digit false in
            let digit'3 = read_digit false in
            let code =
              ((digit'1 - 48) * 100)
                + ((digit'2 - 48) * 10)
                + (digit'3 - 48) in
            UTF8.Buf.add_char res (UCharImpl.to_camomile (UCharImpl.of_char (Char.chr code)))
      end else begin
        UTF8.Buf.add_char res c;
        idx := UTF8.next s !idx
      end
    done;
    UTF8.Buf.contents res

external to_camomile : t -> UTF8.t = "%identity"

external of_camomile : UTF8.t -> t = "%identity"
