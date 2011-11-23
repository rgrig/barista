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


(* Tokens *)

type token =
  | Directive of string
  | Attribute of string
  | Label of UTF8.t
  | Int of int64
  | Float of float
  | String of UTF8.t
  | Class_name of Name.for_class
  | Array_type of UTF8.t
  | Primitive_type of Descriptor.java_type
  | Field of Name.for_class * Name.for_field * Descriptor.for_field
  | Dynamic_method of Name.for_method * Descriptor.for_method
  | Method of Name.for_class * Name.for_method * Descriptor.for_method
  | Array_method of Descriptor.array_type * Name.for_method * Descriptor.for_method
  | Method_signature of Name.for_method * (Descriptor.for_parameter list)
  | Method_type of Descriptor.for_method
  | Method_handle of Bootstrap.method_handle
  | Identifier of UTF8.t
  | Arrow
  | Tilde


(* Exception *)

type error =
  | Invalid_label of UTF8.t
  | Invalid_directive of UTF8.t
  | Invalid_attribute of UTF8.t
  | Invalid_string of UTF8.t
  | Invalid_character of UChar.t
  | Invalid_float of string
  | Invalid_integer of string
  | Invalid_method_handle of UTF8.t
  | Invalid_token
  | Name_error of Name.error
  | Descriptor_error of Descriptor.error
  | UChar_error of UChar.error
  | UTF8_error of UTF8.error

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_label s ->
      Printf.sprintf "invalid label %S" (UTF8.to_string_noerr s)
  | Invalid_directive s ->
      Printf.sprintf "invalid directive %S" (UTF8.to_string_noerr s)
  | Invalid_attribute s ->
      Printf.sprintf "invalid attribute %S" (UTF8.to_string_noerr s)
  | Invalid_string s ->
      Printf.sprintf "invalid string %S" (UTF8.to_string_noerr s)
  | Invalid_character s ->
      Printf.sprintf "invalid character %C" (UChar.to_char_noerr s)
  | Invalid_float s ->
      Printf.sprintf "invalid float constant %S" s
  | Invalid_integer s ->
      Printf.sprintf "invalid integer constant %S" s
  | Invalid_method_handle s ->
      Printf.sprintf "invalid method handle %S" (UTF8.to_string_noerr s)
  | Invalid_token -> "invalid token"
  | Name_error e -> Name.string_of_error e
  | Descriptor_error e -> Descriptor.string_of_error e
  | UChar_error e -> UChar.string_of_error e
  | UTF8_error e -> UTF8.string_of_error e

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Lexing funtion *)

let rec analyse_token s =
  let len = UTF8.length s in
  let last = pred len in
  if UChar.equal colon (UTF8.get s last) then begin
    let i = ref 1 in
    while !i < last && UChar.is_letter_or_digit (UTF8.get s !i) do
      incr i
    done;
    if !i = last && len > 1 && UChar.is_letter (UTF8.get s 0) then
      Label s
    else
      fail (Invalid_label s)
  end else if UTF8.contains percentage s then begin
    let index = UTF8.index_from s 0 percentage in
    let prefix = UTF8.substring s 0 (pred index) in
    let suffix = UTF8.substring s (succ index) last in
    let sub_token = analyse_token suffix in
    let handle =
      match (UTF8.to_string_noerr prefix), sub_token with
      | "getField", Field (x, y, z) -> `getField (x, y, z)
      | "getStatic", Field (x, y, z) -> `getStatic (x, y, z)
      | "putField", Field (x, y, z) -> `putField (x, y, z)
      | "putStatic", Field (x, y, z) -> `putStatic (x, y, z)
      | "invokeVirtual", Method (x, y, z) -> `invokeVirtual (x, y, z)
      | "invokeStatic", Method (x, y, z) -> `invokeStatic (x, y, z)
      | "invokeSpecial", Method (x, y, z) -> `invokeSpecial (x, y, z)
      | "newInvokeSpecial", Method (x, y, (z, t)) ->
          if (UTF8.equal (Name.utf8_for_method y) class_constructor)
              && (Descriptor.equal_java_type t (`Class x)) then
            `newInvokeSpecial (x, z)
          else
            fail (Invalid_method_handle s)
      | "invokeInterface", Method (x, y, z) -> `invokeInterface (x, y, z)
      | _ -> fail (Invalid_method_handle s) in
    Method_handle handle
  end else if UTF8.contains opening_parenthesis s && UTF8.contains closing_parenthesis s then begin
    let opening_index = UTF8.index_from s 0 opening_parenthesis in
    let closing_index = UTF8.rindex_from s last closing_parenthesis in
    let prefix = UTF8.substring s 0 (pred opening_index) in
    let params = UTF8.substring s (succ opening_index) (pred closing_index) in
    if (succ closing_index) < len
        && UChar.equal colon (UTF8.get s (succ closing_index)) then
      let desc = (List.map Descriptor.java_type_of_external_utf8_no_void (UTF8.split comma params)),
        (Descriptor.java_type_of_external_utf8 (UTF8.substring s (succ (succ closing_index)) last)) in
      if UTF8.contains dot prefix then
        let dot_idx = UTF8.rindex_from prefix (pred (UTF8.length prefix)) dot in
        let class_name = UTF8.substring prefix 0 (pred dot_idx) in
        let meth_name = UTF8.substring prefix (succ dot_idx) (pred (UTF8.length prefix)) in
        if UTF8.contains opening_square_bracket class_name then
          Array_method ((Descriptor.filter_non_array Descriptor.Invalid_array_element_type (Descriptor.java_type_of_external_utf8 class_name)),
                        (Name.make_for_method meth_name),
                        desc)
        else
          Method ((Name.make_for_class_from_external class_name),
                  (Name.make_for_method meth_name),
                  desc)
      else if (UTF8.length prefix) = 0 then
        Method_type desc
      else
        Dynamic_method ((Name.make_for_method prefix), desc)
    else
      Method_signature ((Name.make_for_method prefix),
                        (List.map Descriptor.java_type_of_external_utf8_no_void (UTF8.split comma params)))
  end else begin
    if UTF8.contains colon s then
      let colon_idx = UTF8.index_from s 0 colon in
      let prefix = UTF8.substring s 0 (pred colon_idx) in
      let dot_idx = UTF8.rindex_from prefix (pred (UTF8.length prefix)) dot in
      Field ((Name.make_for_class_from_external (UTF8.substring prefix 0 (pred dot_idx))),
             (Name.make_for_field (UTF8.substring prefix (succ dot_idx) (pred (UTF8.length prefix)))),
             (Descriptor.java_type_of_external_utf8_no_void (UTF8.substring s (succ colon_idx) (pred (UTF8.length s)))))
    else if UChar.equal closing_square_bracket (UTF8.get s last) then
      Array_type s
    else if UTF8.contains dot s then
      Class_name (Name.make_for_class_from_external s)
    else
      try
        let t = Descriptor.java_type_of_external_utf8 s in
        if (Descriptor.is_primitive t) || (t = `Void) then
            Primitive_type t
        else
          Identifier s
      with _ ->
        Identifier s
  end

let tokens_of_line l =
  let state = new lexer_state l in
  let skip_whitespace () =
    while state#is_available && state#look_ahead_list [space; tabulation] do
      state#consume
    done in
  let read_token () =
    let buf = UTF8Buffer.make () in
    if state#look_ahead dot then begin
      state#consume;
      while state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) do
        UTF8Buffer.add_char buf state#consume_char
      done;
      let dir = UTF8.to_string (UTF8Buffer.contents buf) in
      if (String.length dir) = 0 then
        fail (Invalid_directive (UTF8Buffer.contents buf))
      else
        Directive dir
    end else if state#look_ahead at_character then begin
      state#consume;
      while state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) do
        UTF8Buffer.add_char buf state#consume_char
      done;
      let attr = UTF8.to_string (UTF8Buffer.contents buf) in
      if (String.length attr) = 0 then
        fail (Invalid_attribute (UTF8Buffer.contents buf))
      else
        Attribute attr
    end else if state#look_ahead quote then begin
      let prev = ref state#consume_char in
      UTF8Buffer.add_char buf !prev;
      while not (state#look_ahead quote && not (UChar.equal !prev back_slash)) do
        let curr = state#consume_char in
        UTF8Buffer.add_char buf curr;
        prev :=
          if (UChar.equal !prev back_slash) && (UChar.equal curr back_slash) then
            quote
          else
            curr
      done;
      UTF8Buffer.add_char buf state#consume_char;
      if state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) then
        fail (Invalid_string (UTF8Buffer.contents buf))
      else
         String (UTF8.unescape (UTF8Buffer.contents buf))
    end else if state#look_ahead (UChar.of_char '\'') then begin
      let prev = ref state#consume_char in
      while not (state#look_ahead (UChar.of_char '\'') && not (UChar.equal !prev back_slash)) do
        let curr = state#consume_char in
        UTF8Buffer.add_char buf curr;
        prev :=
          if (UChar.equal !prev back_slash) && (UChar.equal curr back_slash) then
            quote
          else
            curr
      done;
      state#consume;
      if state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) then
        fail (Invalid_character state#peek);
      let s = UTF8.unescape ((UTF8.of_string "\"") ++ (UTF8Buffer.contents buf) ++ (UTF8.of_string "\"")) in
      if (UTF8.length s) <> 1 then
        fail (Invalid_character (UTF8.get s 0))
      else
        Int (Int64.of_int (UChar.to_code (UTF8.get s 0)))
    end else if state#look_ahead_list ([minus; plus] @ digits) then begin
      while state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) do
        UTF8Buffer.add_char buf state#consume_char
      done;
      let n = UTF8.to_string (UTF8Buffer.contents buf) in
      let number = if (String.get n 0) = '+' then String.sub n 1 (pred (String.length n)) else n in
      if String.contains number '.' then
        try
          Float (float_of_string number)
        with _ -> fail (Invalid_float number)
      else
        try
          Int (Int64.of_string number)
        with _ -> fail (Invalid_integer number)
    end else if state#look_ahead (UChar.of_char '=') then begin
      state#consume;
      state#consume_only greater_than;
      Arrow
    end else if state#look_ahead (UChar.of_char '~') then begin
      state#consume;
      Tilde
    end else begin
      let continue = ref false in
      while state#is_available && not (state#look_ahead_list [space; tabulation; colon; sharp]) do
        let ch = state#consume_char in
        if UChar.equal dot ch then continue := true;
        UTF8Buffer.add_char buf ch;
        if UChar.equal opening_parenthesis ch then begin
          continue := true;
          while not (state#look_ahead closing_parenthesis) do
            let ch = state#consume_char in
            if not ((UChar.equal ch space) || (UChar.equal ch tabulation)) then
              UTF8Buffer.add_char buf ch
          done;
          UTF8Buffer.add_char buf state#consume_char;
        end
      done;
      skip_whitespace ();
      if state#is_available && (state#look_ahead colon) then begin
        UTF8Buffer.add_char buf state#consume_char;
        if !continue then begin
          skip_whitespace ();
          while state#is_available && not (state#look_ahead_list [space; tabulation; sharp]) do
            UTF8Buffer.add_char buf state#consume_char;
          done;
        end;
      end;
      analyse_token (UTF8Buffer.contents buf)
    end in
  let tokens = ref [] in
  while state#is_available do
    skip_whitespace ();
    if not state#is_available || state#look_ahead sharp then
      while state#is_available do state#consume done
    else
      let tok =
        try
          read_token ()
        with
        | Name.Exception e -> fail (Name_error e)
        | Descriptor.Exception e -> fail (Descriptor_error e)
        | UChar.Exception e -> fail (UChar_error e)
        | UTF8.Exception e -> fail (UTF8_error e)
        | Exception e -> fail e
        | _ -> fail Invalid_token in
      tokens := tok :: !tokens;
  done;
  List.rev !tokens


(* Miscellaneous *)

let equal x y =
  match x, y with
  | (Directive d), (Directive d') -> d = d'
  | (Attribute a), (Attribute a') -> a = a'
  | (Label l), (Label l') -> UTF8.equal l l'
  | (Int i), (Int i') -> i = i'
  | (Float f), (Float f') -> f = f'
  | (String s), (String s') -> UTF8.equal s s'
  | (Class_name cn), (Class_name cn') -> Name.equal_for_class cn cn'
  | (Array_type at), (Array_type at') -> UTF8.equal at at'
  | (Primitive_type pt), (Primitive_type pt') -> pt = pt'
  | (Field (x, y, z)), (Field (x', y', z')) ->
      (Name.equal_for_class x x')
        && (Name.equal_for_field y y')
        && (Descriptor.equal_java_type
              (z :> Descriptor.java_type)
              (z' :> Descriptor.java_type))
  | (Dynamic_method (x, y)), (Dynamic_method (x', y')) ->
      (Name.equal_for_method x x')
        && (Descriptor.equal_for_method y y')
  | (Method (x, y, z)), (Method (x', y', z')) ->
      (Name.equal_for_class x x')
        && (Name.equal_for_method y y')
        && (Descriptor.equal_for_method z z')
  | (Array_method (x, y, z)), (Array_method (x', y', z')) ->
      (Descriptor.equal_java_type
         (x :> Descriptor.java_type)
         (x' :> Descriptor.java_type))
        && (Name.equal_for_method y y')
        && (Descriptor.equal_for_method z z')
  | (Method_signature (x, y)), (Method_signature (x', y')) ->
      (Name.equal_for_method x x')
        && (list_equal
              ~eq:Descriptor.equal_java_type
              (y :> Descriptor.java_type list)
              (y' :> Descriptor.java_type list))
  | (Method_type md), (Method_type md') ->
      Descriptor.equal_for_method md md'
  | (Method_handle mh), (Method_handle mh') ->
      Bootstrap.equal_method_handle mh mh'
  | (Identifier i), (Identifier i') -> UTF8.equal i i'
  | Arrow, Arrow -> true
  | _ -> false
