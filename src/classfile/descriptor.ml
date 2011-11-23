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


(* Java types definition *)

type java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Void
  | `Class of Name.for_class
  | `Array of 'a ] constraint 'a = non_void_java_type
and non_void_java_type =
  [ `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Class of Name.for_class
  | `Array of 'a ] constraint 'a = non_void_java_type

type array_type =
  [ `Array of 'a ] constraint 'a = [ `Boolean
                                   | `Byte
                                   | `Char
                                   | `Double
                                   | `Float
                                   | `Int
                                   | `Long
                                   | `Short
                                   | `Class of Name.for_class
                                   | `Array of 'a ]


(* Exception *)

type error =
  | Invalid_class_name
  | Invalid_array_element_type
  | Array_with_too_many_dimensions
  | Invalid_descriptor_string
  | Empty_descriptor_string
  | Invalid_field_type
  | Invalid_local_variable_type
  | Invalid_method_descriptor
  | Invalid_method_parameter_type
  | Void_not_allowed

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_class_name -> "invalid class name"
  | Invalid_array_element_type -> "invalid array element type (void)"
  | Array_with_too_many_dimensions -> "array with more than 255 dimensions"
  | Invalid_descriptor_string -> "invalid descriptor string"
  | Empty_descriptor_string -> "empty descriptor string"
  | Invalid_field_type -> "invalid field type (void)"
  | Invalid_local_variable_type -> "invalid local variable type (void)"
  | Invalid_method_descriptor -> "invalid method descriptor"
  | Invalid_method_parameter_type -> "invalid parameter type (void)"
  | Void_not_allowed -> "void is not allowed here"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Utility functions *)

let is_primitive = function
  | `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short -> true
  | `Void
  | `Class _
  | `Array _ -> false

let filter_void err = function
  | `Boolean -> `Boolean
  | `Byte -> `Byte
  | `Char -> `Char
  | `Double -> `Double
  | `Float -> `Float
  | `Int -> `Int
  | `Long -> `Long
  | `Short -> `Short
  | `Void -> fail err
  | `Class c -> `Class c
  | `Array t -> `Array t

let filter_non_array err = function
  | `Boolean
  | `Byte
  | `Char
  | `Double
  | `Float
  | `Int
  | `Long
  | `Short
  | `Void
  | `Class _ -> fail err
  | `Array t -> `Array t

let java_type_of_partial_utf8 str i =
  let len = UTF8.length str in
  let rec jtopu n idx =
    if idx < len then begin
      let ch = UTF8.get str idx in
      switch
        UChar.equal
        [ capital_z,
          (fun _ -> `Boolean, succ idx);

          capital_b,
          (fun _ -> `Byte, succ idx);

          capital_c,
          (fun _ -> `Char, succ idx);

          capital_d,
          (fun _ -> `Double, succ idx);

          capital_f,
          (fun _ -> `Float, succ idx);

          capital_i,
          (fun _ -> `Int, succ idx);

          capital_j,
          (fun _ -> `Long, succ idx);

          capital_s,
          (fun _ -> `Short, succ idx);

          capital_v,
          (fun _ -> `Void, succ idx);

          capital_l,
          (fun _ ->
            try
              let index = UTF8.index_from str (succ idx) semi_colon in
              let name = (Name.make_for_class_from_internal (UTF8.substring str (succ idx) (pred index))) in
              (`Class name, index + 1)
            with
            | Not_found
            | Name.Exception _ -> fail Invalid_class_name);

          opening_square_bracket,
          (fun _ ->
            if n < 255 then
              let t, res = jtopu (succ n) (succ idx) in
              (`Array (filter_void Invalid_array_element_type t), res)
            else
              fail Array_with_too_many_dimensions) ]
        (fun _ -> fail Invalid_descriptor_string)
        ch
    end else
      fail Empty_descriptor_string in
  jtopu 0 i

let java_type_of_internal_utf8 s =
  let res, idx = java_type_of_partial_utf8 s 0 in
  if idx = UTF8.length s then
    res
  else
    fail Invalid_descriptor_string

let internal_utf8_of_java_type =
  let rec uojt n = function
    | `Boolean -> UTF8.of_string "Z"
    | `Byte -> UTF8.of_string "B"
    | `Char -> UTF8.of_string "C"
    | `Double -> UTF8.of_string "D"
    | `Float -> UTF8.of_string "F"
    | `Int -> UTF8.of_string "I"
    | `Long -> UTF8.of_string "J"
    | `Short -> UTF8.of_string "S"
    | `Void -> UTF8.of_string "V"
    | `Class c ->
        (UTF8.of_string "L")
          ++ (Name.internal_utf8_for_class c)
          ++ (UTF8.of_string ";")
    | `Array jt ->
        if n < 255 then
          (UTF8.of_string "[") ++ (uojt (succ n) (jt :> java_type))
        else
          fail Array_with_too_many_dimensions
  in uojt 0

let rec external_utf8_of_java_type = function
  | `Boolean -> UTF8.of_string "boolean"
  | `Byte -> UTF8.of_string "byte"
  | `Char -> UTF8.of_string "char"
  | `Double -> UTF8.of_string "double"
  | `Float -> UTF8.of_string "float"
  | `Int -> UTF8.of_string "int"
  | `Long -> UTF8.of_string "long"
  | `Short -> UTF8.of_string "short"
  | `Void -> UTF8.of_string "void"
  | `Class n -> Name.external_utf8_for_class n
  | `Array jt -> (external_utf8_of_java_type (jt :> java_type)) ++ (UTF8.of_string "[]")

let java_type_of_external_utf8 s =
  let rec make_array n x =
    if n = 0 then
      x
    else
      `Array (make_array (pred n) x) in
  let l = UTF8.length s in
  let i = ref 0 in
  while !i < l && ((UChar.is_letter_or_digit (UTF8.get s !i))
                 || (UChar.equal dot (UTF8.get s !i))
                 || (UChar.equal dollar (UTF8.get s !i))
                 || (UChar.equal opening_square_bracket (UTF8.get s !i))
                 || (UChar.equal closing_square_bracket (UTF8.get s !i))) do
    incr i
  done;
  if !i = l && UChar.is_letter (UTF8.get s 0) then
    let j = ref (pred l) in
    let dims = ref 0 in
    while (!j - 1 >= 0)
        && (UChar.equal closing_square_bracket (UTF8.get s !j))
        && (UChar.equal opening_square_bracket (UTF8.get s (!j - 1))) do
      incr dims;
      decr j;
      decr j
    done;
    if !dims > 255 then fail Array_with_too_many_dimensions;
    let prefix = UTF8.substring s 0 !j in
    let base = match (try UTF8.to_string prefix with _ -> "") with
    | "boolean" -> `Boolean
    | "byte" -> `Byte
    | "char" -> `Char
    | "double" -> `Double
    | "float" -> `Float
    | "int" -> `Int
    | "long" -> `Long
    | "short" -> `Short
    | "void" -> `Void
    | _ -> `Class (Name.make_for_class_from_external prefix) in
    if !dims = 0 then
      base
    else
      let array = make_array !dims (filter_void Invalid_array_element_type base) in
      (array :> java_type)
  else
    fail Invalid_descriptor_string

let rec equal_java_type x y =
  match (x, y) with
  | `Boolean, `Boolean
  | `Byte, `Byte
  | `Char, `Char
  | `Double, `Double
  | `Float, `Float
  | `Int, `Int
  | `Long, `Long
  | `Short, `Short
  | `Void, `Void -> true
  | (`Class cn1), (`Class cn2) -> Name.equal_for_class cn1 cn2
  | (`Array a1), (`Array a2) -> equal_java_type (a1 :> java_type) (a2 :> java_type)
  | _ -> false


(* Field descriptors *)

type for_field = non_void_java_type

let field_of_utf8 str =
  let t = java_type_of_internal_utf8 str in
  filter_void Invalid_field_type t

let utf8_of_field fd =
  internal_utf8_of_java_type (fd :> java_type)

let java_type_of_external_utf8_no_void s =
  let res = java_type_of_external_utf8 s in
  filter_void Void_not_allowed res

let java_type_of_internal_utf8_no_void s =
  let res = java_type_of_internal_utf8 s in
  filter_void Void_not_allowed res

let equal_for_field x y =
  equal_java_type (x :> java_type) (y :> java_type)


(* Method descriptors *)

type for_parameter = non_void_java_type

let parameter_of_utf8 = field_of_utf8

let utf8_of_parameter = utf8_of_field

let equal_for_parameter = equal_for_field

type for_method = (for_parameter list) * java_type

let method_of_utf8 str =
  let len = UTF8.length str in
  if (len > 2) && (UChar.equal opening_parenthesis (UTF8.get str 0)) then
    let index = (try
      UTF8.index_from str 1 closing_parenthesis
    with Not_found -> fail Invalid_method_descriptor) in
    let (ret, last) = java_type_of_partial_utf8 str (index + 1) in
    if (last <> len) then fail Invalid_method_descriptor;
    let params = ref [] in
    let curr = ref 1 in
    while !curr < index do
      let t, i = java_type_of_partial_utf8 str !curr in
      params := (filter_void Invalid_method_parameter_type t) :: !params;
      curr := i
    done;
    if !curr = index then
      ((List.rev !params), ret)
    else
      fail Invalid_method_descriptor
  else
    fail Invalid_method_descriptor

let utf8_of_method (params, return) =
  (UTF8.of_string "(")
    ++ (UTF8.concat (List.map utf8_of_parameter params))
    ++ (UTF8.of_string ")")
    ++ (internal_utf8_of_java_type return)

let equal_for_method (xp, xr) (yp, yr) =
  (list_equal xp yp)
    && (equal_java_type xr yr)

let compare_for_method md1 md2 =
  let p1, r1 = md1 in
  let p2, r2 = md2 in
  let cmp = compare (List.length p1) (List.length p2) in
  if cmp <> 0 then
    cmp
  else
    let cmp' = compare (utf8_of_method md1) (utf8_of_method md2) in
    if cmp' <> 0 then
      cmp'
    else
      compare (internal_utf8_of_java_type r1) (internal_utf8_of_java_type r2)
