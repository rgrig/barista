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
open Serialization
open Utils

let print_array v =
  let iter f a = Array.iter (fun x -> print_string (f x); print_string "; ") a in
  let string_of_string = function
    | String utf -> UTF8.to_string utf
    | _ -> print_endline "invalid array value"; exit 1 in
  match v with
  | [ Array_instance (_, arr) ] ->
      print_string "  [| ";
      (match arr with
      | Boolean_array x -> iter string_of_bool x
      | Byte_array x -> iter string_of_int (Array.map (fun x -> (x : s1 :> int)) x)
      | Char_array x -> iter string_of_int (Array.map (fun x -> (x : u2 :> int)) x)
      | Double_array x -> iter string_of_float x
      | Float_array x -> iter string_of_float x
      | Int_array x -> iter Int32.to_string (Array.map (fun x -> (x : s4 :> int32)) x)
      | Long_array x -> iter Int64.to_string (Array.map (fun x -> (x : s8 :> int64)) x)
      | Object_array x -> iter string_of_string x
      | Short_array x -> iter string_of_int (Array.map (fun x -> (x : s2 :> int)) x));
      print_endline "|]"
  | _ ->
      print_endline "invalid array value";
      exit 1

let print_array2 v =
  match v with
  | [ Array_instance (_, (Object_array arr)) ] ->
      Array.iter print_array (Array.map (fun x -> [x]) arr)
  | _ ->
      print_endline "invalid array2 value";
      exit 1

let () =
  List.iter
    (fun file ->
      Printf.printf "decoding file '%s' ...\n" file;
      let is = InputStream.make_of_channel (open_in file) in
      let data = decode is in
      print_array data;
      InputStream.close is)
    [ "bool_array.ser" ;
      "byte_array.ser" ;
      "char_array.ser" ;
      "double_array.ser" ;
      "float_array.ser" ;
      "int_array.ser" ;
      "long_array.ser" ;
      "short_array.ser" ;
      "string_array.ser" ];
  List.iter
    (fun file ->
      Printf.printf "decoding file '%s' ...\n" file;
      let is = InputStream.make_of_channel (open_in file) in
      let data = decode is in
      print_array2 data;
      InputStream.close is)
    [ "bool2_array.ser" ;
      "byte2_array.ser" ;
      "char2_array.ser" ;
      "double2_array.ser" ;
      "float2_array.ser" ;
      "int2_array.ser" ;
      "long2_array.ser" ;
      "short2_array.ser" ;
      "string2_array.ser" ]
