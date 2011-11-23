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

let bool_array_serialuid = s8 0x578f203914b85de2L
let byte_array_serialuid = s8 0xacf317f8060854e0L
let char_array_serialuid = s8 0xb02666b0e25d84acL
let double_array_serialuid = s8 0x3ea68c14ab635a1eL
let float_array_serialuid = s8 0xb9c818922e00c42L
let int_array_serialuid = s8 0x4dba602676eab2a5L
let long_array_serialuid = s8 0x782004b512b17593L
let short_array_serialuid = s8 0xef832e06e55db0faL
let string_array_serialuid = s8 0xadd256e7e91d7b47L
let bool2_array_serialuid = s8 0x631f1721cb9a1c15L
let byte2_array_serialuid = s8 0x4bfd19156767db37L
let char2_array_serialuid = s8 0x98327eb72369a9caL
let double2_array_serialuid = s8 0xc7ad0bff6467ff45L
let float2_array_serialuid = s8 0x77aa8d1669fe1976L
let int2_array_serialuid = s8 0x17f7e44f198f893cL
let long2_array_serialuid = s8 0xfe76f8764a55dfbdL
let short2_array_serialuid = s8 0x7cae5dae123b4435L
let string2_array_serialuid = s8 0x324d09ad8432e457L
let data_serialuid = s8 0x1ba3739a707ba99bL

let make_desc d s =
  make_descriptor (UTF8.of_string d) s [] [] None false None

let () =
  let build_string x = String (UTF8.of_string x) in
  List.iter
    (fun (file, desc, serial, v) ->
      Printf.printf "encoding file '%s' ...\n" file;
      let os = OutputStream.make_of_channel (open_out file) in
      let desc = make_desc desc serial in
      encode os [ Array_instance (desc, v) ];
      OutputStream.close os)
    [ "bool_array.ser", "[Z", bool_array_serialuid,
      (Boolean_array [| false; true; true; false; true; false |]) ;

      "byte_array.ser", "[B", byte_array_serialuid,
      (Byte_array [| s1 24; s1 (-98); s1 23; s1 1; s1 (-1); s1 0 |]) ;

      "char_array.ser", "[C", char_array_serialuid,
      (Char_array [| u2 0x0043; u2 0x0045 |]) ;

      "double_array.ser", "[D", double_array_serialuid,
      (Double_array [| 0.0; 3.14; -1.0 |]) ;

      "float_array.ser", "[F", float_array_serialuid,
      (Float_array [| 0.0; 3.14; -1.0 |]) ;

      "int_array.ser", "[I", int_array_serialuid,
      (Int_array [| s4 24l; s4 (-98l); s4 23l; s4 1l; s4 (-1l); s4 0l |]) ;

      "long_array.ser", "[J", long_array_serialuid,
      (Long_array [| s8 24L; s8 (-98L); s8 23L; s8 1L; s8 (-1L); s8 0L |]) ;

      "short_array.ser", "[S", short_array_serialuid,
      (Short_array [| s2 24; s2 (-98); s2 23; s2 1; s2 (-1); s2 0 |]) ;

      "string_array.ser", "[Ljava.lang.String;", string_array_serialuid,
      (Object_array (Array.map build_string [| "abc"; "def"; "zzz"; "ghi"; "jkl"; "zzz" |])) ] ;
  List.iter
    (fun (file, desc, serial, v) ->
      Printf.printf "encoding file '%s' ...\n" file;
      let os = OutputStream.make_of_channel (open_out file) in
      let desc = make_desc desc serial in
      encode os [ Array_instance (desc, (Object_array v)) ];
      OutputStream.close os)
    [ "bool2_array.ser", "[[Z", bool2_array_serialuid,
      [| Array_instance (make_desc "[Z" bool_array_serialuid,
                         Boolean_array [| false; true; true |]) ;
         Array_instance (make_desc "[Z" bool_array_serialuid,
                         Boolean_array [| false; true; false |]) |] ;

      "byte2_array.ser", "[[B", byte2_array_serialuid,
      [| Array_instance (make_desc "[B" byte_array_serialuid,
                         Byte_array [| s1 24; s1 (-98); s1 23 |]) ;
         Array_instance (make_desc "[B" byte_array_serialuid,
                         Byte_array [| s1 1; s1 (-1); s1 0 |]) |] ;

      "char2_array.ser", "[[C", char2_array_serialuid,
      [| Array_instance (make_desc "[C" char_array_serialuid,
                         Char_array [| u2 0x0043 |]) ;
         Array_instance (make_desc "[C" char_array_serialuid,
                         Char_array [| u2 0x0045 |]) |] ;

      "double2_array.ser", "[[D", double2_array_serialuid,
      [| Array_instance (make_desc "[D" double_array_serialuid,
                         Double_array [| 0.0; 3.14 |]) ;
         Array_instance (make_desc "[D" double_array_serialuid,
                         Double_array [| -1.0 |]) |] ;

      "float2_array.ser", "[[F", float2_array_serialuid,
      [| Array_instance (make_desc "[F" float_array_serialuid,
                         Float_array [| 0.0; 3.14 |]) ;
         Array_instance (make_desc "[F" float_array_serialuid,
                         Float_array [| -1.0 |]) |] ;

      "int2_array.ser", "[[I", int2_array_serialuid,
      [| Array_instance (make_desc "[I" int_array_serialuid,
                         Int_array [| s4 24l; s4 (-98l); s4 23l |]) ;
         Array_instance (make_desc "[I" int_array_serialuid,
                         Int_array [| s4 1l; s4 (-1l); s4 0l |]) |] ;

      "long2_array.ser", "[[J", long2_array_serialuid,
      [| Array_instance (make_desc "[J" long_array_serialuid,
                         Long_array [| s8 24L; s8 (-98L); s8 23L |]) ;
         Array_instance (make_desc "[J" long_array_serialuid,
                         Long_array [| s8 1L; s8 (-1L); s8 0L |]) |] ;

      "short2_array.ser", "[[S", short2_array_serialuid,
      [| Array_instance (make_desc "[S" short_array_serialuid,
                         Short_array [| s2 24; s2 (-98); s2 23 |]) ;
         Array_instance (make_desc "[S" short_array_serialuid,
                         Short_array [| s2 1; s2 (-1); s2 0 |]) |] ;

      "string2_array.ser", "[[Ljava.lang.String;", string2_array_serialuid,
      [| Array_instance (make_desc "[Ljava.lang.String;" string_array_serialuid,
                         Object_array (Array.map build_string [| "abc"; "def"; "zzz" |])) ;
         Array_instance (make_desc "[Ljava.lang.String;" string_array_serialuid,
                         Object_array (Array.map build_string [| "ghi"; "jkl"; "zzz" |])) |] ];
  let os = OutputStream.make_of_channel (open_out "data.ser") in
  let desc = 
    make_descriptor
      (UTF8.of_string "pack.Data")
      data_serialuid
      []
      [ `Int,
        (Name.make_for_field (UTF8.of_string "value")) ;
        (`Class (Name.make_for_class_from_external (UTF8.of_string "java.lang.String"))),
        (Name.make_for_field (UTF8.of_string "name")) ]
      None
      false
      None in
      let inst =
        make_instance
          desc
          [ (Name.make_for_field (UTF8.of_string "value")), (Int_value (s4 13l)) ;
            (Name.make_for_field (UTF8.of_string "name")), (Object_value (String (UTF8.of_string "some_prime"))) ]
          [] in
      encode os [ Instance inst ];
      OutputStream.close os
