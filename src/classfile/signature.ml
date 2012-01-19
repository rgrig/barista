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


(* Signatures definition *)

type class_signature = {
    formal_type_parameters : formal_type_parameter list;
    super_class_signature : class_type_signature;
    super_interface_signatures : class_type_signature list;
  }
and formal_type_parameter = {
    identifier : UTF8.t;
    class_bound : field_type_signature;
    interface_bounds : field_type_signature list;
  }
and field_type_signature =
  | Class_type_signature of class_type_signature
  | Array_type_signature of array_type_signature
  | Type_variable_signature of type_variable_signature
and class_type_signature = {
    qualified_class_name : Name.for_class;
    type_arguments : type_argument list;
    signature_suffix : class_type_signature_suffix list;
  }
and array_type_signature = type_signature
and type_signature =
  | Field_type_signature of field_type_signature
  | Base_type of Descriptor.java_type
and type_variable_signature = UTF8.t
and type_argument =
  | Star
  | Plus of field_type_signature
  | Minus of field_type_signature
  | Simple of field_type_signature
and class_type_signature_suffix = {
    suffix_identifier : UTF8.t;
    suffix_type_arguments : type_argument list;
  }
and method_signature = {
    formal_type_params : formal_type_parameter list;
    types : type_signature list;
    return : type_signature;
    throws_signatures : throws_signature list;
  }
and throws_signature =
  | Throws_class_type_signature of class_type_signature
  | Throws_type_variable_signature of type_variable_signature


(* Exception *)

BARISTA_ERROR =
  | Invalid_signature_type_header of (c : UChar.t) ->
      Printf.sprintf "invalid signature type header (%C)" (UChar.to_char_noerr c)
  | Invalid_signature_primitive_character of (c : UChar.t) ->
      Printf.sprintf "invalid signature primitive character (%C)" (UChar.to_char_noerr c)
  | Invalid_signature_type of (t : Descriptor.java_type) ->
      Printf.sprintf "invalid signature type (%s)"
        (UTF8.to_string_noerr (Descriptor.external_utf8_of_java_type t))
  | Invalid_class_signature of (s : UTF8.t) ->
      Printf.sprintf "invalid class signature (%S)" (UTF8.to_string_noerr s)
  | Invalid_field_signature of (s : UTF8.t) ->
      Printf.sprintf "invalid field signature (%S)" (UTF8.to_string_noerr s)
  | Invalid_method_signature of (s : UTF8.t) ->
      Printf.sprintf "invalid method signature (%S)" (UTF8.to_string_noerr s)
  | Extra_elements_after_class_signature of (s : UTF8.t) ->
      Printf.sprintf "extra elements after class signature (%S)" (UTF8.to_string_noerr s)
  | Extra_elements_after_field_signature of (s : UTF8.t) ->
      Printf.sprintf "extra elements after field signature (%S)" (UTF8.to_string_noerr s)
  | Extra_elements_after_method_signature of (s : UTF8.t) ->
      Printf.sprintf "extra elements after method signature (%S)" (UTF8.to_string_noerr s)


(* Conversion functions *)

let rec parse_class_signature ls =
  let ftp =
    if ls#look_ahead lower_than then begin
      ls#consume_only lower_than;
      let res = parse_formal_type_parameters ls in
      ls#consume_only greater_than;
      res
    end else
      [] in
  let super = parse_class_type_signature ls in
  let itfs = parse_class_type_signatures ls in
  { formal_type_parameters = ftp;
    super_class_signature = super;
    super_interface_signatures = itfs; }

and parse_formal_type_parameters ls =
  let res = ref [] in
  while not (ls#look_ahead greater_than) do
    res := (parse_formal_type_parameter ls) :: !res
  done;
  List.rev !res

and parse_formal_type_parameter ls =
  let id = ls#consume_until colon in
  ls#consume_only colon;
  let class_bnd =
    if ls#look_ahead_list [greater_than; colon] then
      Class_type_signature {
      qualified_class_name = Name.make_for_class_from_internal java_lang_Object;
      type_arguments = [];
      signature_suffix = []; }
    else
      parse_field_type_signature ls in
  let itfs = ref [] in
  while ls#look_ahead colon do
    ls#consume_only colon;
    itfs := (parse_field_type_signature ls) :: !itfs
  done;
  { identifier = Name.replace_slash_with_dot id;
    class_bound = class_bnd;
    interface_bounds = List.rev !itfs; }

and parse_field_type_signature ls =
  lexer_switch
    [ capital_l,
      (fun _ -> Class_type_signature (parse_class_type_signature ls)) ;
      opening_square_bracket,
      (fun _ -> Array_type_signature (parse_array_type_signature ls)) ;
      capital_t,
      (fun _ -> Type_variable_signature (parse_type_variable_signature ls)) ]
    (fun ch -> fail (Invalid_signature_type_header ch))
    ls

and parse_class_type_signature ls =
  ls#consume_only capital_l;
  let class_name = ls#consume_until_list [lower_than; dot; semi_colon] in
  let type_args =
    if ls#look_ahead lower_than then
      parse_type_arguments ls
    else
      [] in
  let res = ref [] in
  while ls#look_ahead dot do
    res := (parse_class_type_signature_suffix ls) :: !res
  done;
  ls#consume_only semi_colon;
  { qualified_class_name = Name.make_for_class_from_internal class_name;
    type_arguments = type_args;
    signature_suffix = List.rev !res; }

and parse_class_type_signatures ls =
  let res = ref [] in
  while ls#is_available do
    res := (parse_class_type_signature ls) :: !res
  done;
  List.rev !res

and parse_class_type_signature_suffix ls =
  ls#consume_only dot;
  let id = ls#consume_until_list [lower_than; dot; semi_colon] in
  let type_args =
    if ls#look_ahead lower_than then
      parse_type_arguments ls
    else
      [] in
  { suffix_identifier = id;
    suffix_type_arguments = type_args; }

and parse_type_variable_signature ls =
  ls#consume_only capital_t;
  let id = Name.replace_slash_with_dot (ls#consume_until semi_colon) in
  ls#consume_only semi_colon;
  id

and parse_type_argument ls =
  lexer_switch
    [ star,
      (fun _ -> ls#consume_only star; Star) ;
      minus,
      (fun _ -> ls#consume_only minus; Minus (parse_field_type_signature ls)) ;
      plus,
      (fun _ -> ls#consume_only plus; Plus (parse_field_type_signature ls)) ]
    (fun _ -> Simple (parse_field_type_signature ls))
    ls

and parse_type_arguments ls =
  ls#consume_only lower_than;
  let res = ref [] in
  while not(ls#look_ahead greater_than) do
    res := (parse_type_argument ls) :: !res
  done;
  ls#consume_only greater_than;
  List.rev !res

and parse_array_type_signature ls =
  ls#consume_only opening_square_bracket;
  parse_type_signature ls

and parse_type_signature ls =
  if ls#look_ahead_list [capital_l; opening_square_bracket; capital_t] then
    Field_type_signature (parse_field_type_signature ls)
  else
    Base_type (parse_base_type ls)

and parse_base_type ls =
  lexer_switch
    [ capital_b,
      (fun _ -> ls#consume_only capital_b; `Byte) ;
      capital_c,
      (fun _ -> ls#consume_only capital_c; `Char) ;
      capital_d,
      (fun _ -> ls#consume_only capital_d; `Double) ;
      capital_f,
      (fun _ -> ls#consume_only capital_f; `Float) ;
      capital_i,
      (fun _ -> ls#consume_only capital_i; `Int) ;
      capital_j,
      (fun _ -> ls#consume_only capital_j; `Long) ;
      capital_s,
      (fun _ -> ls#consume_only capital_s; `Short) ;
      capital_z,
      (fun _ -> ls#consume_only capital_z; `Boolean) ]
    (fun ch -> fail (Invalid_signature_primitive_character ch))
    ls

and parse_method_type_signature ls =
  let ftp =
    if not (ls#look_ahead opening_parenthesis) then begin
      ls#consume_only lower_than;
      let res = parse_formal_type_parameters ls in
      ls#consume_only greater_than;
      res
    end else
      [] in
  ls#consume_only opening_parenthesis;
  let types = ref [] in
  while not (ls#look_ahead closing_parenthesis) do
    types := (parse_type_signature ls) :: !types
  done;
  ls#consume_only closing_parenthesis;
  let rt = parse_return_type ls in
  let throws = ref [] in
  while ls#is_available do
    throws := (parse_throws_signature ls) :: !throws
  done;
  { formal_type_params = ftp;
    types = List.rev !types;
    return = rt;
    throws_signatures = List.rev !throws; }

and parse_return_type ls =
  if ls#look_ahead capital_v then
    parse_void_descriptor ls
  else
    parse_type_signature ls

and parse_throws_signature ls =
  ls#consume_only circonflex;
  if ls#look_ahead capital_t then
    Throws_type_variable_signature (parse_type_variable_signature ls)
  else
    Throws_class_type_signature (parse_class_type_signature ls)

and parse_void_descriptor ls =
  ls#consume_only capital_v;
  Base_type `Void

let rec dump_class_signature buf cs =
  if cs.formal_type_parameters <> [] then begin
    UTF8Buffer.add_char buf lower_than;
    dump_formal_type_parameters buf cs.formal_type_parameters;
    UTF8Buffer.add_char buf greater_than
  end;
  dump_class_type_signature buf cs.super_class_signature;
  dump_class_type_signatures buf cs.super_interface_signatures

and dump_formal_type_parameters buf l =
  List.iter (dump_formal_type_parameter buf) l

and dump_formal_type_parameter buf ftp =
  UTF8Buffer.add_string buf (Name.replace_dot_with_slash ftp.identifier);
  UTF8Buffer.add_char buf colon;
  dump_field_type_signature buf ftp.class_bound;
  List.iter (fun ib ->
    UTF8Buffer.add_char buf colon;
    dump_field_type_signature buf ib)
    ftp.interface_bounds

and dump_field_type_signature buf = function
  | Class_type_signature cts -> dump_class_type_signature buf cts
  | Array_type_signature ats -> dump_array_type_signature buf ats
  | Type_variable_signature tvs -> dump_type_variable_signature buf tvs

and dump_class_type_signature buf cts =
  UTF8Buffer.add_char buf capital_l;
  UTF8Buffer.add_string buf (Name.internal_utf8_for_class cts.qualified_class_name);
  if cts.type_arguments <> [] then dump_type_arguments buf cts.type_arguments;
  List.iter (dump_class_type_signature_suffix buf) cts.signature_suffix;
  UTF8Buffer.add_char buf semi_colon

and dump_class_type_signatures buf l =
  List.iter (dump_class_type_signature buf) l

and dump_class_type_signature_suffix buf ctss =
  UTF8Buffer.add_char buf dot;
  UTF8Buffer.add_string buf ctss.suffix_identifier;
  if ctss.suffix_type_arguments <> [] then
    dump_type_arguments buf ctss.suffix_type_arguments

and dump_type_variable_signature buf tvs =
  UTF8Buffer.add_char buf capital_t;
  UTF8Buffer.add_string buf tvs;
  UTF8Buffer.add_char buf semi_colon

and dump_type_argument buf = function
  | Star ->
      UTF8Buffer.add_char buf star
  | Plus fts ->
      UTF8Buffer.add_char buf plus;
      dump_field_type_signature buf fts
  | Minus fts ->
      UTF8Buffer.add_char buf minus;
      dump_field_type_signature buf fts
  | Simple fts ->
      dump_field_type_signature buf fts

and dump_type_arguments buf l =
  UTF8Buffer.add_char buf lower_than;
  List.iter (dump_type_argument buf) l;
  UTF8Buffer.add_char buf greater_than

and dump_array_type_signature buf ats =
  UTF8Buffer.add_char buf opening_square_bracket;
  dump_type_signature buf ats

and dump_type_signature buf = function
  | Field_type_signature fts -> dump_field_type_signature buf fts
  | Base_type jt -> dump_base_type buf jt

and dump_base_type buf = function
  | `Boolean -> UTF8Buffer.add_char buf capital_z
  | `Byte -> UTF8Buffer.add_char buf capital_b
  | `Char -> UTF8Buffer.add_char buf capital_c
  | `Double -> UTF8Buffer.add_char buf capital_d
  | `Float -> UTF8Buffer.add_char buf capital_f
  | `Int -> UTF8Buffer.add_char buf capital_i
  | `Long -> UTF8Buffer.add_char buf capital_j
  | `Short -> UTF8Buffer.add_char buf capital_s
  | x -> fail (Invalid_signature_type x)

and dump_method_type_signature buf mts =
  if mts.formal_type_params <> [] then begin
    UTF8Buffer.add_char buf lower_than;
    dump_formal_type_parameters buf mts.formal_type_params;
    UTF8Buffer.add_char buf greater_than
  end;
  UTF8Buffer.add_char buf opening_parenthesis;
  List.iter (dump_type_signature buf) mts.types;
  UTF8Buffer.add_char buf closing_parenthesis;
  dump_return_type buf mts.return;
  List.iter (dump_throws_signature buf) mts.throws_signatures

and dump_return_type buf = function
  | Base_type `Void -> dump_void_descriptor buf
  | Base_type bt -> dump_base_type buf bt
  | Field_type_signature fts -> dump_field_type_signature buf fts

and dump_throws_signature buf = function
  | Throws_class_type_signature cts ->
      UTF8Buffer.add_char buf circonflex;
      dump_class_type_signature buf cts
  | Throws_type_variable_signature tvs ->
      UTF8Buffer.add_char buf circonflex;
      dump_type_variable_signature buf tvs

and dump_void_descriptor buf =
  UTF8Buffer.add_char buf capital_v

let class_signature_of_utf8 s =
  try
    let ls = new lexer_state s in
    let res = parse_class_signature ls in
    if ls#is_available then
      fail (Extra_elements_after_class_signature ls#consume_all)
    else
      res
  with Lexer_state_exception _ -> fail (Invalid_class_signature s)

let utf8_of_class_signature cs =
  let buf = UTF8Buffer.make () in
  dump_class_signature buf cs;
  UTF8Buffer.contents buf

let field_type_signature_of_utf8 s =
  try
    let ls = new lexer_state s in
    let res = parse_field_type_signature ls in
    if ls#is_available then
      fail (Extra_elements_after_field_signature ls#consume_all)
    else
      res
  with Lexer_state_exception _ -> fail (Invalid_field_signature s)

let utf8_of_field_type_signature fts =
  let buf = UTF8Buffer.make () in
  dump_field_type_signature buf fts;
  UTF8Buffer.contents buf

let method_signature_of_utf8 s =
  try
    let ls = new lexer_state s in
    let res = parse_method_type_signature ls in
    if ls#is_available then
      fail (Extra_elements_after_method_signature ls#consume_all)
    else
      res
  with Lexer_state_exception _ -> fail (Invalid_method_signature s)

let utf8_of_method_signature ms =
  let buf = UTF8Buffer.make () in
  dump_method_type_signature buf ms;
  UTF8Buffer.contents buf
