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

BARISTA_ERROR =
  | Invalid_desciptor of (s : UTF8.t) ->
      Printf.sprintf "invalid method descriptor %S" (UTF8.to_string_noerr s)
  | Method_not_found -> "method not found"
  | Method_has_no_code -> "method has no code"

let print_to_buffer buffer ld s =
  let class_name, method_name, (method_params, method_ret) =
    try
      let tokens = Lexer.tokens_of_line s in
      match tokens with
      | [ Lexer.Method (cn, mn, md) ] -> cn, mn, md
      | _ -> fail (Invalid_desciptor s)
    with _ -> fail (Invalid_desciptor s) in
  let clas = ClassLoader.find_class ld (Name.external_utf8_for_class class_name) in
  let meth =
    try
      let method_name = Name.utf8_for_method method_name in
      let is_constructor = UTF8.equal method_name Consts.class_constructor in
      let is_initializer = UTF8.equal method_name Consts.class_initializer in
      let eq_type = Descriptor.equal_java_type in
      let eq_params l l' =
        list_equal
          ~eq:eq_type
          (l :> Descriptor.java_type list)
          (l' :> Descriptor.java_type list) in
      List.find
        (function
          | Method.Regular { Method.name = name; descriptor = (params, ret); _ } ->
              (UTF8.equal (Name.utf8_for_method name) method_name)
                && (eq_params method_params params)
                && (eq_type method_ret ret)
          | Method.Constructor { Method.cstr_descriptor = params; _ } ->
              is_constructor
                && (eq_params method_params params)
                && (method_ret = `Void)
          | Method.Initializer _ ->
              is_initializer && (method_params = []) && (method_ret = `Void))
        clas.ClassDefinition.methods
    with Not_found -> fail Method_not_found in
  let code =
    try
      Attribute.extract_code
        ((match meth with
        | Method.Regular { Method.attributes = a; _ } -> a
        | Method.Constructor { Method.cstr_attributes = a; _ } -> a
        | Method.Initializer { Method.init_attributes = a; _ } -> a) :> Attribute.t list)
    with _ -> fail Method_has_no_code in
  let graph =
    ControlFlow.graph_of_instructions
      code.Attribute.code
      code.Attribute.exception_table in
  let dot =
    ControlFlow.dot_of_graph
      (fun (ofs, _) -> Printf.sprintf "[offset %ld]" ofs)
      (fun _ -> "")
      graph in
  let dot = UTF8.of_string dot in
  UTF8Buffer.add_string buffer dot

let print_to_stream chan ld s =
  let buffer = UTF8Buffer.make () in
  print_to_buffer buffer ld s;
  LineUtils.output chan buffer

let print ld s =
  print_to_stream OutputStream.stdout ld s
