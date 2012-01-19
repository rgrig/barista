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


(* Constants *)

let opening_bracket = @"<"

let comma_space = @", "

let closing_bracket = @">"

let closing_bracket_space = @"> "

let class' = @"class "

let interface = @"interface "

let extends = @" extends "

let implements = @" implements "

let throws = @" throws "

let space_and_space = @" & "

let array_suffix = @"[]"

let wildcard = @"?"

let wildcard_extends = @"? extends "

let wildcard_super = @"? super "

let true_string = @"true"

let false_string = @"false"

let dot_string = @"."

let equal = @"="

let tab = @"  "

let opening_square_bracket = @"["

let closing_square_bracket = @"]"

let at_string = @"@"

let opening_parenthesis = @"("

let closing_parenthesis = @")"

let semi_colon = @";"

let opening_curly_bracket = @" {"

let closing_curly_bracket = @"}"

let static_block = @""


(* Functions *)

let printed_flag = function
  | `Public
  | `Private
  | `Protected
  | `Static
  | `Final
  | `Synchronized
  | `Volatile
  | `Abstract
  | `Strict
  | `Transient
  | `Native
  | `Module -> true
  | `Super
  | `Bridge
  | `Varargs
  | `Interface
  | `Synthetic
  | `Annotation
  | `Enum -> false

let rec utf8_of_class_signature cs =
  let ftp =
    if cs.Signature.formal_type_parameters <> [] then
      opening_bracket
        ++ (UTF8.concat_sep_map
              comma_space
              utf8_of_formal_type_parameter
              cs.Signature.formal_type_parameters)
        ++ closing_bracket
    else empty_utf8 in
  let scs = utf8_of_class_type_signature cs.Signature.super_class_signature in
  let sis = List.map utf8_of_class_type_signature cs.Signature.super_interface_signatures in
  (ftp, scs, sis)
and utf8_of_formal_type_parameter ftp =
  let fts = utf8_of_field_type_signature ftp.Signature.class_bound in
  ftp.Signature.identifier
    ++ (if UTF8.equal fts java_lang_Object then
      (if ftp.Signature.interface_bounds <> [] then
        extends
          ++ (UTF8.concat_sep_map
                space_and_space
                utf8_of_field_type_signature
                ftp.Signature.interface_bounds)
      else
        empty_utf8)
    else
      extends
        ++ fts
        ++ (UTF8.concat
              (List.map
                 (fun x -> space_and_space ++ (utf8_of_field_type_signature x))
                 ftp.Signature.interface_bounds)))
and utf8_of_field_type_signature = function
  | Signature.Class_type_signature cts -> utf8_of_class_type_signature cts
  | Signature.Array_type_signature ats -> utf8_of_array_type_signature ats
  | Signature.Type_variable_signature tvs -> utf8_of_type_variable_signature tvs
and utf8_of_class_type_signature cts =
  (Name.printable_utf8_for_class cts.Signature.qualified_class_name)
    ++ (if cts.Signature.type_arguments <> [] then
      opening_bracket
        ++ (UTF8.concat_sep_map
              comma_space
              utf8_of_type_argument
              cts.Signature.type_arguments)
        ++ closing_bracket
    else empty_utf8)
and utf8_of_array_type_signature ats =
  (utf8_of_type_signature ats) ++ array_suffix
and utf8_of_type_signature = function
  | Signature.Field_type_signature fts -> utf8_of_field_type_signature fts
  | Signature.Base_type jt -> Descriptor.external_utf8_of_java_type jt
and utf8_of_type_variable_signature tvs = tvs
and utf8_of_type_argument = function
  | Signature.Star -> wildcard
  | Signature.Plus fts ->
      wildcard_extends ++ (utf8_of_field_type_signature fts)
  | Signature.Minus fts ->
      wildcard_super ++ (utf8_of_field_type_signature fts)
  | Signature.Simple fts ->
      utf8_of_field_type_signature fts
and utf8_of_class_type_signature_suffix ctss = utf8_of_class_type_signature ctss
and utf8_of_throws_signature = function
  | Signature.Throws_class_type_signature cts ->
      utf8_of_class_type_signature cts
  | Signature.Throws_type_variable_signature tvs ->
      utf8_of_type_variable_signature tvs

let rec utf8_of_element_value = function
  | Annotation.Boolean_value true -> true_string
  | Annotation.Boolean_value false -> false_string
  | Annotation.Byte_value i -> UTF8.of_string (string_of_int i)
  | Annotation.Char_value c -> UTF8.escape_char c
  | Annotation.Double_value f -> UTF8.of_string (string_of_float f)
  | Annotation.Float_value f -> UTF8.of_string (string_of_float f)
  | Annotation.Int_value i -> UTF8.of_string (Int32.to_string i)
  | Annotation.Long_value i -> UTF8.of_string (Int64.to_string i)
  | Annotation.Short_value i -> UTF8.of_string (string_of_int i)
  | Annotation.String_value s -> UTF8.escape s
  | Annotation.Enum_value (c, i) ->
      (Name.printable_utf8_for_class c)
        ++ dot_string
        ++ (Name.utf8_for_field i)
  | Annotation.Class_value cn -> Name.printable_utf8_for_class cn
  | Annotation.Annotation_value a -> utf8_of_annotation a
  | Annotation.Array_value l  ->
      opening_square_bracket ++
        (UTF8.concat_sep comma_space (List.map utf8_of_element_value l)) ++
        closing_square_bracket
and utf8_of_pair (n, v) =
  n ++ equal ++ (utf8_of_element_value v)
and utf8_of_annotation (name, values) =
  at_string ++ (Name.printable_utf8_for_class name) ++
  (if values <> [] then
    opening_parenthesis ++
      (UTF8.concat_sep comma_space (List.map utf8_of_pair values)) ++
      closing_parenthesis
  else
    empty_utf8)

let add_annotations buffer prefix annotations =
  List.iter
    (fun a -> UTF8Buffer.add_endline buffer (prefix ++ (utf8_of_annotation a)))
    annotations

let add_parents buffer is_interface cd =
  try
    let signature = Attribute.extract_class_signature cd.ClassDefinition.attributes in
    let cs, sc, si = utf8_of_class_signature signature in
    UTF8Buffer.add_string buffer cs;
    if not is_interface then begin
      UTF8Buffer.add_string buffer extends;
      UTF8Buffer.add_string buffer sc
    end;
    if si <> [] then begin
      UTF8Buffer.add_string buffer (if is_interface then extends else implements);
      UTF8Buffer.add_string buffer (UTF8.concat_sep comma_space si)
    end
  with Not_found ->
    (match is_interface, cd.ClassDefinition.extends with
    | false, (Some v) ->
        let name = Name.printable_utf8_for_class v in
        UTF8Buffer.add_string buffer extends;
        UTF8Buffer.add_string buffer name;
    | _ -> ());
    if cd.ClassDefinition.implements <> [] then begin
      UTF8Buffer.add_string buffer (if is_interface then extends else implements);
      UTF8Buffer.add_string
        buffer
        (UTF8.concat_sep_map
           comma_space
           Name.printable_utf8_for_class
           cd.ClassDefinition.implements)
    end

let add_field buffer f =
  add_annotations buffer tab (Attribute.extract_annotations (f.Field.attributes :> Attribute.t list));
  UTF8Buffer.add_string buffer tab;
  UTF8Buffer.add_string
    buffer
    (AccessFlag.list_to_utf8 (List.filter printed_flag (f.Field.flags :> AccessFlag.t list)));
  (try
    let fts = Attribute.extract_field_signature f.Field.attributes in
    UTF8Buffer.add_string buffer (utf8_of_field_type_signature fts)
  with Not_found ->
    UTF8Buffer.add_string buffer (Descriptor.external_utf8_of_java_type (f.Field.descriptor :> Descriptor.java_type)));
  UTF8Buffer.add_char buffer @' ';
  UTF8Buffer.add_string buffer (Name.utf8_for_field f.Field.name);
  UTF8Buffer.add_endline buffer semi_colon

let add_method buffer class_name m =
  let flags, name, desc, attrs =
    match m with
    | Method.Regular mr ->
        mr.Method.flags,
        (Name.utf8_for_method mr.Method.name),
        mr.Method.descriptor,
        mr.Method.attributes
    | Method.Constructor mc ->
        (mc.Method.cstr_flags :> AccessFlag.for_method list),
        class_constructor,
        (mc.Method.cstr_descriptor, `Void),
        mc.Method.cstr_attributes
    | Method.Initializer mi ->
        (mi.Method.init_flags :> AccessFlag.for_method list),
        class_initializer,
        ([], `Void),
        mi.Method.init_attributes in
  add_annotations buffer tab (Attribute.extract_annotations (attrs :> Attribute.t list));
  UTF8Buffer.add_string buffer tab;
  UTF8Buffer.add_string
    buffer
    (AccessFlag.list_to_utf8 (List.filter printed_flag (flags :> AccessFlag.t list)));
  try
    let fts = Attribute.extract_method_signature attrs in
    if fts.Signature.formal_type_params <> [] then begin
      UTF8Buffer.add_char buffer @'<';
      UTF8Buffer.add_string
        buffer
        (UTF8.concat_sep_map
           comma_space
           utf8_of_formal_type_parameter
           fts.Signature.formal_type_params);
      UTF8Buffer.add_string buffer closing_bracket_space
    end;
    if UTF8.equal name class_initializer then
      UTF8Buffer.add_string buffer static_block
    else if UTF8.equal name class_constructor then
      UTF8Buffer.add_string buffer class_name
    else begin
      UTF8Buffer.add_string buffer (utf8_of_type_signature fts.Signature.return);
      UTF8Buffer.add_char buffer @' ';
      UTF8Buffer.add_string buffer name
    end;
    UTF8Buffer.add_char buffer @'(';
    UTF8Buffer.add_string
      buffer
      (UTF8.concat_sep_map
         comma_space
         utf8_of_type_signature
         fts.Signature.types);
    UTF8Buffer.add_char buffer @')';
    if fts.Signature.throws_signatures <> [] then begin
      UTF8Buffer.add_string buffer throws;
      UTF8Buffer.add_string
        buffer
        (UTF8.concat_sep_map
           comma_space
           utf8_of_throws_signature
           fts.Signature.throws_signatures)
    end;
    UTF8Buffer.add_endline buffer semi_colon
  with Not_found ->
    let params, return = desc in
    if UTF8.equal name class_initializer then
      UTF8Buffer.add_string buffer static_block
    else if UTF8.equal name class_constructor then
      UTF8Buffer.add_string buffer class_name
    else begin
      UTF8Buffer.add_string buffer (Descriptor.external_utf8_of_java_type return);
      UTF8Buffer.add_char buffer @' ';
      UTF8Buffer.add_string buffer name
    end;
    UTF8Buffer.add_char buffer @'(';
    UTF8Buffer.add_string
      buffer
      (UTF8.concat_sep_map
         comma_space
         Descriptor.external_utf8_of_java_type
         (params :> Descriptor.java_type list));
    UTF8Buffer.add_char buffer @')';
    (try
      let thrown = Attribute.extract_exceptions (attrs :> Attribute.t list) in
      UTF8Buffer.add_string buffer throws;
      UTF8Buffer.add_string
        buffer
        (UTF8.concat_sep_map
           comma_space
           (fun x -> Name.printable_utf8_for_class x) thrown)
    with Not_found -> ());
    UTF8Buffer.add_endline buffer semi_colon

let print_to_buffer buffer cp s =
  let cl = ClassLoader.make cp in
  let cd = ClassLoader.find_class cl s in
  add_annotations buffer empty_utf8 (Attribute.extract_annotations (cd.ClassDefinition.attributes :> Attribute.t list));
  let flags = (cd.ClassDefinition.access_flags :> AccessFlag.t list) in
  let is_interface = List.mem `Interface flags in
  let flags_to_print = List.filter (fun f ->
    if f = `Abstract then
      not is_interface
    else
      printed_flag f) flags in
  UTF8Buffer.add_string
    buffer
    (AccessFlag.list_to_utf8 (flags_to_print :> AccessFlag.t list));
  let name = Name.printable_utf8_for_class cd.ClassDefinition.name in
  UTF8Buffer.add_string
    buffer
    (if is_interface then interface else class');
  UTF8Buffer.add_string buffer name;
  add_parents buffer is_interface cd;
  UTF8Buffer.add_endline buffer opening_curly_bracket;
  List.iter
    (add_field buffer)
    (List.sort Field.compare cd.ClassDefinition.fields);
  List.iter
    (add_method buffer name)
    (List.sort Method.compare cd.ClassDefinition.methods);
  UTF8Buffer.add_string buffer closing_curly_bracket

let print_to_stream chan cp s =
  let buffer = UTF8Buffer.make () in
  print_to_buffer buffer cp s;
  LineUtils.output chan buffer

let print cp s =
  print_to_stream OutputStream.stdout cp s
