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

let utf8 = Utils.UTF8.of_string
let utf8_for_class x = Name.make_for_class_from_external (utf8 x)
let utf8_for_field x = Name.make_for_field (utf8 x)
let utf8_for_method x = Name.make_for_method (utf8 x)

let class_String = `Class (utf8_for_class "java.lang.String")
let class_PrintStream = `Class (utf8_for_class "java.io.PrintStream")

let loader = ClassLoader.make (ClassPath.make ())

let generate_stack_map_if_needed version class_name init_state instructions exception_table code_attrs =
  let rec contains_stack_map (l : Attribute.code_attribute list) =
    match l with
    | (`StackMapTable _) :: _ -> true
    | _ :: tl -> contains_stack_map tl
    | [] -> false in
  if not (contains_stack_map code_attrs) && (version >= Version.Java_1_7) then begin
    let graph =
      ControlFlow.graph_of_instructions
        instructions
        exception_table in
    let _, _, stack_map_frame =
      Code.compute_stack_infos
        (StackState.unify_to_closest_common_parent loader
           [class_name, Some (utf8_for_class "java.lang.Object")])
        graph
        init_state in
    (`StackMapTable stack_map_frame) :: code_attrs
  end else
    code_attrs

let compile_method
    ?(version = Version.default)
    ?(class_name = utf8_for_class "pack.Test")
    ?(qualifiers = [`Public; `Static])
    ?(name = "main")
    ?(signature = ([`Array (`Class (utf8_for_class "java.lang.String"))], `Void))
    ?(max_stack = u2 4)
    ?(max_locals = u2 4)
    ?(exceptions_table = [])
    ?(code_attributes = [])
    ?(meth_attributes = [])
    instructions =
  let open Method in
  let init_state =
    let m = Regular { flags = qualifiers;
                      name = utf8_for_method name;
                      descriptor = signature;
                      attributes = []; } in
    StackState.make_of_method class_name m in
  let code = {
    Attribute.max_stack = max_stack;
    Attribute.max_locals = max_locals;
    Attribute.code = instructions;
    Attribute.exception_table = exceptions_table;
    Attribute.attributes =
    generate_stack_map_if_needed
      version
      class_name
      init_state
      instructions
      exceptions_table
      code_attributes;
  } in
  Regular { flags = qualifiers;
            name = utf8_for_method name;
            descriptor = signature;
            attributes = [`Code code] @ meth_attributes; }

let compile_constructor
    ?(qualifiers = [`Public])
    ?(signature = [])
    ?(max_stack = u2 4)
    ?(max_locals = u2 4)
    ?(exceptions_table = [])
    ?(code_attributes = [])
    instructions =
  let code = {
    Attribute.max_stack = max_stack;
    Attribute.max_locals = max_locals;
    Attribute.code = instructions;
    Attribute.exception_table = exceptions_table;
    Attribute.attributes = code_attributes;
  } in
  let open Method in
  Constructor { cstr_flags = qualifiers;
                cstr_descriptor = signature;
                cstr_attributes = [`Code code] }

let compile_initializer
    ?(qualifiers = [`Static])
    ?(max_stack = u2 4)
    ?(max_locals = u2 4)
    ?(exceptions_table = [])
    ?(code_attributes = [])
    instructions =
  let code = {
    Attribute.max_stack = max_stack;
    Attribute.max_locals = max_locals;
    Attribute.code = instructions;
    Attribute.exception_table = exceptions_table;
    Attribute.attributes = code_attributes;
  } in
  let open Method in
  Initializer { init_flags = qualifiers;
                init_attributes = [`Code code] }

let compile_class
    ?(version = Version.default)
    ?(qualifiers = [`Public; `Super; `Final])
    ?(name = "pack.Test")
    ?(parent = Some (utf8_for_class "java.lang.Object"))
    ?(parents = [])
    ?(fields = [])
    ?(attributes = [])
    methods =
  let cls = {
    ClassDefinition.access_flags = qualifiers;
    ClassDefinition.name = utf8_for_class name;
    ClassDefinition.extends = parent;
    ClassDefinition.implements = parents;
    ClassDefinition.fields = fields;
    ClassDefinition.methods = methods;
    ClassDefinition.attributes = attributes;
  } in
  ClassDefinition.encode ~version:version cls
