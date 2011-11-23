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
open Common
open Instruction
open Utils

let () =
  let prefix = { Field.flags = [`Private; `Static; `Final];
		 Field.name = utf8_for_field "PREFIX";
                 Field.descriptor = class_String;
                 Field.attributes = [`ConstantValue (Attribute.String_value (utf8 "  - << "))] } in
  let suffix = { Field.flags = [`Private; `Static; `Final];
		 Field.name = utf8_for_field "SUFFIX";
		 Field.descriptor = class_String;
                 Field.attributes = [`ConstantValue (Attribute.String_value (utf8 " >>"))] } in
  let print_instructions = [
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    DUP;
    DUP;
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "PREFIX"),
               class_String);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "print"),
                   ([(`Class (utf8_for_class "java.lang.String"))], `Void));
    ALOAD_0;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "print"),
                   ([(`Class (utf8_for_class "java.lang.String"))], `Void));
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "SUFFIX"),
               class_String);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))], `Void));
    RETURN] in
  let instructions = [
    NOP;
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "hello\t... \n\t... \"world\""));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                               (utf8_for_method "println"),
                               ([(`Class (utf8_for_class "java.lang.String"))],
                                `Void));
    ICONST_0;
    ISTORE_1;
    ALOAD_0;
    ARRAYLENGTH;
    ISTORE_2;
    (* loop: *)
    ILOAD_1;
    ILOAD_2;
    IF_ICMPEQ (s2 15); (* end *)
    ALOAD_0;
    ILOAD_1;
    AALOAD;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "print"),
                  ([(`Class (utf8_for_class "java.lang.String"))], `Void));
    IINC (u1 1, s1 1);
    GOTO (s2 ~-14); (* loop *)
    (* end: *)
    RETURN ] in
  let print = compile_method ~name:"print" ~signature:([class_String], `Void) print_instructions in
  let main = compile_method instructions in
  let cls = compile_class ~fields:[prefix; suffix] [print; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
