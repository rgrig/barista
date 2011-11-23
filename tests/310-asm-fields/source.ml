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
  let x = { Field.flags = [`Private; `Final];
            Field.name = utf8_for_field "x";
            Field.descriptor = `Int;
            Field.attributes = [] } in
  let y = { Field.flags = [`Private; `Final];
            Field.name = utf8_for_field "y";
            Field.descriptor = `Int;
            Field.attributes = [] } in
  let prefix = { Field.flags = [`Private; `Static];
                 Field.name = utf8_for_field "prefix";
                 Field.descriptor = class_String;
		 Field.attributes = [`ConstantValue (Attribute.String_value (utf8 "prefix: "))] } in
  let clinit_instructions = [
    LDC (`String (utf8 "state: "));
    PUTSTATIC ((utf8_for_class "pack.Test"), (utf8_for_field "prefix"), class_String);
    RETURN ] in
  let init_instructions = [
    ALOAD_0;
    INVOKESPECIAL ((utf8_for_class "java.lang.Object"),
                   (utf8_for_method "<init>"),
                   ([], `Void));
    ALOAD_0;
    ILOAD_1;
    PUTFIELD ((utf8_for_class "pack.Test"), (utf8_for_field "x"), `Int);
    ALOAD_0;
    ILOAD_2;
    PUTFIELD ((utf8_for_class "pack.Test"), (utf8_for_field "y"), `Int);
    RETURN ] in
  let set_instructions = [
    ALOAD_0;
    ILOAD_1;
    PUTFIELD ((utf8_for_class "pack.Test"), (utf8_for_field "x"), `Int);
    ALOAD_0;
    ILOAD_2;
    PUTFIELD ((utf8_for_class "pack.Test"), (utf8_for_field "y"), `Int);
    RETURN ] in
  let tostring_instructions = [
    NEW (utf8_for_class "java.lang.StringBuilder");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.StringBuilder"),
                   (utf8_for_method "<init>"),
                   ([], `Void));
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "prefix"),
               class_String);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "append"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    (`Class (utf8_for_class "java.lang.StringBuilder"))));
    LDC (`String (utf8 "x="));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "append"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    (`Class (utf8_for_class "java.lang.StringBuilder"))));
    ALOAD_0;
    GETFIELD ((utf8_for_class "pack.Test"),
              (utf8_for_field "x"),
              `Int);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "append"),
                   ([`Int],
                    (`Class (utf8_for_class "java.lang.StringBuilder"))));
    LDC (`String (utf8 ", y="));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "append"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    (`Class (utf8_for_class "java.lang.StringBuilder"))));
    ALOAD_0;
    GETFIELD ((utf8_for_class "pack.Test"),
              (utf8_for_field "y"),
              `Int);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "append"),
                   ([`Int],
                    (`Class (utf8_for_class "java.lang.StringBuilder"))));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.StringBuilder")),
                   (utf8_for_method "toString"),
                   ([],
                    class_String));
    ARETURN ] in
  let instructions = [
    NEW (utf8_for_class "pack.Test");
    DUP;
    ICONST_3;
    ICONST_5;
    INVOKESPECIAL ((utf8_for_class "pack.Test"),
                   (utf8_for_method "<init>"),
                   ([`Int; `Int], `Void));
    DUP;
    DUP;
    DUP2;
    POP2;
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.Object"))],
                    `Void));
    SIPUSH (s2 ~-3);
    BIPUSH (s1 7);
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "pack.Test")),
                   (utf8_for_method "set"),
                   ([`Int; `Int], `Void));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.Object"))],
                    `Void));
    RETURN ] in
  let clinit = compile_initializer clinit_instructions in
  let init = compile_constructor  ~signature:[`Int; `Int] init_instructions in
  let set = compile_method ~qualifiers:[`Private] ~name:"set" ~signature:([`Int; `Int], `Void) set_instructions in
  let tostring = compile_method ~qualifiers:[`Public] ~name:"toString" ~signature:([], class_String) tostring_instructions in
  let main = compile_method ~max_stack:(u2 8) instructions in
  let cls = compile_class
      ~parents:[utf8_for_class "java.io.Serializable"]
      ~fields:[x; y; prefix]
      [clinit; init; set; tostring; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
