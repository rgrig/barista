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
  let instructions = [
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "hello"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let lvt = {
    Attribute.local_start = u2 0;
    Attribute.local_length = u2 9;
    Attribute.local_name = utf8 "args";
    Attribute.local_descriptor = `Array class_String;
    Attribute.local_index = u2 0;
  } in
  let main = compile_method
      ~version:Version.Java_1_6
      ~meth_attributes:[`Deprecated;
			`Exceptions [(utf8_for_class "java.io.IOException");
				     (utf8_for_class "java.lang.RuntimeException")]]
      ~code_attributes:[`LineNumberTable [(u2 0, u2 10); (u2 3, u2 20); (u2 5, u2 30); (u2 8, u2 40)];
			`LocalVariableTable [lvt]]
      instructions in
  let cls = compile_class [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
