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
    (* interface *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "interface"));
    INVOKEINTERFACE (((utf8_for_class "java.lang.Appendable"),
		      (utf8_for_method "append"),
		      ([(`Class (utf8_for_class "java.lang.CharSequence"))],
		       (`Class (utf8_for_class "java.lang.Appendable")))), u1 2);

    (* special *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    NEW (utf8_for_class "java.lang.Integer");
    DUP;
    LDC (`String (utf8 "012"));
    INVOKESPECIAL ((utf8_for_class "java.lang.Integer"),
		   (utf8_for_method "<init>"),
		   ([class_String], `Void));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.Object"))],
                    `Void));

    (* static *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "123"));
    INVOKESTATIC ((utf8_for_class "java.lang.Integer"),
		  (utf8_for_method "parseInt"),
		  ([class_String], `Int));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Int)], `Void));

    (* virtual (array type) *)
    ICONST_3;
    NEWARRAY `Int;
    INVOKEVIRTUAL ((`Array_type (`Array `Int)),
                   (utf8_for_method "toString"),
                   ([], (`Class (utf8_for_class "java.lang.String"))));
    POP;

    (* virtual (over class) *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "virtual"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RETURN ] in
  let main = compile_method ~max_stack:(u2 5) instructions in
  let cls = compile_class [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
