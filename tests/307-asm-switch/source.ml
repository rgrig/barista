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
  let table_instructions = [
    ICONST_0;
    ISTORE_1;
    (* loop: *)
    ILOAD_1;
    DUP;
    LDC (`Int 5l);
    IF_ICMPEQ (s2 73); (* end *)
    TABLESWITCH (s4 48l, s4 0l, s4 2l, [s4 27l; s4 34l; s4 41l]);
    (* zero: *)
    LDC (`String (utf8 "zero"));
    GOTO_W (s4 26l); (* print *)
    (* one: *)
    LDC (`String (utf8 "once"));
    GOTO_W (s4 19l); (* print *)
    (* two: *)
    LDC (`String (utf8 "twice"));
    GOTO_W (s4 12l); (* print *)
    (* default: *)
    LDC (`String (utf8 "many"));
    GOTO_W (s4 5l); (* print *)
    (* print: *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    IINC (u1 1, s1 1);
    GOTO_W (s4 (-72l)); (* loop *)
    (* end: *)
    RETURN ] in
  let lookup_instructions = [
    ICONST_0;
    ISTORE_1;
    (* loop: *)
    ILOAD_1;
    DUP;
    LDC (`Int 5l);
    IF_ICMPEQ (s2 71); (* end *)
    LOOKUPSWITCH (s4 50l, s4 3l, [(s4 0l, s4 35l); (s4 1l, s4 40l); (s4 2l, s4 45l)]);
    (* zero: *)
    LDC (`String (utf8 "zero"));
    GOTO (s2 18); (* print *)
    (* one: *)
    LDC (`String (utf8 "once"));
    GOTO (s2 13); (* print *)
    (* two: *)
    LDC (`String (utf8 "twice"));
    GOTO (s2 8); (* print *)
    (* default: *)
    LDC (`String (utf8 "many"));
    GOTO (s2 3); (* print *)
    (* print: *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    IINC (u1 1, s1 1);
    GOTO (s2 ~-72); (* loop *)
    (* end: *)
    RETURN ] in
  let instructions = [
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "table"),
		  ([], `Void));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "---"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "lookup"),
		  ([], `Void));
    RETURN ] in
  let table = compile_method ~name:"table" ~signature:([], `Void) table_instructions in
  let lookup = compile_method ~name:"lookup" ~signature:([], `Void) lookup_instructions in
  let main = compile_method instructions in
  let cls = compile_class [table; lookup; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
