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

let mk_table l =
  List.map
    (fun (x, y, z, t) ->
      { Attribute.try_start = x;
        Attribute.try_end = y;
        Attribute.catch = z;
        Attribute.caught = t; })
    l

let () =
  let catch_instructions = [
    NEW (utf8_for_class "java.lang.StringBuilder");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.StringBuilder"),
		   (utf8_for_method "<init>"),
		   ([], `Void));
    CHECKCAST (`Array_type (`Array `Int));
    LDC (`String (utf8 "uncaught"));
    GOTO (s2 5);
    LDC (`String (utf8 "caught"));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RETURN ] in
  let catch_npe_instructions = [
    ACONST_NULL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.Object")),
                   (utf8_for_method "toString"),
                   ([], class_String));
    LDC (`String (utf8 "uncaught"));
    GOTO (s2 13);
    LDC (`String (utf8 "caught"));
    GOTO (s2 8);
    LDC (`String (utf8 "wrong caught"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RETURN ] in
  let throw_instructions = [
    NEW (utf8_for_class "java.lang.ClassCastException");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.ClassCastException"),
		   (utf8_for_method "<init>"),
		   ([], `Void));
    ATHROW;
    LDC (`String (utf8 "uncaught"));
    GOTO (s2 5);
    LDC (`String (utf8 "caught"));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RETURN ] in
  let finally_instructions = [
    ACONST_NULL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.Object")),
                   (utf8_for_method "toString"),
                   ([], class_String));
    JSR (s2 8);
    RETURN;
    JSR (s2 4);
    RETURN;
    ASTORE_1;
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "finally"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RET (u1 1);
    RETURN ] in
  let finally_w_instructions = [
    ACONST_NULL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.Object")),
                   (utf8_for_method "toString"),
                   ([], class_String));
    JSR_W (s4 12l);
    RETURN;
    JSR_W (s4 6l);
    RETURN;
    ASTORE_1;
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "finally"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    RET (u1 1);
    RETURN ] in
  let instructions = [
    INVOKESTATIC ((utf8_for_class "pack.Test"), (utf8_for_method "catch"), ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"), (utf8_for_method "catch_npe"), ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"), (utf8_for_method "throw"), ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"), (utf8_for_method "finally"), ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"), (utf8_for_method "finally_w"), ([], `Void));
    RETURN ] in
  let catch = compile_method
      ~qualifiers:[`Public; `Static]
      ~name:"catch"
      ~signature:([], `Void)
      ~exceptions_table:(mk_table [(u2 0, u2 15, u2 15, None)])
      catch_instructions in
  let catch_npe = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"catch_npe"
      ~signature:([], `Void)
      ~exceptions_table:(mk_table [(u2 0, u2 9, u2 9, Some (utf8_for_class "java.lang.NullPointerException"));
			           (u2 0, u2 9, u2 14, Some (utf8_for_class "java.lang.ArrayIndexOutOfBoundsException"))])
      catch_npe_instructions in
  let throw = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"throw"
      ~signature:([], `Void)
      ~exceptions_table:(mk_table [(u2 0, u2 13, u2 13, None)])
      throw_instructions in
  let finally = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"finally"
      ~signature:([], `Void)
      ~exceptions_table:(mk_table [(u2 0, u2 8, u2 8, None)])
      finally_instructions in
  let finally_w = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"finally_w"
      ~signature:([], `Void)
      ~exceptions_table:(mk_table [(u2 0, u2 10, u2 10, None)])
      finally_w_instructions in
  let main = compile_method instructions in
  let cls = compile_class ~version:Version.Java_1_5 [catch; catch_npe; throw; finally; finally_w; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
