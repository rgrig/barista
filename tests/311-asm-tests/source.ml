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
  let result_instructions = [
    ILOAD_0;
    LOOKUPSWITCH (s4 50l, s4 3l, [(s4 (-1l), s4 35l); (s4 0l, s4 40l); (s4 1l, s4 45l)]);
    LDC (`String (utf8 "lower"));
    GOTO (s2 18);
    LDC (`String (utf8 "equal"));
    GOTO (s2 13);
    LDC (`String (utf8 "greater"));
    GOTO (s2 8);
    LDC (`String (utf8 "default"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let double_instructions = [
    LDC2_W (`Double 1.2);
    LDC2_W (`Double 3.4);
    DCMPG;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                   (utf8_for_method "result"),
                   ([`Int], `Void));
    LDC2_W (`Double 5.6);
    LDC2_W (`Double 3.4);
    DCMPL;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                   (utf8_for_method "result"),
                   ([`Int], `Void));
    RETURN ] in
  let float_instructions = [
    LDC (`Float 1.2);
    LDC (`Float 1.2);
    FCMPG;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                   (utf8_for_method "result"),
                   ([`Int], `Void));
    LDC (`Float 5.6);
    LDC (`Float 3.4);
    FCMPL;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                   (utf8_for_method "result"),
                   ([`Int], `Void));
    RETURN ] in
  let long_instructions = [
    LDC2_W (`Long 12L);
    LDC2_W (`Long 34L);
    LCMP;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                   (utf8_for_method "result"),
                   ([`Int], `Void));
    LDC2_W (`Long 56L);
    LDC2_W (`Long 34L);
    LCMP;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "result"),
                  ([`Int], `Void));
    RETURN ] in
  let instanceof_instructions = [
    NEW (utf8_for_class "java.lang.String");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.String"),
		   (utf8_for_method "<init>"),
                   ([], `Void));
    INSTANCEOF (`Class_or_interface (utf8_for_class "java.lang.Integer"));
    IFNE (s2 8);
    LDC (`String (utf8 "not inst"));
    GOTO (s2 5);
    LDC (`String (utf8 "inst"));
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let null_instructions = [
    ALOAD_0;
    IFNULL (s2 10);
    ALOAD_0;
    IFNONNULL (s2 11);
    GOTO (s2 13);
    LDC (`String (utf8 "null"));
    GOTO (s2 13);
    LDC (`String (utf8 "non null"));
    GOTO (s2 8);
    LDC (`String (utf8 "error"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let ref_eq_instructions = [
    ALOAD_0;
    ALOAD_1;
    IF_ACMPEQ (s2 13);
    ALOAD_0;
    ALOAD_1;
    IF_ACMPNE (s2 13);
    LDC (`String (utf8 "error"));
    GOTO (s2 13);
    LDC (`String (utf8 "equal references"));
    GOTO (s2 8);
    LDC (`String (utf8 "different references"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let int_eq_instructions = [
    ILOAD_0;
    ILOAD_1;
    IF_ICMPEQ (s2 33);
    ILOAD_0;
    ILOAD_1;
    IF_ICMPNE (s2 33);
    ILOAD_0;
    ILOAD_1;
    IF_ICMPLT (s2 33);
    ILOAD_0;
    ILOAD_1;
    IF_ICMPLE (s2 33);
    ILOAD_0;
    ILOAD_1;
    IF_ICMPGT (s2 33);
    ILOAD_0;
    ILOAD_1;
    IF_ICMPGE (s2 33);
    LDC (`String (utf8 "error"));
    GOTO (s2 33);
    LDC (`String (utf8 "eq"));
    GOTO (s2 28);
    LDC (`String (utf8 "ne"));
    GOTO (s2 23);
    LDC (`String (utf8 "lt/never reached"));
    GOTO (s2 18);
    LDC (`String (utf8 "le/never reached"));
    GOTO (s2 13);
    LDC (`String (utf8 "gt/never reached"));
    GOTO (s2 8);
    LDC (`String (utf8 "ge/never reached"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let int_eq0_instructions = [
    ILOAD_0;
    IFEQ (s2 28);
    ILOAD_0;
    IFNE (s2 29);
    ILOAD_0;
    IFLT (s2 30);
    ILOAD_0;
    IFLE (s2 31);
    ILOAD_0;
    IFGT (s2 32);
    ILOAD_0;
    IFGE (s2 33);
    LDC (`String (utf8 "error"));
    GOTO (s2 33);
    LDC (`String (utf8 "eq"));
    GOTO (s2 28);
    LDC (`String (utf8 "ne"));
    GOTO (s2 23);
    LDC (`String (utf8 "lt/never reached"));
    GOTO (s2 18);
    LDC (`String (utf8 "le/never reached"));
    GOTO (s2 13);
    LDC (`String (utf8 "gt/never reached"));
    GOTO (s2 8);
    LDC (`String (utf8 "ge/never reached"));
    GOTO (s2 3);
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let instructions = [
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "double"),
                  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "float"),
                  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "long"),
                  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "instanceof"),
                  ([], `Void));

    ACONST_NULL;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "null"),
                  ([`Class (utf8_for_class "java.lang.Object")], `Void));

    NEW (utf8_for_class "java.lang.String");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.String"),
		   (utf8_for_method "<init>"),
                   ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "null"),
                  ([`Class (utf8_for_class "java.lang.Object")], `Void));

    ACONST_NULL;
    NEW (utf8_for_class "java.lang.String");
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.String"),
		   (utf8_for_method "<init>"),
                  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "ref_eq"),
                  ([`Class (utf8_for_class "java.lang.Object"); `Class (utf8_for_class "java.lang.Object")], `Void));

    NEW (utf8_for_class "java.lang.String");
    DUP;
    DUP;
    INVOKESPECIAL ((utf8_for_class "java.lang.String"),
		   (utf8_for_method "<init>"),
                  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "ref_eq"),
                  ([`Class (utf8_for_class "java.lang.Object"); `Class (utf8_for_class "java.lang.Object")], `Void));

    LDC (`Int 3l);
    LDC (`Int 4l);
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq"),
                  ([`Int; `Int], `Void));

    LDC (`Int 3l);
    LDC (`Int (-4l));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq"),
                  ([`Int; `Int], `Void));

    LDC (`Int 3l);
    LDC (`Int 3l);
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq"),
                  ([`Int; `Int], `Void));

    LDC (`Int 4l);
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq0"),
                  ([`Int], `Void));

    LDC (`Int (-4l));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq0"),
                  ([`Int], `Void));

    LDC (`Int 0l);
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "int_eq0"),
                  ([`Int], `Void));

    RETURN ] in
  let result = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"result"
      ~signature:([`Int], `Void)
      result_instructions in
  let double = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"double"
      ~signature:([], `Void)
      double_instructions in
  let float = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"float"
      ~signature:([], `Void)
      float_instructions in
  let long = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"long"
      ~signature:([], `Void)
      long_instructions in
  let instanceof = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"instanceof"
      ~signature:([], `Void)
      instanceof_instructions in
  let null = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"null"
      ~signature:([`Class (utf8_for_class "java.lang.Object")], `Void)
      null_instructions in
  let ref_eq = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"ref_eq"
      ~signature:([`Class (utf8_for_class "java.lang.Object");
		   `Class (utf8_for_class "java.lang.Object")], `Void)
      ref_eq_instructions in
  let int_eq = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"int_eq"
      ~signature:([`Int; `Int], `Void)
      int_eq_instructions in
  let int_eq0 = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"int_eq0"
      ~signature:([`Int], `Void)
      int_eq0_instructions in
  let main = compile_method ~max_stack:(u2 5) instructions in
  let cls = compile_class [result; double; float; long; instanceof; null; ref_eq; int_eq; int_eq0; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
