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
  let n = { Field.flags = [`Public; `Static; `Final];
	    Field.name = utf8_for_field "N";
	    Field.descriptor = `Int;
	    Field.attributes = [`ConstantValue (Attribute.Integer_value 5l)] } in
  let floats_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Float;
    DUP;
    DUP;
    ICONST_1;
    FALOAD;
    FCONST_2;
    FADD;
    ICONST_1;
    SWAP;
    FASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Float], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let doubles_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Double;
    DUP;
    DUP;
    ICONST_1;
    DALOAD;
    DCONST_1;
    DADD;
    DSTORE_0;
    ICONST_1;
    DLOAD_0;
    DASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Double], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let ints_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Int;
    DUP;
    DUP;
    ICONST_1;
    IALOAD;
    ICONST_3;
    IADD;
    ICONST_1;
    SWAP;
    IASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Int], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let longs_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Long;
    DUP;
    DUP;
    ICONST_2;
    LALOAD;
    LCONST_1;
    LADD;
    LSTORE_0;
    ICONST_2;
    LLOAD_0;
    LASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Long], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let bytes_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Byte;
    DUP;
    DUP;
    ICONST_1;
    BALOAD;
    ICONST_3;
    IADD;
    ICONST_1;
    SWAP;
    BASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Byte], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let chars_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Char;
    DUP;
    DUP;
    DUP;
    ICONST_1;
    CALOAD;
    ICONST_3;
    IADD;
    ICONST_1;
    SWAP;
    CASTORE;
    ICONST_2;
    LDC (`Int 90l);
    CASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Char], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let shorts_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    NEWARRAY `Short;
    DUP;
    DUP;
    ICONST_1;
    SALOAD;
    ICONST_3;
    IADD;
    ICONST_1;
    SWAP;
    SASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array `Short], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let strings_instructions = [
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    ANEWARRAY (`Class_or_interface (utf8_for_class "java.lang.String"));
    DUP;
    DUP;
    DUP;
    DUP;
    DUP;
    ICONST_0;
    LDC (`String (utf8 "first"));
    AASTORE;
    ICONST_1;
    LDC (`String (utf8 "second"));
    AASTORE;
    ICONST_2;
    LDC (`String (utf8 "third"));
    AASTORE;
    ICONST_3;
    LDC (`String (utf8 "fourth"));
    AASTORE;
    ICONST_4;
    LDC (`String (utf8 "fifth"));
    AASTORE;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array (`Class (utf8_for_class "java.lang.Object"))], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let matrix_instructions = [
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "matrix:"));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    GETSTATIC ((utf8_for_class "pack.Test"),
               (utf8_for_field "N"),
               `Int);
    DUP;
    MULTIANEWARRAY ((`Array_type (`Array (`Array class_String))), u1 2);
    ASTORE_0;
    ICONST_0;
    ISTORE_1;
    ALOAD_0;
    ARRAYLENGTH;
    ISTORE_2;
    (* loop: *)
    ILOAD_1;
    ILOAD_2;
    IF_ICMPEQ (s2 22);
    ALOAD_0;
    ILOAD_1;
    AALOAD;
    INVOKESTATIC ((utf8_for_class "java.util.Arrays"),
		  (utf8_for_method "toString"),
                  ([`Array (`Class (utf8_for_class "java.lang.Object"))], class_String));
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    IINC (u1 1, s1 1);
    GOTO (s2 ~-21);
    (* end: *)
    RETURN ] in
  let classes_instructions = [
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`Array_type (`Array `Int));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.lang.Object")),
                   (utf8_for_method "toString"),
                   ([], class_String));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let instructions = [
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "floats"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "doubles"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "ints"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "longs"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "bytes"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "chars"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "shorts"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "strings"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "matrix"),
		  ([], `Void));
    INVOKESTATIC ((utf8_for_class "pack.Test"),
		  (utf8_for_method "classes"),
		  ([], `Void));
    RETURN ] in
  let floats = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"floats"
      ~signature:([], `Void)
      floats_instructions in
  let doubles = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"doubles"
      ~signature:([], `Void)
      doubles_instructions in
  let ints = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"ints"
      ~signature:([], `Void)
      ints_instructions in
  let longs = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"longs"
      ~signature:([], `Void)
      longs_instructions in
  let bytes = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"bytes"
      ~signature:([], `Void)
      bytes_instructions in
  let chars = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"chars"
      ~signature:([], `Void)
      chars_instructions in
  let shorts = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"shorts"
      ~signature:([], `Void)
      shorts_instructions in
  let strings = compile_method
      ~max_stack:(u2 8)
      ~qualifiers:[`Private; `Static]
      ~name:"strings"
      ~signature:([], `Void)
      strings_instructions in
  let matrix = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"matrix"
      ~signature:([], `Void)
      matrix_instructions in
  let classes = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"classes"
      ~signature:([], `Void)
      classes_instructions in
  let main = compile_method instructions in
  let cls = compile_class
      ~fields:[n]
      [floats; doubles; ints; longs; bytes; chars; shorts; strings; matrix; classes; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
