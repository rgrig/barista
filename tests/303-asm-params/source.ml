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
  let sum_floats_instructions = [
    FLOAD_0;
    FLOAD_1;
    FLOAD_2;
    FLOAD_3;
    FLOAD (u1 4);
    FADD;
    FADD;
    FADD;
    FADD;
    FRETURN ] in
  let sum_doubles_instructions = [
    DLOAD_0;
    DLOAD_2;
    DLOAD (u1 4);
    DADD;
    DADD;
    DRETURN ] in
  let sum_ints_instructions = [
    ILOAD_0;
    ILOAD_1;
    ILOAD_2;
    ILOAD_3;
    ILOAD (u1 4);
    ILOAD (u1 5);
    ILOAD (u1 6);
    IADD;
    IADD;
    IADD;
    IADD;
    IADD;
    IADD;
    IRETURN ] in
  let sum_longs_instructions = [
    LLOAD_0;
    LLOAD_2;
    LLOAD (u1 4);
    LADD;
    LADD;
    LRETURN ] in
  let instructions = [
    (* float *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
	       (utf8_for_field "out"),
	       class_PrintStream);
    FCONST_0;
    FCONST_1;
    FCONST_1;
    FCONST_2;
    FCONST_2;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "sum"),
                  ([`Float; `Float; `Float; `Float; `Float], `Float));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                               (utf8_for_method "println"),
                               ([`Float], `Void));

    (* doubles *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
	       (utf8_for_field "out"),
	       class_PrintStream);
    DCONST_0;
    DCONST_1;
    DCONST_1;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "sum"),
                  ([`Double; `Double; `Double; ], `Double));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                               (utf8_for_method "println"),
                               ([`Double], `Void));

    (* ints *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
	       (utf8_for_field "out"),
	       class_PrintStream);
    ICONST_0;
    ICONST_1;
    ICONST_2;
    ICONST_3;
    ICONST_4;
    ICONST_5;
    ICONST_M1;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "sum"),
                  ([`Int; `Int; `Int; `Int; `Int; `Int; `Int], `Int));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                               (utf8_for_method "println"),
                               ([`Int], `Void));

    (* longs *)
    GETSTATIC ((utf8_for_class "java.lang.System"),
	       (utf8_for_field "out"),
	       class_PrintStream);
    LCONST_0;
    LCONST_1;
    LCONST_1;
    INVOKESTATIC ((utf8_for_class "pack.Test"),
                  (utf8_for_method "sum"),
                  ([`Long; `Long; `Long], `Long));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                               (utf8_for_method "println"),
                               ([`Long], `Void));

    RETURN ] in
  let sum_floats = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"sum"
      ~signature:([`Float; `Float; `Float; `Float; `Float], `Float)
      ~max_stack:(u2 5)
      ~max_locals:(u2 5)
      sum_floats_instructions in
  let sum_doubles = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"sum"
      ~signature:([`Double; `Double; `Double], `Double)
      ~max_stack:(u2 6)
      ~max_locals:(u2 6)
      sum_doubles_instructions in
  let sum_ints = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"sum"
      ~signature:([`Int; `Int; `Int; `Int; `Int; `Int; `Int], `Int)
      ~max_stack:(u2 7)
      ~max_locals:(u2 7)
      sum_ints_instructions in
  let sum_longs = compile_method
      ~qualifiers:[`Private; `Static]
      ~name:"sum"
      ~signature:([`Long; `Long; `Long], `Long)
      ~max_stack:(u2 6)
      ~max_locals:(u2 6)
      sum_longs_instructions in
  let main = compile_method ~max_stack:(u2 8) instructions in
  let cls = compile_class [sum_floats; sum_doubles; sum_ints; sum_longs; main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
