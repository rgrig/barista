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
    (* ddiv *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Double 7.5);
    LDC2_W (`Double 2.5);
    DDIV;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* dmul *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Double 1.5);
    LDC2_W (`Double 5.0);
    DMUL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* dneg *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Double 1.5);
    DNEG;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* drem *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Double 5.0);
    LDC2_W (`Double 1.5);
    DREM;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* dsub *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Double 5.0);
    LDC2_W (`Double 1.5);
    DSUB;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* fdiv *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Float 7.5);
    LDC (`Float 2.5);
    FDIV;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* fmul *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Float 1.5);
    LDC (`Float 5.0);
    FMUL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* fneg *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Float 1.5);
    FNEG;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* frem *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Float 5.0);
    LDC (`Float 1.5);
    FREM;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* fsub *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Float 5.0);
    LDC (`Float 1.5);
    FSUB;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* iand *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 0xFCl);
    LDC (`Int 0xABl);
    IAND;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* idiv *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 7l);
    IDIV;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* imul *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 6l);
    LDC (`Int 7l);
    IMUL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* ineg *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 7l);
    INEG;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* ior *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 4l);
    LDC (`Int 3l);
    IOR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* irem *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 7l);
    IREM;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* ishl *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 2l);
    ISHL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* ishr *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 2l);
    ISHR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* isub *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 17l);
    ISUB;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* iushr *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 24l);
    LDC (`Int 2l);
    IUSHR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* ixor *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC (`Int 27l);
    LDC (`Int 8l);
    IXOR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* land *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 0xFCL);
    LDC2_W (`Long 0xABL);
    LAND;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* ldiv *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC2_W (`Long 7L);
    LDIV;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lmul *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 6L);
    LDC2_W (`Long 7L);
    LMUL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lneg *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 7L);
    LNEG;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lor *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 4L);
    LDC2_W (`Long 3L);
    LOR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lrem *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC2_W (`Long 7L);
    LREM;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lshl *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC (`Int 2l);
    LSHL;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lshr *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC (`Int 2l);
    LSHR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lsub *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC2_W (`Long 17L);
    LSUB;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lushr *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 24L);
    LDC (`Int 2l);
    LUSHR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* lxor *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LDC2_W (`Long 27L);
    LDC2_W (`Long 8L);
    LXOR;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    RETURN ] in
  let main = compile_method ~max_stack:(u2 5) instructions in
  let cls = compile_class [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
