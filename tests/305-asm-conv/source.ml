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

let () =
  let instructions = [
    (* d2f *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    DCONST_1;
    D2F;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* d2i *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    DCONST_1;
    D2I;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* d2l *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    DCONST_1;
    D2L;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* f2d *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    FCONST_0;
    F2D;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* f2i *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    FCONST_0;
    F2I;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* f2l *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    FCONST_0;
    F2L;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* i2b *)
    ICONST_5;
    I2B;
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* i2c *)
    ICONST_5;
    I2C;
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* i2d *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    ICONST_5;
    I2D;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* i2f *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    ICONST_5;
    I2F;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* i2l *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    ICONST_5;
    I2L;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Long], `Void));

    (* i2s *)
    ICONST_5;
    I2S;
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    (* l2d *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LCONST_1;
    L2D;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Double], `Void));

    (* l2f *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LCONST_1;
    L2F;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Float], `Void));

    (* l2i *)
    GETSTATIC ((utf8_for_class "java.lang.System"), (utf8_for_field "out"), class_PrintStream);
    LCONST_1;
    L2I;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([`Int], `Void));

    RETURN ] in
  let main = compile_method instructions in
  let cls = compile_class [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
