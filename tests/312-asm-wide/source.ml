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
    ICONST_0;
    WIDE_ISTORE (u2 1);
    ALOAD_0;
    ARRAYLENGTH;
    WIDE_ISTORE (u2 2);
    (* loop: *)
    WIDE_ILOAD (u2 1);
    WIDE_ILOAD (u2 2);
    IF_ICMPEQ (s2 28); (* end *)
    WIDE_ALOAD (u2 0);
    WIDE_ILOAD (u2 1);
    AALOAD;
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    SWAP;
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([(`Class (utf8_for_class "java.lang.String"))],
                    `Void));
    WIDE_IINC (u2 1, s2 1);
    GOTO (s2 ~-33); (* loop *)
    (* end: *)
    RETURN ] in
  let main = compile_method instructions in
  let cls = compile_class [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))
