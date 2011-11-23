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


open Utils


(* Rewriting rules *)

type rewriting_rules = (u2 * Instruction.t) list -> (u2 * Instruction.t) list

let s1_zero = s1 0

let s2_zero = s2 0

let is_commutative_int_operation = function
  | Instruction.IADD | Instruction.IMUL
  | Instruction.IAND | Instruction.IOR | Instruction.IXOR -> true
  | _ -> false

let is_commutative_long_operation = function
  | Instruction.LADD | Instruction.LMUL
  | Instruction.LAND | Instruction.LOR | Instruction.LXOR -> true
  | _ -> false

let is_category1_field (_, _, d) =
  match d with
  | `Double | `Long -> false
  | _ -> true

let is_category2_field (_, _, d) =
  match d with
  | `Double | `Long -> true
  | _ -> false

let same_field (c1, f1, d1) (c2, f2, d2) =
  (Name.equal_for_class c1 c2)
    && (Name.equal_for_field f1 f2)
    && (Descriptor.equal_java_type (d1 :> Descriptor.java_type) (d2 :> Descriptor.java_type))

let same_method (c1, m1, d1) (c2, m2, d2) =
  (Name.equal_for_class c1 c2)
    && (Name.equal_for_method m1 m2)
    && (Descriptor.equal_for_method d1 d2)

let same_cast c1 c2 =
  match (c1, c2) with
  | `Array_type at1, `Array_type at2 ->
      Descriptor.equal_java_type
        (at1 :> Descriptor.java_type)
        (at2 :> Descriptor.java_type)
  | `Class_or_interface cn1, `Class_or_interface cn2 ->
      Name.equal_for_class cn1 cn2
  | _ -> false

let is_dload = function
  | Instruction.DLOAD_0
  | Instruction.DLOAD_1
  | Instruction.DLOAD_2
  | Instruction.DLOAD_3
  | Instruction.DLOAD _
  | Instruction.WIDE_DLOAD _ -> true
  | _ -> false

let is_fload = function
  | Instruction.FLOAD_0
  | Instruction.FLOAD_1
  | Instruction.FLOAD_2
  | Instruction.FLOAD_3
  | Instruction.FLOAD _
  | Instruction.WIDE_FLOAD _ -> true
  | _ -> false

let is_iload = function
  | Instruction.ILOAD_0
  | Instruction.ILOAD_1
  | Instruction.ILOAD_2
  | Instruction.ILOAD_3
  | Instruction.ILOAD _
  | Instruction.WIDE_ILOAD _ -> true
  | _ -> false

let is_lload = function
  | Instruction.LLOAD_0
  | Instruction.LLOAD_1
  | Instruction.LLOAD_2
  | Instruction.LLOAD_3
  | Instruction.LLOAD _
  | Instruction.WIDE_LLOAD _ -> true
  | _ -> false

let extract_iload_index = function
  | Instruction.ILOAD_0 -> 0
  | Instruction.ILOAD_1 -> 1
  | Instruction.ILOAD_2 -> 2
  | Instruction.ILOAD_3 -> 3
  | Instruction.ILOAD n -> (n :> int)
  | Instruction.WIDE_ILOAD n -> (n :> int)
  | _ -> invalid_arg "BaristaLibrary.Peephole.extract_iload_index"

let extract_iload_index_u1 x =
  u1 (extract_iload_index x)

let extract_iload_index_u2 x =
  u2 (extract_iload_index x)

let extract_istore_index = function
  | Instruction.ISTORE_0 -> 0
  | Instruction.ISTORE_1 -> 1
  | Instruction.ISTORE_2 -> 2
  | Instruction.ISTORE_3 -> 3
  | Instruction.ISTORE n -> (n :> int)
  | Instruction.WIDE_ISTORE n -> (n :> int)
  | _ -> invalid_arg "BaristaLibrary.Peephole.extract_istore_index"

let load_store_same_index instr1 instr2 =
  try
    (extract_iload_index instr1) = (extract_istore_index instr2)
  with _ -> false

let optimize_constants l =
  let rec rewrite acc = function
  | (line, (Instruction.LDC2_W (`Double 0.0))) :: tl -> rewrite ((line, Instruction.DCONST_0) :: acc) tl
  | (line, (Instruction.LDC2_W (`Double 1.0))) :: tl -> rewrite ((line, Instruction.DCONST_1) :: acc) tl
  | (line, (Instruction.LDC (`Float 0.0))) :: tl -> rewrite ((line, Instruction.FCONST_0) :: acc) tl
  | (line, (Instruction.LDC (`Float 1.0))) :: tl -> rewrite ((line, Instruction.FCONST_1) :: acc) tl
  | (line, (Instruction.LDC_W (`Float 0.0))) :: tl -> rewrite ((line, Instruction.FCONST_0) :: acc) tl
  | (line, (Instruction.LDC_W (`Float 1.0))) :: tl -> rewrite ((line, Instruction.FCONST_1) :: acc) tl
  | (line, (Instruction.LDC (`Int 0l))) :: tl -> rewrite ((line, Instruction.ICONST_0) :: acc) tl
  | (line, (Instruction.LDC (`Int 1l))) :: tl -> rewrite ((line, Instruction.ICONST_1) :: acc) tl
  | (line, (Instruction.LDC (`Int 2l))) :: tl -> rewrite ((line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.LDC (`Int 3l))) :: tl -> rewrite ((line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.LDC (`Int 4l))) :: tl -> rewrite ((line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.LDC (`Int 5l))) :: tl -> rewrite ((line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.LDC (`Int (-1l)))) :: tl -> rewrite ((line, Instruction.ICONST_M1) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ICONST_0) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ICONST_1) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 4 -> rewrite ((line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = 5 -> rewrite ((line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: tl when (x :> int) = -1 -> rewrite ((line, Instruction.ICONST_M1) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ICONST_0) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ICONST_1) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 4 -> rewrite ((line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = 5 -> rewrite ((line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: tl when (x :> int) = -1 -> rewrite ((line, Instruction.ICONST_M1) :: acc) tl
  | (line, (Instruction.LDC (`Int x))) :: tl when (x >= -128l) && (x <= 127l) -> rewrite ((line, (Instruction.BIPUSH (s1 (Int32.to_int x)))) :: acc) tl
  | (line, (Instruction.LDC (`Int x))) :: tl when (x >= -32768l) && (x <= 32767l) -> rewrite ((line, (Instruction.SIPUSH (s2 (Int32.to_int x)))) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 0l))) :: tl -> rewrite ((line, Instruction.ICONST_0) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 1l))) :: tl -> rewrite ((line, Instruction.ICONST_1) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 2l))) :: tl -> rewrite ((line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 3l))) :: tl -> rewrite ((line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 4l))) :: tl -> rewrite ((line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.LDC_W (`Int 5l))) :: tl -> rewrite ((line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.LDC_W (`Int (-1l)))) :: tl -> rewrite ((line, Instruction.ICONST_M1) :: acc) tl
  | (line, (Instruction.LDC_W (`Int x))) :: tl when (x >= -128l) && (x <= 127l) -> rewrite ((line, (Instruction.BIPUSH (s1 (Int32.to_int x)))) :: acc) tl
  | (line, (Instruction.LDC_W (`Int x))) :: tl when (x >= -32768l) && (x <= 32767l) -> rewrite ((line, (Instruction.SIPUSH (s2 (Int32.to_int x)))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 0L))) :: tl -> rewrite ((line, Instruction.LCONST_0) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 1L))) :: tl -> rewrite ((line, Instruction.LCONST_1) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let optimize_locals l =
  let rec rewrite acc = function
  | (line, (Instruction.WIDE_ALOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ALOAD_0) :: acc) tl
  | (line, (Instruction.WIDE_ALOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ALOAD_1) :: acc) tl
  | (line, (Instruction.WIDE_ALOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ALOAD_2) :: acc) tl
  | (line, (Instruction.WIDE_ALOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ALOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_ALOAD x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.ALOAD (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.ALOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ALOAD_0) :: acc) tl
  | (line, (Instruction.ALOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ALOAD_1) :: acc) tl
  | (line, (Instruction.ALOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ALOAD_2) :: acc) tl
  | (line, (Instruction.ALOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ALOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_ASTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ASTORE_0) :: acc) tl
  | (line, (Instruction.WIDE_ASTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ASTORE_1) :: acc) tl
  | (line, (Instruction.WIDE_ASTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ASTORE_2) :: acc) tl
  | (line, (Instruction.WIDE_ASTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ASTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_ASTORE x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.ASTORE (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.ASTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ASTORE_0) :: acc) tl
  | (line, (Instruction.ASTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ASTORE_1) :: acc) tl
  | (line, (Instruction.ASTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ASTORE_2) :: acc) tl
  | (line, (Instruction.ASTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ASTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_DLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.DLOAD_0) :: acc) tl
  | (line, (Instruction.WIDE_DLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.DLOAD_1) :: acc) tl
  | (line, (Instruction.WIDE_DLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.DLOAD_2) :: acc) tl
  | (line, (Instruction.WIDE_DLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.DLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_DLOAD x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.DLOAD (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.DLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.DLOAD_0) :: acc) tl
  | (line, (Instruction.DLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.DLOAD_1) :: acc) tl
  | (line, (Instruction.DLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.DLOAD_2) :: acc) tl
  | (line, (Instruction.DLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.DLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.DSTORE_0) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.DSTORE_1) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.DSTORE_2) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.DSTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.DSTORE (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.DSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.DSTORE_0) :: acc) tl
  | (line, (Instruction.DSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.DSTORE_1) :: acc) tl
  | (line, (Instruction.DSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.DSTORE_2) :: acc) tl
  | (line, (Instruction.DSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.DSTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_FLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.FLOAD_0) :: acc) tl
  | (line, (Instruction.WIDE_FLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.FLOAD_1) :: acc) tl
  | (line, (Instruction.WIDE_FLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.FLOAD_2) :: acc) tl
  | (line, (Instruction.WIDE_FLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.FLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_FLOAD x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.FLOAD (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.FLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.FLOAD_0) :: acc) tl
  | (line, (Instruction.FLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.FLOAD_1) :: acc) tl
  | (line, (Instruction.FLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.FLOAD_2) :: acc) tl
  | (line, (Instruction.FLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.FLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.FSTORE_0) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.FSTORE_1) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.FSTORE_2) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.FSTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.FSTORE (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.FSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.FSTORE_0) :: acc) tl
  | (line, (Instruction.FSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.FSTORE_1) :: acc) tl
  | (line, (Instruction.FSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.FSTORE_2) :: acc) tl
  | (line, (Instruction.FSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.FSTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_ILOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ILOAD_0) :: acc) tl
  | (line, (Instruction.WIDE_ILOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ILOAD_1) :: acc) tl
  | (line, (Instruction.WIDE_ILOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ILOAD_2) :: acc) tl
  | (line, (Instruction.WIDE_ILOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ILOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_ILOAD x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.ILOAD (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.ILOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ILOAD_0) :: acc) tl
  | (line, (Instruction.ILOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ILOAD_1) :: acc) tl
  | (line, (Instruction.ILOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ILOAD_2) :: acc) tl
  | (line, (Instruction.ILOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ILOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ISTORE_0) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ISTORE_1) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ISTORE_2) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ISTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.ISTORE (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.ISTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.ISTORE_0) :: acc) tl
  | (line, (Instruction.ISTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.ISTORE_1) :: acc) tl
  | (line, (Instruction.ISTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.ISTORE_2) :: acc) tl
  | (line, (Instruction.ISTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.ISTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_LLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.LLOAD_0) :: acc) tl
  | (line, (Instruction.WIDE_LLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.LLOAD_1) :: acc) tl
  | (line, (Instruction.WIDE_LLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.LLOAD_2) :: acc) tl
  | (line, (Instruction.WIDE_LLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.LLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_LLOAD x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.LLOAD (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.LLOAD x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.LLOAD_0) :: acc) tl
  | (line, (Instruction.LLOAD x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.LLOAD_1) :: acc) tl
  | (line, (Instruction.LLOAD x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.LLOAD_2) :: acc) tl
  | (line, (Instruction.LLOAD x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.LLOAD_3) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.LSTORE_0) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.LSTORE_1) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.LSTORE_2) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.LSTORE_3) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE x)) :: tl when (x :> int) < 256 -> rewrite ((line, (Instruction.LSTORE (u1 (x :> int)))) :: acc) tl
  | (line, (Instruction.LSTORE x)) :: tl when (x :> int) = 0 -> rewrite ((line, Instruction.LSTORE_0) :: acc) tl
  | (line, (Instruction.LSTORE x)) :: tl when (x :> int) = 1 -> rewrite ((line, Instruction.LSTORE_1) :: acc) tl
  | (line, (Instruction.LSTORE x)) :: tl when (x :> int) = 2 -> rewrite ((line, Instruction.LSTORE_2) :: acc) tl
  | (line, (Instruction.LSTORE x)) :: tl when (x :> int) = 3 -> rewrite ((line, Instruction.LSTORE_3) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let remove_nops_and_unused_pushed_values l =
  let rec rewrite acc = function
  | (_, Instruction.NOP) :: tl -> rewrite acc tl
  | (_, Instruction.SWAP) :: (_, Instruction.SWAP) :: tl -> rewrite acc tl
  | (_, Instruction.ACONST_NULL) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.BIPUSH _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.DCONST_0) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.DCONST_1) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.FCONST_0) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FCONST_1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FCONST_2) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_2) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_3) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_4) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_5) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_M1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_0) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_1) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, (Instruction.LDC _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.LDC_W _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.LDC2_W _)) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, (Instruction.SIPUSH _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.ALOAD _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_0) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_2) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_3) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.DLOAD _)) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_0) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_1) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_2) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_3) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, (Instruction.FLOAD _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_0) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_2) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_3) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.ILOAD _)) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_0) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_1) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_2) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_3) :: (_, Instruction.POP) :: tl -> rewrite acc tl
  | (_, (Instruction.LLOAD _)) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_0) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_1) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_2) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_3) :: (_, Instruction.POP2) :: tl -> rewrite acc tl
  | (_, (Instruction.GETSTATIC f)) :: (_, Instruction.POP) :: tl when is_category1_field f -> rewrite acc tl
  | (_, (Instruction.GETSTATIC f)) :: (_, Instruction.POP2) :: tl when is_category2_field f -> rewrite acc tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let optimize_iinc l =
  let rec rewrite acc = function
  | (line, iload) :: (_, Instruction.ICONST_M1) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 1))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_1) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-1)))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_2) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-2)))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_3) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-3)))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_4) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-4)))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_5) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-5)))) :: acc) tl
  | (line, iload) :: (_, (Instruction.BIPUSH n)) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1_neg n))) :: acc) tl
  | (line, iload) :: (_, (Instruction.SIPUSH n)) :: (_, Instruction.ISUB) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.WIDE_IINC ((extract_iload_index_u2 iload), s2_neg n))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_M1) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 (-1)))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_1) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 1))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_2) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 2))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_3) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 3))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_4) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 4))) :: acc) tl
  | (line, iload) :: (_, Instruction.ICONST_5) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), s1 5))) :: acc) tl
  | (line, iload) :: (_, (Instruction.BIPUSH n)) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.IINC ((extract_iload_index_u1 iload), n))) :: acc) tl
  | (line, iload) :: (_, (Instruction.SIPUSH n)) :: (_, Instruction.IADD) :: (_, istore) :: tl when (load_store_same_index iload istore) -> rewrite ((line, (Instruction.WIDE_IINC ((extract_iload_index_u2 iload), n))) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let remove_load_store l =
  let rec rewrite acc = function
  | (_, (Instruction.WIDE_ALOAD i1)) :: (_, (Instruction.WIDE_ASTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, (Instruction.ALOAD i1)) :: (_, (Instruction.ASTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, Instruction.ALOAD_0) :: (_, Instruction.ASTORE_0) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_1) :: (_, Instruction.ASTORE_1) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_2) :: (_, Instruction.ASTORE_2) :: tl -> rewrite acc tl
  | (_, Instruction.ALOAD_3) :: (_, Instruction.ASTORE_3) :: tl -> rewrite acc tl
  | (_, (Instruction.WIDE_DLOAD i1)) :: (_, (Instruction.WIDE_DSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, (Instruction.DLOAD i1)) :: (_, (Instruction.DSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, Instruction.DLOAD_0) :: (_, Instruction.DSTORE_0) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_1) :: (_, Instruction.DSTORE_1) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_2) :: (_, Instruction.DSTORE_2) :: tl -> rewrite acc tl
  | (_, Instruction.DLOAD_3) :: (_, Instruction.DSTORE_3) :: tl -> rewrite acc tl
  | (_, (Instruction.WIDE_FLOAD i1)) :: (_, (Instruction.WIDE_FSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, (Instruction.FLOAD i1)) :: (_, (Instruction.FSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, Instruction.FLOAD_0) :: (_, Instruction.FSTORE_0) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_1) :: (_, Instruction.FSTORE_1) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_2) :: (_, Instruction.FSTORE_2) :: tl -> rewrite acc tl
  | (_, Instruction.FLOAD_3) :: (_, Instruction.FSTORE_3) :: tl -> rewrite acc tl
  | (_, (Instruction.WIDE_ILOAD i1)) :: (_, (Instruction.WIDE_ISTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, (Instruction.ILOAD i1)) :: (_, (Instruction.ISTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, Instruction.ILOAD_0) :: (_, Instruction.ISTORE_0) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_1) :: (_, Instruction.ISTORE_1) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_2) :: (_, Instruction.ISTORE_2) :: tl -> rewrite acc tl
  | (_, Instruction.ILOAD_3) :: (_, Instruction.ISTORE_3) :: tl -> rewrite acc tl
  | (_, (Instruction.WIDE_LLOAD i1)) :: (_, (Instruction.WIDE_LSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, (Instruction.LLOAD i1)) :: (_, (Instruction.LSTORE i2)) :: tl when i1 = i2 -> rewrite acc tl
  | (_, Instruction.LLOAD_0) :: (_, Instruction.LSTORE_0) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_1) :: (_, Instruction.LSTORE_1) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_2) :: (_, Instruction.LSTORE_2) :: tl -> rewrite acc tl
  | (_, Instruction.LLOAD_3) :: (_, Instruction.LSTORE_3) :: tl -> rewrite acc tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let remove_get_put l =
  let rec rewrite acc = function
  | (_, (Instruction.GETSTATIC f1)) :: (_, (Instruction.PUTSTATIC f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, (Instruction.ALOAD i1)) :: (_, (Instruction.ALOAD i2)) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (i1 = i2) && (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_0) :: (_, Instruction.ALOAD_0) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_1) :: (_, Instruction.ALOAD_1) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_2) :: (_, Instruction.ALOAD_2) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_3) :: (_, Instruction.ALOAD_3) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, (Instruction.ALOAD _)) :: (_, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_0) :: (_, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_1) :: (_, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_2) :: (_, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (_, Instruction.ALOAD_3) :: (_, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite acc tl
  | (line, Instruction.DUP) :: (_, (Instruction.GETFIELD f1)) :: (_, (Instruction.PUTFIELD f2)) :: tl when (same_field f1 f2) -> rewrite ((line, Instruction.POP) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let rewrite_store_store l =
  let rec rewrite acc = function
  | (line, (Instruction.WIDE_ASTORE i1)) :: (_, (Instruction.WIDE_ASTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_ASTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.ASTORE i1)) :: (_, (Instruction.ASTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.ASTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ASTORE_0) :: (_, Instruction.ASTORE_0) :: tl -> rewrite ((line, Instruction.ASTORE_0) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ASTORE_1) :: (_, Instruction.ASTORE_1) :: tl -> rewrite ((line, Instruction.ASTORE_1) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ASTORE_2) :: (_, Instruction.ASTORE_2) :: tl -> rewrite ((line, Instruction.ASTORE_2) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ASTORE_3) :: (_, Instruction.ASTORE_3) :: tl -> rewrite ((line, Instruction.ASTORE_3) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE i1)) :: (_, (Instruction.WIDE_DSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_DSTORE i1)) :: (line, Instruction.POP2) :: acc) tl
  | (line, (Instruction.DSTORE i1)) :: (_, (Instruction.DSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.DSTORE i1)) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.DSTORE_0) :: (_, Instruction.DSTORE_0) :: tl -> rewrite ((line, Instruction.DSTORE_0) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.DSTORE_1) :: (_, Instruction.DSTORE_1) :: tl -> rewrite ((line, Instruction.DSTORE_1) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.DSTORE_2) :: (_, Instruction.DSTORE_2) :: tl -> rewrite ((line, Instruction.DSTORE_2) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.DSTORE_3) :: (_, Instruction.DSTORE_3) :: tl -> rewrite ((line, Instruction.DSTORE_3) :: (line, Instruction.POP2) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE i1)) :: (_, (Instruction.WIDE_FSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_FSTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.FSTORE i1)) :: (_, (Instruction.FSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.FSTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.FSTORE_0) :: (_, Instruction.FSTORE_0) :: tl -> rewrite ((line, Instruction.FSTORE_0) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.FSTORE_1) :: (_, Instruction.FSTORE_1) :: tl -> rewrite ((line, Instruction.FSTORE_1) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.FSTORE_2) :: (_, Instruction.FSTORE_2) :: tl -> rewrite ((line, Instruction.FSTORE_2) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.FSTORE_3) :: (_, Instruction.FSTORE_3) :: tl -> rewrite ((line, Instruction.FSTORE_3) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE i1)) :: (_, (Instruction.WIDE_ISTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_ISTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.ISTORE i1)) :: (_, (Instruction.ISTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.ISTORE i1)) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ISTORE_0) :: (_, Instruction.ISTORE_0) :: tl -> rewrite ((line, Instruction.ISTORE_0) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ISTORE_1) :: (_, Instruction.ISTORE_1) :: tl -> rewrite ((line, Instruction.ISTORE_1) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ISTORE_2) :: (_, Instruction.ISTORE_2) :: tl -> rewrite ((line, Instruction.ISTORE_2) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.ISTORE_3) :: (_, Instruction.ISTORE_3) :: tl -> rewrite ((line, Instruction.ISTORE_3) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE i1)) :: (_, (Instruction.WIDE_LSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_LSTORE i1)) :: (line, Instruction.POP2) :: acc) tl
  | (line, (Instruction.LSTORE i1)) :: (_, (Instruction.LSTORE i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.LSTORE i1)) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.LSTORE_0) :: (_, Instruction.LSTORE_0) :: tl -> rewrite ((line, Instruction.LSTORE_0) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.LSTORE_1) :: (_, Instruction.LSTORE_1) :: tl -> rewrite ((line, Instruction.LSTORE_1) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.LSTORE_2) :: (_, Instruction.LSTORE_2) :: tl -> rewrite ((line, Instruction.LSTORE_2) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.LSTORE_3) :: (_, Instruction.LSTORE_3) :: tl -> rewrite ((line, Instruction.LSTORE_3) :: (line, Instruction.POP2) :: acc) tl
  | (line, (Instruction.PUTSTATIC f1)) :: (_, (Instruction.PUTSTATIC f2)) :: tl when (same_field f1 f2) && (is_category1_field f1) -> rewrite ((line, (Instruction.PUTSTATIC f1)) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.PUTSTATIC f1)) :: (_, (Instruction.PUTSTATIC f2)) :: tl when (same_field f1 f2) && (is_category2_field f1) -> rewrite ((line, (Instruction.PUTSTATIC f1)) :: (line, Instruction.POP2) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let rewrite_store_load l =
  let rec rewrite acc = function
  | (line, (Instruction.WIDE_ASTORE i1)) :: (_, (Instruction.WIDE_ALOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_ASTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.ASTORE i1)) :: (_, (Instruction.ALOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.ASTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ASTORE_0) :: (_, Instruction.ALOAD_0) :: tl -> rewrite ((line, Instruction.ASTORE_0) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ASTORE_1) :: (_, Instruction.ALOAD_1) :: tl -> rewrite ((line, Instruction.ASTORE_1) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ASTORE_2) :: (_, Instruction.ALOAD_2) :: tl -> rewrite ((line, Instruction.ASTORE_2) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ASTORE_3) :: (_, Instruction.ALOAD_3) :: tl -> rewrite ((line, Instruction.ASTORE_3) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.WIDE_DSTORE i1)) :: (_, (Instruction.WIDE_DLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_DSTORE i1)) :: (line, Instruction.DUP2) :: acc) tl
  | (line, (Instruction.DSTORE i1)) :: (_, (Instruction.DLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.DSTORE i1)) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.DSTORE_0) :: (_, Instruction.DLOAD_0) :: tl -> rewrite ((line, Instruction.DSTORE_0) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.DSTORE_1) :: (_, Instruction.DLOAD_1) :: tl -> rewrite ((line, Instruction.DSTORE_1) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.DSTORE_2) :: (_, Instruction.DLOAD_2) :: tl -> rewrite ((line, Instruction.DSTORE_2) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.DSTORE_3) :: (_, Instruction.DLOAD_3) :: tl -> rewrite ((line, Instruction.DSTORE_3) :: (line, Instruction.DUP2) :: acc) tl
  | (line, (Instruction.WIDE_FSTORE i1)) :: (_, (Instruction.WIDE_FLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_FSTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.FSTORE i1)) :: (_, (Instruction.FLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.FSTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.FSTORE_0) :: (_, Instruction.FLOAD_0) :: tl -> rewrite ((line, Instruction.FSTORE_0) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.FSTORE_1) :: (_, Instruction.FLOAD_1) :: tl -> rewrite ((line, Instruction.FSTORE_1) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.FSTORE_2) :: (_, Instruction.FLOAD_2) :: tl -> rewrite ((line, Instruction.FSTORE_2) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.FSTORE_3) :: (_, Instruction.FLOAD_3) :: tl -> rewrite ((line, Instruction.FSTORE_3) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.WIDE_ISTORE i1)) :: (_, (Instruction.WIDE_ILOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_ISTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.ISTORE i1)) :: (_, (Instruction.ILOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.ISTORE i1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ISTORE_0) :: (_, Instruction.ILOAD_0) :: tl -> rewrite ((line, Instruction.ISTORE_0) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ISTORE_1) :: (_, Instruction.ILOAD_1) :: tl -> rewrite ((line, Instruction.ISTORE_1) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ISTORE_2) :: (_, Instruction.ILOAD_2) :: tl -> rewrite ((line, Instruction.ISTORE_2) :: (line, Instruction.DUP) :: acc) tl
  | (line, Instruction.ISTORE_3) :: (_, Instruction.ILOAD_3) :: tl -> rewrite ((line, Instruction.ISTORE_3) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.WIDE_LSTORE i1)) :: (_, (Instruction.WIDE_LLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.WIDE_LSTORE i1)) :: (line, Instruction.DUP2) :: acc) tl
  | (line, (Instruction.LSTORE i1)) :: (_, (Instruction.LLOAD i2)) :: tl when i1 = i2 -> rewrite ((line, (Instruction.LSTORE i1)) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.LSTORE_0) :: (_, Instruction.LLOAD_0) :: tl -> rewrite ((line, Instruction.LSTORE_0) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.LSTORE_1) :: (_, Instruction.LLOAD_1) :: tl -> rewrite ((line, Instruction.LSTORE_1) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.LSTORE_2) :: (_, Instruction.LLOAD_2) :: tl -> rewrite ((line, Instruction.LSTORE_2) :: (line, Instruction.DUP2) :: acc) tl
  | (line, Instruction.LSTORE_3) :: (_, Instruction.LLOAD_3) :: tl -> rewrite ((line, Instruction.LSTORE_3) :: (line, Instruction.DUP2) :: acc) tl
  | (line, (Instruction.PUTSTATIC f1)) :: (_, (Instruction.GETSTATIC f2)) :: tl when (same_field f1 f2) && (is_category1_field f1) -> rewrite ((line, (Instruction.PUTSTATIC f1)) :: (line, Instruction.DUP) :: acc) tl
  | (line, (Instruction.PUTSTATIC f1)) :: (_, (Instruction.GETSTATIC f2)) :: tl when (same_field f1 f2) && (is_category2_field f1) -> rewrite ((line, (Instruction.PUTSTATIC f1)) :: (line, Instruction.DUP2) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let constant_on_the_top l =
  let rec rewrite acc = function
  | (line, Instruction.ICONST_0) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_0) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_1) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_1) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_2) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_2) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_3) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_3) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_4) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_4) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_5) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_5) :: (line, iload) :: acc) tl
  | (line, Instruction.ICONST_M1) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, Instruction.ICONST_M1) :: (line, iload) :: acc) tl
  | (line, (Instruction.BIPUSH c)) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, (Instruction.BIPUSH c)) :: (line, iload) :: acc) tl
  | (line, (Instruction.SIPUSH c)) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, (Instruction.SIPUSH c)) :: (line, iload) :: acc) tl
  | (line, (Instruction.LDC (`Int c))) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, (Instruction.LDC (`Int c))) :: (line, iload) :: acc) tl
  | (line, (Instruction.LDC_W (`Int c))) :: (_, iload) :: (_, op) :: tl when (is_iload iload) && (is_commutative_int_operation op) -> rewrite ((line, op) :: (line, (Instruction.LDC_W (`Int c))) :: (line, iload) :: acc) tl
  | (line, Instruction.LCONST_0) :: (_, lload) :: (_, op) :: tl when (is_lload lload) && (is_commutative_long_operation op) -> rewrite ((line, op) :: (line, Instruction.LCONST_0) :: (line, lload) :: acc) tl
  | (line, Instruction.LCONST_1) :: (_, lload) :: (_, op) :: tl when (is_lload lload) && (is_commutative_long_operation op) -> rewrite ((line, op) :: (line, Instruction.LCONST_1) :: (line, lload) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long c))) :: (_, (Instruction.LLOAD i)) :: (_, op) :: tl when (is_commutative_long_operation op) -> rewrite ((line, op) :: (line, (Instruction.LDC2_W (`Long c))) :: (line, (Instruction.LLOAD i)) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let remove_neutral_elements l =
  let rec rewrite acc = function
  | (_, Instruction.ICONST_0) :: (_, Instruction.IADD) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_0) :: (_, Instruction.LADD) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.ISUB) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_0) :: (_, Instruction.LSUB) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_1) :: (_, Instruction.IMUL) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_1) :: (_, Instruction.LMUL) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_1) :: (_, Instruction.IDIV) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_1) :: (_, Instruction.LDIV) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.IOR) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_0) :: (_, Instruction.LOR) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.IXOR) :: tl -> rewrite acc tl
  | (_, Instruction.LCONST_0) :: (_, Instruction.LXOR) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_M1) :: (_, Instruction.IAND) :: tl -> rewrite acc tl
  | (_, (Instruction.LDC2_W (`Long (-1L)))) :: (_, Instruction.LAND) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.ISHL) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.LSHL) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.ISHR) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.LSHR) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.IUSHR) :: tl -> rewrite acc tl
  | (_, Instruction.ICONST_0) :: (_, Instruction.LUSHR) :: tl -> rewrite acc tl
  | (_, (Instruction.IINC (_, z))) :: tl when z = s1_zero -> rewrite acc tl
  | (_, (Instruction.WIDE_IINC (_, z))) :: tl when z = s2_zero -> rewrite acc tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let rewrite_absorbing_elements l =
  let rec rewrite acc = function
  | (line, Instruction.ICONST_0) :: (_, Instruction.IMUL) :: tl -> rewrite ((line, Instruction.ICONST_0) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.LCONST_0) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LCONST_0) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.ICONST_0) :: (_, Instruction.IAND) :: tl -> rewrite ((line, Instruction.ICONST_0) :: (line, Instruction.POP) :: acc) tl
  | (line, Instruction.LCONST_0) :: (_, Instruction.LAND) :: tl -> rewrite ((line, Instruction.LCONST_0) :: (line, Instruction.POP2) :: acc) tl
  | (line, Instruction.ICONST_M1) :: (_, Instruction.IOR) :: tl -> rewrite ((line, Instruction.ICONST_M1) :: (line, Instruction.POP) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long (-1L)))) :: (_, Instruction.IOR) :: tl -> rewrite ((line, (Instruction.LDC2_W (`Long (-1L)))) :: (line, Instruction.POP2) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let apply_stength_reduction l =
  let rec rewrite acc = function
  | (line, Instruction.ICONST_M1) :: (_, Instruction.IMUL) :: tl -> rewrite ((line, Instruction.INEG) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long (-1L)))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LNEG) :: acc) tl
  | (line, Instruction.ICONST_M1) :: (_, Instruction.IDIV) :: tl -> rewrite ((line, Instruction.INEG) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long (-1L)))) :: (_, Instruction.LDIV) :: tl -> rewrite ((line, Instruction.LNEG) :: acc) tl
  | (line, Instruction.ICONST_2) :: (_, Instruction.IMUL) :: tl -> rewrite ((line, Instruction.ISHL) :: (line, Instruction.ICONST_1) :: acc) tl
  | (line, Instruction.ICONST_4) :: (_, Instruction.IMUL) :: tl -> rewrite ((line, Instruction.ISHL) :: (line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 8 -> rewrite ((line, Instruction.ISHL) :: (line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 16 -> rewrite ((line, Instruction.ISHL) :: (line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 32 -> rewrite ((line, Instruction.ISHL) :: (line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.BIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 64 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 6))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 128 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 7))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 256 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 8))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 512 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 9))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 1024 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 10))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 2048 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 11))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 4096 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 12))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 8192 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 13))) :: acc) tl
  | (line, (Instruction.SIPUSH x)) :: (_, Instruction.IMUL) :: tl when (x :> int) = 16384 -> rewrite ((line, Instruction.ISHL) :: (line, (Instruction.BIPUSH (s1 14))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 2L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, Instruction.ICONST_1) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 4L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, Instruction.ICONST_2) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 8L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, Instruction.ICONST_3) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 16L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, Instruction.ICONST_4) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 32L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, Instruction.ICONST_5) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 64L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 6))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 128L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 7))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 256L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 8))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 512L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 9))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 1024L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 10))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 2048L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 11))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 4096L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 12))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 8192L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 13))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 16384L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 14))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 32768L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 15))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 65536L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 16))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 16777216L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 24))) :: acc) tl
  | (line, (Instruction.LDC2_W (`Long 4294967296L))) :: (_, Instruction.LMUL) :: tl -> rewrite ((line, Instruction.LSHL) :: (line, (Instruction.BIPUSH (s1 32))) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let remove_identity l =
  let rec rewrite acc = function
  | (_, Instruction.INEG) :: (_, Instruction.INEG) :: tl -> rewrite acc tl
  | (_, Instruction.LNEG) :: (_, Instruction.LNEG) :: tl -> rewrite acc tl
  | (line, Instruction.INEG) :: (_, Instruction.IADD) :: tl -> rewrite ((line, Instruction.ISUB) :: acc) tl
  | (line, Instruction.LNEG) :: (_, Instruction.LADD) :: tl -> rewrite ((line, Instruction.LSUB) :: acc) tl
  | (line, Instruction.I2B) :: (_, Instruction.I2B) :: tl -> rewrite ((line, Instruction.I2B) :: acc) tl
  | (line, Instruction.I2C) :: (_, Instruction.I2C) :: tl -> rewrite ((line, Instruction.I2C) :: acc) tl
  | (line, Instruction.I2S) :: (_, Instruction.I2S) :: tl -> rewrite ((line, Instruction.I2S) :: acc) tl
  | (line, Instruction.I2C) :: (_, Instruction.I2B) :: tl -> rewrite ((line, Instruction.I2B) :: acc) tl
  | (line, Instruction.I2S) :: (_, Instruction.I2B) :: tl -> rewrite ((line, Instruction.I2B) :: acc) tl
  | (line, Instruction.I2S) :: (_, Instruction.I2C) :: tl -> rewrite ((line, Instruction.I2C) :: acc) tl
  | (line, Instruction.I2C) :: (_, Instruction.I2S) :: tl -> rewrite ((line, Instruction.I2S) :: acc) tl
  | (line, Instruction.I2B) :: (_, Instruction.I2S) :: tl -> rewrite ((line, Instruction.I2B) :: acc) tl
  | (_, Instruction.I2L) :: (_, Instruction.L2I) :: tl -> rewrite acc tl
  | (line, (Instruction.CHECKCAST c1)) :: (_, (Instruction.CHECKCAST c2)) :: tl when same_cast c1 c2 -> rewrite ((line, (Instruction.CHECKCAST c1)) :: acc) tl
  | hd :: tl -> rewrite (hd :: acc) tl
  | [] -> List.rev acc in
  rewrite [] l

let all_rules = [
  optimize_constants ;
  optimize_locals ;
  remove_nops_and_unused_pushed_values ;
  optimize_iinc ;
  remove_load_store ;
  remove_get_put ;
  rewrite_store_store ;
  rewrite_store_load ;
  constant_on_the_top ;
  remove_neutral_elements ;
  rewrite_absorbing_elements ;
  apply_stength_reduction ;
  remove_identity ;
]

type position =
  | Head
  | Tail
  | Before of rewriting_rules
  | After of rewriting_rules

let rec insert r p l =
  match (p, l) with
  | Head, _ -> r :: l
  | Tail, _ -> l @ [r]
  | (Before r'), (hd :: tl) -> if r' == hd then r :: l else hd :: (insert r p tl)
  | (After r'), (hd :: tl) -> if r' == hd then hd :: r :: tl else hd :: (insert r p tl)
  | _ -> raise Not_found


(* Application of rewriting rules *)

let instruction_eq x y =
  let eq_coi x y =
    match x, y with
    | (`Class_or_interface c1), (`Class_or_interface c2) -> Name.equal_for_class c1 c2
    | (`Array_type d1), (`Array_type d2) -> Descriptor.equal_java_type (d1 :> Descriptor.java_type) (d2 :> Descriptor.java_type)
    | _ -> false in
  let eq_cst x y =
    match x, y with
    | (`Int v1), (`Int v2) -> v1 = v2
    | (`Float v1), (`Float v2) -> v1 = v2 || v1 == v2 || ((abs_float (v1 -. v2)) < epsilon_float)
    | (`String v1), (`String v2) -> UTF8.equal v1 v2
    | (`Class_or_interface c1), (`Class_or_interface c2) -> Name.equal_for_class c1 c2
    | (`Array_type d1), (`Array_type d2) -> Descriptor.equal_java_type (d1 :> Descriptor.java_type) (d2 :> Descriptor.java_type)
    | _ -> false in
  let eq_cst2 x y =
    match x, y with
    | (`Long v1), (`Long v2) -> v1 = v2
    | (`Double v1), (`Double v2) -> v1 = v2 || v1 == v2 || ((abs_float (v1 -. v2)) < epsilon_float)
    | _ -> false in
  let same_dynamic_method (bs1, mn1, md1) (bs2, mn2, md2) =
    (Bootstrap.equal_method_specifier bs1 bs2)
    && (Name.equal_for_method mn1 mn2)
    && (Descriptor.equal_for_method md1 md2) in
  match x, y with
  | (Instruction.ANEWARRAY coi1), (Instruction.ANEWARRAY coi2) -> eq_coi coi1 coi2
  | (Instruction.CHECKCAST coi1), (Instruction.CHECKCAST coi2) -> eq_coi coi1 coi2
  | (Instruction.GETFIELD f1), (Instruction.GETFIELD f2) -> same_field f1 f2
  | (Instruction.GETSTATIC f1), (Instruction.GETSTATIC f2) -> same_field f1 f2
  | (Instruction.INSTANCEOF coi1), (Instruction.INSTANCEOF coi2) -> eq_coi coi1 coi2
  | (Instruction.INVOKEDYNAMIC m1), (Instruction.INVOKEDYNAMIC m2) -> same_dynamic_method m1 m2
  | (Instruction.INVOKEINTERFACE (m1, n1)), (Instruction.INVOKEINTERFACE (m2, n2)) -> (same_method m1 m2) && (n1 = n2)
  | (Instruction.INVOKESPECIAL m1), (Instruction.INVOKESPECIAL m2) -> same_method m1 m2
  | (Instruction.INVOKESTATIC m1), (Instruction.INVOKESTATIC m2) -> same_method m1 m2
  | (Instruction.INVOKEVIRTUAL (coi1, m1, d1)), (Instruction.INVOKEVIRTUAL (coi2, m2, d2)) -> (eq_coi coi1 coi2) && (Name.equal_for_method m1 m2) && (Descriptor.equal_for_method d1 d2)
  | (Instruction.LDC c1), (Instruction.LDC c2) -> eq_cst c1 c2
  | (Instruction.LDC2_W c1), (Instruction.LDC2_W c2) -> eq_cst2 c1 c2
  | (Instruction.LDC_W c1), (Instruction.LDC_W c2) -> eq_cst c1 c2
  | (Instruction.MULTIANEWARRAY (coi1, n1)), (Instruction.MULTIANEWARRAY (coi2, n2)) -> (eq_coi coi1 coi2) && (n1 = n2)
  | (Instruction.NEW c1), (Instruction.NEW c2) -> Name.equal_for_class c1 c2
  | (Instruction.NEWARRAY d1), (Instruction.NEWARRAY d2) -> Descriptor.equal_java_type (d1 :> Descriptor.java_type) (d2 :> Descriptor.java_type)
  | (Instruction.PUTFIELD f1), (Instruction.PUTFIELD f2) -> same_field f1 f2
  | (Instruction.PUTSTATIC f1), (Instruction.PUTSTATIC f2) -> same_field f1 f2
  | _ -> x = y

let instruction_list_eq l1 l2 =
  try
    List.for_all2 (fun (_, x) (_, y) -> instruction_eq x y) l1 l2
  with Invalid_argument _ -> false

let optimize_graph ?(rules=all_rules) g =
  let optimize_list l = fix_point instruction_list_eq (compose_list rules) l in
  let same = ref true in
  let map_list l l' =
    let orig = List.combine l l' in
    let lst = optimize_list orig in
    same := !same && (instruction_list_eq orig lst);
    lst in
  let g' =
    ControlFlow.map_graph
      (fun (x, l) l' -> let l, l' = List.split (map_list l l') in ((x, l), l'))
      (fun x _ -> x)
      (fun x _ _ -> x)
      g in
  if !same then g else g'
