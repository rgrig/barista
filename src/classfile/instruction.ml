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
open Consts

module U = Utils

(* Types *)

type short_offset = s2

type long_offset = s4

type t =
  | AALOAD
  | AASTORE
  | ACONST_NULL
  | ALOAD of u1
  | ALOAD_0
  | ALOAD_1
  | ALOAD_2
  | ALOAD_3
  | ANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | ARETURN
  | ARRAYLENGTH
  | ASTORE of u1
  | ASTORE_0
  | ASTORE_1
  | ASTORE_2
  | ASTORE_3
  | ATHROW
  | BALOAD
  | BASTORE
  | BIPUSH of s1
  | CALOAD
  | CASTORE
  | CHECKCAST of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | D2F
  | D2I
  | D2L
  | DADD
  | DALOAD
  | DASTORE
  | DCMPG
  | DCMPL
  | DCONST_0
  | DCONST_1
  | DDIV
  | DLOAD of u1
  | DLOAD_0
  | DLOAD_1
  | DLOAD_2
  | DLOAD_3
  | DMUL
  | DNEG
  | DREM
  | DRETURN
  | DSTORE of u1
  | DSTORE_0
  | DSTORE_1
  | DSTORE_2
  | DSTORE_3
  | DSUB
  | DUP
  | DUP2
  | DUP2_X1
  | DUP2_X2
  | DUP_X1
  | DUP_X2
  | F2D
  | F2I
  | F2L
  | FADD
  | FALOAD
  | FASTORE
  | FCMPG
  | FCMPL
  | FCONST_0
  | FCONST_1
  | FCONST_2
  | FDIV
  | FLOAD of u1
  | FLOAD_0
  | FLOAD_1
  | FLOAD_2
  | FLOAD_3
  | FMUL
  | FNEG
  | FREM
  | FRETURN
  | FSTORE of u1
  | FSTORE_0
  | FSTORE_1
  | FSTORE_2
  | FSTORE_3
  | FSUB
  | GETFIELD of (Name.for_class * Name.for_field * Descriptor.for_field)
  | GETSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field)
  | GOTO of short_offset
  | GOTO_W of long_offset
  | I2B
  | I2C
  | I2D
  | I2F
  | I2L
  | I2S
  | IADD
  | IALOAD
  | IAND
  | IASTORE
  | ICONST_0
  | ICONST_1
  | ICONST_2
  | ICONST_3
  | ICONST_4
  | ICONST_5
  | ICONST_M1
  | IDIV
  | IF_ACMPEQ of short_offset
  | IF_ACMPNE of short_offset
  | IF_ICMPEQ of short_offset
  | IF_ICMPGE of short_offset
  | IF_ICMPGT of short_offset
  | IF_ICMPLE of short_offset
  | IF_ICMPLT of short_offset
  | IF_ICMPNE of short_offset
  | IFEQ of short_offset
  | IFGE of short_offset
  | IFGT of short_offset
  | IFLE of short_offset
  | IFLT of short_offset
  | IFNE of short_offset
  | IFNONNULL of short_offset
  | IFNULL of short_offset
  | IINC of u1 * s1
  | ILOAD of u1
  | ILOAD_0
  | ILOAD_1
  | ILOAD_2
  | ILOAD_3
  | IMUL
  | INEG
  | INSTANCEOF of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | INVOKEDYNAMIC of (Bootstrap.method_specifier * Name.for_method * Descriptor.for_method)
  | INVOKEINTERFACE of (Name.for_class * Name.for_method * Descriptor.for_method) * u1
  | INVOKESPECIAL of (Name.for_class * Name.for_method * Descriptor.for_method)
  | INVOKESTATIC of (Name.for_class * Name.for_method * Descriptor.for_method)
  | INVOKEVIRTUAL of ([`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * Name.for_method * Descriptor.for_method)
  | IOR
  | IREM
  | IRETURN
  | ISHL
  | ISHR
  | ISTORE of u1
  | ISTORE_0
  | ISTORE_1
  | ISTORE_2
  | ISTORE_3
  | ISUB
  | IUSHR
  | IXOR
  | JSR of short_offset
  | JSR_W of long_offset
  | L2D
  | L2F
  | L2I
  | LADD
  | LALOAD
  | LAND
  | LASTORE
  | LCMP
  | LCONST_0
  | LCONST_1
  | LDC of [`Int of int32 | `Float of float | `String of UTF8.t | `Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type | `Method_type of Descriptor.for_method | `Method_handle of Bootstrap.method_handle]
  | LDC2_W of [`Long of int64 | `Double of float]
  | LDC_W of [`Int of int32 | `Float of float | `String of UTF8.t | `Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type | `Method_type of Descriptor.for_method | `Method_handle of Bootstrap.method_handle]
  | LDIV
  | LLOAD of u1
  | LLOAD_0
  | LLOAD_1
  | LLOAD_2
  | LLOAD_3
  | LMUL
  | LNEG
  | LOOKUPSWITCH of long_offset * s4 * ((s4 * long_offset) list)
  | LOR
  | LREM
  | LRETURN
  | LSHL
  | LSHR
  | LSTORE of u1
  | LSTORE_0
  | LSTORE_1
  | LSTORE_2
  | LSTORE_3
  | LSUB
  | LUSHR
  | LXOR
  | MONITORENTER
  | MONITOREXIT
  | MULTIANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * u1
  | NEW of Name.for_class
  | NEWARRAY of Descriptor.java_type
  | NOP
  | POP
  | POP2
  | PUTFIELD of (Name.for_class * Name.for_field * Descriptor.for_field)
  | PUTSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field)
  | RET of u1
  | RETURN
  | SALOAD
  | SASTORE
  | SIPUSH of s2
  | SWAP
  | TABLESWITCH of long_offset * s4 * s4 * (long_offset list)
  | WIDE_ALOAD of u2
  | WIDE_ASTORE of u2
  | WIDE_DLOAD of u2
  | WIDE_DSTORE of u2
  | WIDE_FLOAD of u2
  | WIDE_FSTORE of u2
  | WIDE_IINC of u2 * s2
  | WIDE_ILOAD of u2
  | WIDE_ISTORE of u2
  | WIDE_LLOAD of u2
  | WIDE_LSTORE of u2
  | WIDE_RET of u2


(* Exception *)

type error =
  | Invalid_pool_element
  | Invalid_field
  | Invalid_dynamic_method
  | Invalid_interface_method
  | Invalid_method
  | Invalid_parameter
  | Invalid_primitive_type
  | Invalid_switch_cases
  | Invalid_pool_index
  | Invalid_pool_entry
  | Invalid_primitive_array_type
  | Invalid_index
  | Invalid_array_element
  | Invalid_class_name
  | Invalid_unsigned_byte
  | Invalid_byte
  | Invalid_unsigned_short
  | Invalid_signed_short
  | Invalid_signed_long
  | Invalid_short_offset
  | Invalid_long_offset
  | Invalid_number_of_arguments
  | Unknown_instruction
  | Invalid_method_handle
  | Too_many_bootstrap_specifiers

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_pool_element -> "invalid pool element"
  | Invalid_field -> "invalid field"
  | Invalid_dynamic_method -> "invalid dynamic method"
  | Invalid_interface_method -> "invalid interface method"
  | Invalid_method -> "invalid dynamic method"
  | Invalid_parameter -> "invalid parameter"
  | Invalid_primitive_type -> "invalid primitive type"
  | Invalid_switch_cases -> "invalid number of switch cases"
  | Invalid_pool_index -> "invalid pool index"
  | Invalid_pool_entry -> "invalid pool entry"
  | Invalid_primitive_array_type -> "invalid primitive array type"
  | Invalid_index -> "index is too large"
  | Invalid_array_element -> "invalid array element (void)"
  | Invalid_class_name -> "invalid class name"
  | Invalid_unsigned_byte -> "invalid unsigned byte"
  | Invalid_byte -> "invalid byte"
  | Invalid_unsigned_short -> "invalid unsigned short"
  | Invalid_signed_short -> "invalid signed short"
  | Invalid_signed_long -> "invalid signed long"
  | Invalid_short_offset -> "invalid short offset"
  | Invalid_long_offset -> "invalid long offset"
  | Invalid_number_of_arguments -> "wrong number of arguments"
  | Unknown_instruction -> "unknown instruction"
  | Invalid_method_handle -> "invalid method handle"
  | Too_many_bootstrap_specifiers -> "too many bootstrap specifiers"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Conversion functions *)

let decode bsi cpool i =
  let get_entry idx =
    try
      ConstantPool.get_entry cpool idx
    with _ -> fail Invalid_pool_index in
  let get_utf8 idx = match get_entry idx with
    | ConstantPool.UTF8 v -> v
    | _ -> fail Invalid_pool_entry in
  let get_class_or_array idx = match get_entry idx with
    | ConstantPool.UTF8 v ->
        if UChar.equal opening_square_bracket (UTF8.get v 0) then
          let t = Descriptor.java_type_of_internal_utf8 v in `Array_type (Descriptor.filter_non_array Descriptor.Invalid_array_element_type t)
        else
          `Class_or_interface (Name.make_for_class_from_internal v)
    | _ -> fail Invalid_pool_entry in
  let get_field_ref cls nat = match (get_entry cls), (get_entry nat) with
    | (ConstantPool.Class i1), (ConstantPool.NameAndType (i2, i3)) ->
      ((Name.make_for_class_from_internal (get_utf8 i1)),
       (Name.make_for_field (get_utf8 i2)),
       (Descriptor.field_of_utf8 (get_utf8 i3)))
    | _ -> fail Invalid_pool_entry in
  let get_method_ref cls nat = match (get_entry cls), (get_entry nat) with
    | (ConstantPool.Class i1), (ConstantPool.NameAndType (i2, i3)) ->
      (Name.make_for_class_from_internal (get_utf8 i1)),
      (Name.make_for_method (get_utf8 i2)),
      (Descriptor.method_of_utf8 (get_utf8 i3))
    | _ -> fail Invalid_pool_entry in
  let get_special_ref cls nat = match (get_entry cls), (get_entry nat) with
    | (ConstantPool.Class i1), (ConstantPool.NameAndType (i2, i3))
      when UTF8.equal (get_utf8 i2) class_constructor ->
      (Name.make_for_class_from_internal (get_utf8 i1)),
      (fst (Descriptor.method_of_utf8 (get_utf8 i3)))
    | _ -> fail Invalid_pool_entry in
    let get_dynamic idx nat = match get_entry nat with
      | ConstantPool.NameAndType (idx2, idx3)
        when ((idx : u2 :> int) < ExtendableArray.length bsi) ->
        ExtendableArray.get bsi idx,
        (Name.make_for_method (get_utf8 idx2)),
        (Descriptor.method_of_utf8 (get_utf8 idx3))
      | _ -> fail Invalid_pool_entry in
  let get_array_method_ref cls nat = match (get_entry cls), (get_entry nat) with
    | (ConstantPool.Class i1), (ConstantPool.NameAndType (i2, i3)) ->
      let s = get_utf8 i1 in
      (if not (UChar.equal opening_square_bracket (UTF8.get s 0)) then
         `Class_or_interface (Name.make_for_class_from_internal s)
      else
        `Array_type (Descriptor.filter_non_array Descriptor.Invalid_array_element_type (Descriptor.java_type_of_internal_utf8 s))),
      (Name.make_for_method (get_utf8 i2)),
      (Descriptor.method_of_utf8 (get_utf8 i3))
    | _ -> fail Invalid_pool_entry in
  let primitive_array_type_of_int = function
    | 4 -> `Boolean
    | 5 -> `Char
    | 6 -> `Float
    | 7 -> `Double
    | 8 -> `Byte
    | 9 -> `Short
    | 10 -> `Int
    | 11 -> `Long
    | _ -> fail Invalid_primitive_array_type in
  let get_method_handle kind idx =
    match kind, (get_entry idx) with
    | ConstantPool.REF_getField, ConstantPool.Fieldref (fc, nt) ->
      `getField (get_field_ref fc nt)
    | ConstantPool.REF_getStatic, ConstantPool.Fieldref (fc, nt) ->
      `getStatic (get_field_ref fc nt)
    | ConstantPool.REF_putField, ConstantPool.Fieldref (fc, nt) ->
      `putField (get_field_ref fc nt)
    | ConstantPool.REF_putStatic, ConstantPool.Fieldref (fc, nt) ->
      `putStatic (get_field_ref fc nt)
    | ConstantPool.REF_invokeVirtual, ConstantPool.Methodref (mc, mt) ->
      `invokeVirtual (get_method_ref mc mt)
    | ConstantPool.REF_invokeStatic, ConstantPool.Methodref (mc, mt) ->
      `invokeStatic (get_method_ref mc mt)
    | ConstantPool.REF_invokeSpecial, ConstantPool.Methodref (mc, mt) ->
      `invokeSpecial (get_method_ref mc mt)
    | ConstantPool.REF_newInvokeSpecial, ConstantPool.Methodref (mc, mt) ->
      `newInvokeSpecial (get_special_ref mc mt)
    | ConstantPool.REF_invokeInterface, ConstantPool.Methodref (mc, mt) ->
      `invokeInterface (get_method_ref mc mt)
    | _ -> fail Invalid_method_handle in
  match i with
  | ByteCode.AALOAD -> AALOAD
  | ByteCode.AASTORE -> AASTORE
  | ByteCode.ACONST_NULL -> ACONST_NULL
  | ByteCode.ALOAD p1 -> ALOAD p1
  | ByteCode.ALOAD_0 -> ALOAD_0
  | ByteCode.ALOAD_1 -> ALOAD_1
  | ByteCode.ALOAD_2 -> ALOAD_2
  | ByteCode.ALOAD_3 -> ALOAD_3
  | ByteCode.ANEWARRAY p1 -> ANEWARRAY (match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
  | ByteCode.ARETURN -> ARETURN
  | ByteCode.ARRAYLENGTH -> ARRAYLENGTH
  | ByteCode.ASTORE p1 -> ASTORE p1
  | ByteCode.ASTORE_0 -> ASTORE_0
  | ByteCode.ASTORE_1 -> ASTORE_1
  | ByteCode.ASTORE_2 -> ASTORE_2
  | ByteCode.ASTORE_3 -> ASTORE_3
  | ByteCode.ATHROW -> ATHROW
  | ByteCode.BALOAD -> BALOAD
  | ByteCode.BASTORE -> BASTORE
  | ByteCode.BIPUSH p1 -> BIPUSH p1
  | ByteCode.CALOAD -> CALOAD
  | ByteCode.CASTORE -> CASTORE
  | ByteCode.CHECKCAST p1 -> CHECKCAST (match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
  | ByteCode.D2F -> D2F
  | ByteCode.D2I -> D2I
  | ByteCode.D2L -> D2L
  | ByteCode.DADD -> DADD
  | ByteCode.DALOAD -> DALOAD
  | ByteCode.DASTORE -> DASTORE
  | ByteCode.DCMPG -> DCMPG
  | ByteCode.DCMPL -> DCMPL
  | ByteCode.DCONST_0 -> DCONST_0
  | ByteCode.DCONST_1 -> DCONST_1
  | ByteCode.DDIV -> DDIV
  | ByteCode.DLOAD p1 -> DLOAD p1
  | ByteCode.DLOAD_0 -> DLOAD_0
  | ByteCode.DLOAD_1 -> DLOAD_1
  | ByteCode.DLOAD_2 -> DLOAD_2
  | ByteCode.DLOAD_3 -> DLOAD_3
  | ByteCode.DMUL -> DMUL
  | ByteCode.DNEG -> DNEG
  | ByteCode.DREM -> DREM
  | ByteCode.DRETURN -> DRETURN
  | ByteCode.DSTORE p1 -> DSTORE p1
  | ByteCode.DSTORE_0 -> DSTORE_0
  | ByteCode.DSTORE_1 -> DSTORE_1
  | ByteCode.DSTORE_2 -> DSTORE_2
  | ByteCode.DSTORE_3 -> DSTORE_3
  | ByteCode.DSUB -> DSUB
  | ByteCode.DUP -> DUP
  | ByteCode.DUP2 -> DUP2
  | ByteCode.DUP2_X1 -> DUP2_X1
  | ByteCode.DUP2_X2 -> DUP2_X2
  | ByteCode.DUP_X1 -> DUP_X1
  | ByteCode.DUP_X2 -> DUP_X2
  | ByteCode.F2D -> F2D
  | ByteCode.F2I -> F2I
  | ByteCode.F2L -> F2L
  | ByteCode.FADD -> FADD
  | ByteCode.FALOAD -> FALOAD
  | ByteCode.FASTORE -> FASTORE
  | ByteCode.FCMPG -> FCMPG
  | ByteCode.FCMPL -> FCMPL
  | ByteCode.FCONST_0 -> FCONST_0
  | ByteCode.FCONST_1 -> FCONST_1
  | ByteCode.FCONST_2 -> FCONST_2
  | ByteCode.FDIV -> FDIV
  | ByteCode.FLOAD p1 -> FLOAD p1
  | ByteCode.FLOAD_0 -> FLOAD_0
  | ByteCode.FLOAD_1 -> FLOAD_1
  | ByteCode.FLOAD_2 -> FLOAD_2
  | ByteCode.FLOAD_3 -> FLOAD_3
  | ByteCode.FMUL -> FMUL
  | ByteCode.FNEG -> FNEG
  | ByteCode.FREM -> FREM
  | ByteCode.FRETURN -> FRETURN
  | ByteCode.FSTORE p1 -> FSTORE p1
  | ByteCode.FSTORE_0 -> FSTORE_0
  | ByteCode.FSTORE_1 -> FSTORE_1
  | ByteCode.FSTORE_2 -> FSTORE_2
  | ByteCode.FSTORE_3 -> FSTORE_3
  | ByteCode.FSUB -> FSUB
  | ByteCode.GETFIELD p1 -> GETFIELD (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.GETSTATIC p1 -> GETSTATIC (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.GOTO p1 -> GOTO p1
  | ByteCode.GOTO_W p1 -> GOTO_W p1
  | ByteCode.I2B -> I2B
  | ByteCode.I2C -> I2C
  | ByteCode.I2D -> I2D
  | ByteCode.I2F -> I2F
  | ByteCode.I2L -> I2L
  | ByteCode.I2S -> I2S
  | ByteCode.IADD -> IADD
  | ByteCode.IALOAD -> IALOAD
  | ByteCode.IAND -> IAND
  | ByteCode.IASTORE -> IASTORE
  | ByteCode.ICONST_0 -> ICONST_0
  | ByteCode.ICONST_1 -> ICONST_1
  | ByteCode.ICONST_2 -> ICONST_2
  | ByteCode.ICONST_3 -> ICONST_3
  | ByteCode.ICONST_4 -> ICONST_4
  | ByteCode.ICONST_5 -> ICONST_5
  | ByteCode.ICONST_M1 -> ICONST_M1
  | ByteCode.IDIV -> IDIV
  | ByteCode.IF_ACMPEQ p1 -> IF_ACMPEQ p1
  | ByteCode.IF_ACMPNE p1 -> IF_ACMPNE p1
  | ByteCode.IF_ICMPEQ p1 -> IF_ICMPEQ p1
  | ByteCode.IF_ICMPGE p1 -> IF_ICMPGE p1
  | ByteCode.IF_ICMPGT p1 -> IF_ICMPGT p1
  | ByteCode.IF_ICMPLE p1 -> IF_ICMPLE p1
  | ByteCode.IF_ICMPLT p1 -> IF_ICMPLT p1
  | ByteCode.IF_ICMPNE p1 -> IF_ICMPNE p1
  | ByteCode.IFEQ p1 -> IFEQ p1
  | ByteCode.IFGE p1 -> IFGE p1
  | ByteCode.IFGT p1 -> IFGT p1
  | ByteCode.IFLE p1 -> IFLE p1
  | ByteCode.IFLT p1 -> IFLT p1
  | ByteCode.IFNE p1 -> IFNE p1
  | ByteCode.IFNONNULL p1 -> IFNONNULL p1
  | ByteCode.IFNULL p1 -> IFNULL p1
  | ByteCode.IINC (p1, p2) -> IINC (p1, p2)
  | ByteCode.ILOAD p1 -> ILOAD p1
  | ByteCode.ILOAD_0 -> ILOAD_0
  | ByteCode.ILOAD_1 -> ILOAD_1
  | ByteCode.ILOAD_2 -> ILOAD_2
  | ByteCode.ILOAD_3 -> ILOAD_3
  | ByteCode.IMUL -> IMUL
  | ByteCode.INEG -> INEG
  | ByteCode.INSTANCEOF p1 -> INSTANCEOF (match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
  | ByteCode.INVOKEDYNAMIC p1 -> INVOKEDYNAMIC (match get_entry p1 with | ConstantPool.InvokeDynamic (idx, nat) -> get_dynamic idx nat | _ -> fail Invalid_pool_element)
  | ByteCode.INVOKEINTERFACE (p1, p2) -> INVOKEINTERFACE ((match get_entry p1 with | ConstantPool.InterfaceMethodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element), p2)
  | ByteCode.INVOKESPECIAL p1 -> INVOKESPECIAL (match get_entry p1 with | ConstantPool.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.INVOKESTATIC p1 -> INVOKESTATIC (match get_entry p1 with | ConstantPool.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.INVOKEVIRTUAL p1 -> INVOKEVIRTUAL (match get_entry p1 with | ConstantPool.Methodref (cls, nat) -> (get_array_method_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.IOR -> IOR
  | ByteCode.IREM -> IREM
  | ByteCode.IRETURN -> IRETURN
  | ByteCode.ISHL -> ISHL
  | ByteCode.ISHR -> ISHR
  | ByteCode.ISTORE p1 -> ISTORE p1
  | ByteCode.ISTORE_0 -> ISTORE_0
  | ByteCode.ISTORE_1 -> ISTORE_1
  | ByteCode.ISTORE_2 -> ISTORE_2
  | ByteCode.ISTORE_3 -> ISTORE_3
  | ByteCode.ISUB -> ISUB
  | ByteCode.IUSHR -> IUSHR
  | ByteCode.IXOR -> IXOR
  | ByteCode.JSR p1 -> JSR p1
  | ByteCode.JSR_W p1 -> JSR_W p1
  | ByteCode.L2D -> L2D
  | ByteCode.L2F -> L2F
  | ByteCode.L2I -> L2I
  | ByteCode.LADD -> LADD
  | ByteCode.LALOAD -> LALOAD
  | ByteCode.LAND -> LAND
  | ByteCode.LASTORE -> LASTORE
  | ByteCode.LCMP -> LCMP
  | ByteCode.LCONST_0 -> LCONST_0
  | ByteCode.LCONST_1 -> LCONST_1
  | ByteCode.LDC p1 -> LDC (match get_entry (u2_of_u1 p1) with | ConstantPool.Integer v -> `Int v | ConstantPool.Float v -> `Float (Int32.float_of_bits v) | ConstantPool.String idx -> `String (get_utf8 idx) | ConstantPool.Class idx -> get_class_or_array idx | ConstantPool.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (get_utf8 idx)) | ConstantPool.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
  | ByteCode.LDC2_W p1 -> LDC2_W (match get_entry p1 with | ConstantPool.Long (hi, lo) -> `Long (U.i64_of_2i32 hi lo) | ConstantPool.Double (hi, lo) -> `Double (Int64.float_of_bits (U.i64_of_2i32 hi lo)) | _ -> fail Invalid_pool_element)
  | ByteCode.LDC_W p1 -> LDC_W (match get_entry p1 with | ConstantPool.Integer v -> `Int v | ConstantPool.Float v -> `Float (Int32.float_of_bits v) | ConstantPool.String idx -> `String (get_utf8 idx) | ConstantPool.Class idx -> get_class_or_array idx | ConstantPool.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (get_utf8 idx)) | ConstantPool.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
  | ByteCode.LDIV -> LDIV
  | ByteCode.LLOAD p1 -> LLOAD p1
  | ByteCode.LLOAD_0 -> LLOAD_0
  | ByteCode.LLOAD_1 -> LLOAD_1
  | ByteCode.LLOAD_2 -> LLOAD_2
  | ByteCode.LLOAD_3 -> LLOAD_3
  | ByteCode.LMUL -> LMUL
  | ByteCode.LNEG -> LNEG
  | ByteCode.LOOKUPSWITCH (p1, p2, p3) -> LOOKUPSWITCH (p1, p2, p3)
  | ByteCode.LOR -> LOR
  | ByteCode.LREM -> LREM
  | ByteCode.LRETURN -> LRETURN
  | ByteCode.LSHL -> LSHL
  | ByteCode.LSHR -> LSHR
  | ByteCode.LSTORE p1 -> LSTORE p1
  | ByteCode.LSTORE_0 -> LSTORE_0
  | ByteCode.LSTORE_1 -> LSTORE_1
  | ByteCode.LSTORE_2 -> LSTORE_2
  | ByteCode.LSTORE_3 -> LSTORE_3
  | ByteCode.LSUB -> LSUB
  | ByteCode.LUSHR -> LUSHR
  | ByteCode.LXOR -> LXOR
  | ByteCode.MONITORENTER -> MONITORENTER
  | ByteCode.MONITOREXIT -> MONITOREXIT
  | ByteCode.MULTIANEWARRAY (p1, p2) -> MULTIANEWARRAY ((match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element), p2)
  | ByteCode.NEW p1 -> NEW (match get_entry p1 with | ConstantPool.Class idx -> (Name.make_for_class_from_internal (get_utf8 idx)) | _ -> fail Invalid_pool_element)
  | ByteCode.NEWARRAY p1 -> NEWARRAY (primitive_array_type_of_int (p1 :> int))
  | ByteCode.NOP -> NOP
  | ByteCode.POP -> POP
  | ByteCode.POP2 -> POP2
  | ByteCode.PUTFIELD p1 -> PUTFIELD (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.PUTSTATIC p1 -> PUTSTATIC (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.RET p1 -> RET p1
  | ByteCode.RETURN -> RETURN
  | ByteCode.SALOAD -> SALOAD
  | ByteCode.SASTORE -> SASTORE
  | ByteCode.SIPUSH p1 -> SIPUSH p1
  | ByteCode.SWAP -> SWAP
  | ByteCode.TABLESWITCH (p1, p2, p3, p4) -> TABLESWITCH (p1, p2, p3, p4)
  | ByteCode.WIDE_ALOAD p1 -> WIDE_ALOAD p1
  | ByteCode.WIDE_ASTORE p1 -> WIDE_ASTORE p1
  | ByteCode.WIDE_DLOAD p1 -> WIDE_DLOAD p1
  | ByteCode.WIDE_DSTORE p1 -> WIDE_DSTORE p1
  | ByteCode.WIDE_FLOAD p1 -> WIDE_FLOAD p1
  | ByteCode.WIDE_FSTORE p1 -> WIDE_FSTORE p1
  | ByteCode.WIDE_IINC (p1, p2) -> WIDE_IINC (p1, p2)
  | ByteCode.WIDE_ILOAD p1 -> WIDE_ILOAD p1
  | ByteCode.WIDE_ISTORE p1 -> WIDE_ISTORE p1
  | ByteCode.WIDE_LLOAD p1 -> WIDE_LLOAD p1
  | ByteCode.WIDE_LSTORE p1 -> WIDE_LSTORE p1
  | ByteCode.WIDE_RET p1 -> WIDE_RET p1

let encode bsm cpool i =
  let check_byte x =
    let x = (x : u2 :> int) in
    if x < 256 && x >= 0 then u1 x else fail Invalid_index in
  let int_of_primitive_array_type = function
    | `Boolean -> 4
    | `Char -> 5
    | `Float -> 6
    | `Double -> 7
    | `Byte -> 8
    | `Short -> 9
    | `Int -> 10
    | `Long -> 11
    | _ -> fail Invalid_primitive_array_type in
  let reference = function
    | `getField x -> ConstantPool.Reference_getField x
    | `getStatic x -> ConstantPool.Reference_getStatic x
    | `putField x -> ConstantPool.Reference_putField x
    | `putStatic x -> ConstantPool.Reference_putStatic x
    | `invokeVirtual x -> ConstantPool.Reference_invokeVirtual x
    | `invokeStatic x -> ConstantPool.Reference_invokeStatic x
    | `invokeSpecial x -> ConstantPool.Reference_invokeSpecial x
    | `newInvokeSpecial x -> ConstantPool.Reference_newInvokeSpecial x
    | `invokeInterface x -> ConstantPool.Reference_invokeInterface x in
  match i with
  | AALOAD -> ByteCode.AALOAD
  | AASTORE -> ByteCode.AASTORE
  | ACONST_NULL -> ByteCode.ACONST_NULL
  | ALOAD p1 -> ByteCode.ALOAD p1
  | ALOAD_0 -> ByteCode.ALOAD_0
  | ALOAD_1 -> ByteCode.ALOAD_1
  | ALOAD_2 -> ByteCode.ALOAD_2
  | ALOAD_3 -> ByteCode.ALOAD_3
  | ANEWARRAY p1 -> ByteCode.ANEWARRAY (match p1 with | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t)
  | ARETURN -> ByteCode.ARETURN
  | ARRAYLENGTH -> ByteCode.ARRAYLENGTH
  | ASTORE p1 -> ByteCode.ASTORE p1
  | ASTORE_0 -> ByteCode.ASTORE_0
  | ASTORE_1 -> ByteCode.ASTORE_1
  | ASTORE_2 -> ByteCode.ASTORE_2
  | ASTORE_3 -> ByteCode.ASTORE_3
  | ATHROW -> ByteCode.ATHROW
  | BALOAD -> ByteCode.BALOAD
  | BASTORE -> ByteCode.BASTORE
  | BIPUSH p1 -> ByteCode.BIPUSH p1
  | CALOAD -> ByteCode.CALOAD
  | CASTORE -> ByteCode.CASTORE
  | CHECKCAST p1 -> ByteCode.CHECKCAST (match p1 with | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t)
  | D2F -> ByteCode.D2F
  | D2I -> ByteCode.D2I
  | D2L -> ByteCode.D2L
  | DADD -> ByteCode.DADD
  | DALOAD -> ByteCode.DALOAD
  | DASTORE -> ByteCode.DASTORE
  | DCMPG -> ByteCode.DCMPG
  | DCMPL -> ByteCode.DCMPL
  | DCONST_0 -> ByteCode.DCONST_0
  | DCONST_1 -> ByteCode.DCONST_1
  | DDIV -> ByteCode.DDIV
  | DLOAD p1 -> ByteCode.DLOAD p1
  | DLOAD_0 -> ByteCode.DLOAD_0
  | DLOAD_1 -> ByteCode.DLOAD_1
  | DLOAD_2 -> ByteCode.DLOAD_2
  | DLOAD_3 -> ByteCode.DLOAD_3
  | DMUL -> ByteCode.DMUL
  | DNEG -> ByteCode.DNEG
  | DREM -> ByteCode.DREM
  | DRETURN -> ByteCode.DRETURN
  | DSTORE p1 -> ByteCode.DSTORE p1
  | DSTORE_0 -> ByteCode.DSTORE_0
  | DSTORE_1 -> ByteCode.DSTORE_1
  | DSTORE_2 -> ByteCode.DSTORE_2
  | DSTORE_3 -> ByteCode.DSTORE_3
  | DSUB -> ByteCode.DSUB
  | DUP -> ByteCode.DUP
  | DUP2 -> ByteCode.DUP2
  | DUP2_X1 -> ByteCode.DUP2_X1
  | DUP2_X2 -> ByteCode.DUP2_X2
  | DUP_X1 -> ByteCode.DUP_X1
  | DUP_X2 -> ByteCode.DUP_X2
  | F2D -> ByteCode.F2D
  | F2I -> ByteCode.F2I
  | F2L -> ByteCode.F2L
  | FADD -> ByteCode.FADD
  | FALOAD -> ByteCode.FALOAD
  | FASTORE -> ByteCode.FASTORE
  | FCMPG -> ByteCode.FCMPG
  | FCMPL -> ByteCode.FCMPL
  | FCONST_0 -> ByteCode.FCONST_0
  | FCONST_1 -> ByteCode.FCONST_1
  | FCONST_2 -> ByteCode.FCONST_2
  | FDIV -> ByteCode.FDIV
  | FLOAD p1 -> ByteCode.FLOAD p1
  | FLOAD_0 -> ByteCode.FLOAD_0
  | FLOAD_1 -> ByteCode.FLOAD_1
  | FLOAD_2 -> ByteCode.FLOAD_2
  | FLOAD_3 -> ByteCode.FLOAD_3
  | FMUL -> ByteCode.FMUL
  | FNEG -> ByteCode.FNEG
  | FREM -> ByteCode.FREM
  | FRETURN -> ByteCode.FRETURN
  | FSTORE p1 -> ByteCode.FSTORE p1
  | FSTORE_0 -> ByteCode.FSTORE_0
  | FSTORE_1 -> ByteCode.FSTORE_1
  | FSTORE_2 -> ByteCode.FSTORE_2
  | FSTORE_3 -> ByteCode.FSTORE_3
  | FSUB -> ByteCode.FSUB
  | GETFIELD p1 -> ByteCode.GETFIELD (let c, n, t = p1 in ConstantPool.add_field cpool c n t)
  | GETSTATIC p1 -> ByteCode.GETSTATIC (let c, n, t = p1 in ConstantPool.add_field cpool c n t)
  | GOTO p1 -> ByteCode.GOTO p1
  | GOTO_W p1 -> ByteCode.GOTO_W p1
  | I2B -> ByteCode.I2B
  | I2C -> ByteCode.I2C
  | I2D -> ByteCode.I2D
  | I2F -> ByteCode.I2F
  | I2L -> ByteCode.I2L
  | I2S -> ByteCode.I2S
  | IADD -> ByteCode.IADD
  | IALOAD -> ByteCode.IALOAD
  | IAND -> ByteCode.IAND
  | IASTORE -> ByteCode.IASTORE
  | ICONST_0 -> ByteCode.ICONST_0
  | ICONST_1 -> ByteCode.ICONST_1
  | ICONST_2 -> ByteCode.ICONST_2
  | ICONST_3 -> ByteCode.ICONST_3
  | ICONST_4 -> ByteCode.ICONST_4
  | ICONST_5 -> ByteCode.ICONST_5
  | ICONST_M1 -> ByteCode.ICONST_M1
  | IDIV -> ByteCode.IDIV
  | IF_ACMPEQ p1 -> ByteCode.IF_ACMPEQ p1
  | IF_ACMPNE p1 -> ByteCode.IF_ACMPNE p1
  | IF_ICMPEQ p1 -> ByteCode.IF_ICMPEQ p1
  | IF_ICMPGE p1 -> ByteCode.IF_ICMPGE p1
  | IF_ICMPGT p1 -> ByteCode.IF_ICMPGT p1
  | IF_ICMPLE p1 -> ByteCode.IF_ICMPLE p1
  | IF_ICMPLT p1 -> ByteCode.IF_ICMPLT p1
  | IF_ICMPNE p1 -> ByteCode.IF_ICMPNE p1
  | IFEQ p1 -> ByteCode.IFEQ p1
  | IFGE p1 -> ByteCode.IFGE p1
  | IFGT p1 -> ByteCode.IFGT p1
  | IFLE p1 -> ByteCode.IFLE p1
  | IFLT p1 -> ByteCode.IFLT p1
  | IFNE p1 -> ByteCode.IFNE p1
  | IFNONNULL p1 -> ByteCode.IFNONNULL p1
  | IFNULL p1 -> ByteCode.IFNULL p1
  | IINC (p1, p2) -> ByteCode.IINC (p1, p2)
  | ILOAD p1 -> ByteCode.ILOAD p1
  | ILOAD_0 -> ByteCode.ILOAD_0
  | ILOAD_1 -> ByteCode.ILOAD_1
  | ILOAD_2 -> ByteCode.ILOAD_2
  | ILOAD_3 -> ByteCode.ILOAD_3
  | IMUL -> ByteCode.IMUL
  | INEG -> ByteCode.INEG
  | INSTANCEOF p1 -> ByteCode.INSTANCEOF (match p1 with | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t)
  | INVOKEDYNAMIC p1 -> ByteCode.INVOKEDYNAMIC (let bs, mn, md = p1 in let idx = Bootstrap.add_method_specifier bsm bs in ConstantPool.add_invoke_dynamic cpool idx mn md)
  | INVOKEINTERFACE (p1, p2) -> ByteCode.INVOKEINTERFACE ((let c, n, t = p1 in ConstantPool.add_interface_method cpool c n t), p2)
  | INVOKESPECIAL p1 -> ByteCode.INVOKESPECIAL (let c, n, t = p1 in ConstantPool.add_method cpool c n t)
  | INVOKESTATIC p1 -> ByteCode.INVOKESTATIC (let c, n, t = p1 in ConstantPool.add_method cpool c n t)
  | INVOKEVIRTUAL p1 -> ByteCode.INVOKEVIRTUAL (match p1 with | (`Class_or_interface c), n, t -> ConstantPool.add_method cpool c n t | (`Array_type a), n, t -> ConstantPool.add_array_method cpool a n t)
  | IOR -> ByteCode.IOR
  | IREM -> ByteCode.IREM
  | IRETURN -> ByteCode.IRETURN
  | ISHL -> ByteCode.ISHL
  | ISHR -> ByteCode.ISHR
  | ISTORE p1 -> ByteCode.ISTORE p1
  | ISTORE_0 -> ByteCode.ISTORE_0
  | ISTORE_1 -> ByteCode.ISTORE_1
  | ISTORE_2 -> ByteCode.ISTORE_2
  | ISTORE_3 -> ByteCode.ISTORE_3
  | ISUB -> ByteCode.ISUB
  | IUSHR -> ByteCode.IUSHR
  | IXOR -> ByteCode.IXOR
  | JSR p1 -> ByteCode.JSR p1
  | JSR_W p1 -> ByteCode.JSR_W p1
  | L2D -> ByteCode.L2D
  | L2F -> ByteCode.L2F
  | L2I -> ByteCode.L2I
  | LADD -> ByteCode.LADD
  | LALOAD -> ByteCode.LALOAD
  | LAND -> ByteCode.LAND
  | LASTORE -> ByteCode.LASTORE
  | LCMP -> ByteCode.LCMP
  | LCONST_0 -> ByteCode.LCONST_0
  | LCONST_1 -> ByteCode.LCONST_1
  | LDC p1 -> ByteCode.LDC (check_byte (match p1 with | `Int v -> ConstantPool.add_integer cpool v | `Float v -> ConstantPool.add_float cpool v | `String v -> ConstantPool.add_string cpool v | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t | `Method_type v -> ConstantPool.add_method_type cpool v | `Method_handle v -> ConstantPool.add_method_handle cpool (reference v)))
  | LDC2_W p1 -> ByteCode.LDC2_W (match p1 with | `Long v -> ConstantPool.add_long cpool v | `Double v -> ConstantPool.add_double cpool v)
  | LDC_W p1 -> ByteCode.LDC_W (match p1 with | `Int v -> ConstantPool.add_integer cpool v | `Float v -> ConstantPool.add_float cpool v | `String v -> ConstantPool.add_string cpool v | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t | `Method_type v -> ConstantPool.add_method_type cpool v | `Method_handle v -> ConstantPool.add_method_handle cpool (reference v))
  | LDIV -> ByteCode.LDIV
  | LLOAD p1 -> ByteCode.LLOAD p1
  | LLOAD_0 -> ByteCode.LLOAD_0
  | LLOAD_1 -> ByteCode.LLOAD_1
  | LLOAD_2 -> ByteCode.LLOAD_2
  | LLOAD_3 -> ByteCode.LLOAD_3
  | LMUL -> ByteCode.LMUL
  | LNEG -> ByteCode.LNEG
  | LOOKUPSWITCH (p1, p2, p3) -> ByteCode.LOOKUPSWITCH (p1, p2, p3)
  | LOR -> ByteCode.LOR
  | LREM -> ByteCode.LREM
  | LRETURN -> ByteCode.LRETURN
  | LSHL -> ByteCode.LSHL
  | LSHR -> ByteCode.LSHR
  | LSTORE p1 -> ByteCode.LSTORE p1
  | LSTORE_0 -> ByteCode.LSTORE_0
  | LSTORE_1 -> ByteCode.LSTORE_1
  | LSTORE_2 -> ByteCode.LSTORE_2
  | LSTORE_3 -> ByteCode.LSTORE_3
  | LSUB -> ByteCode.LSUB
  | LUSHR -> ByteCode.LUSHR
  | LXOR -> ByteCode.LXOR
  | MONITORENTER -> ByteCode.MONITORENTER
  | MONITOREXIT -> ByteCode.MONITOREXIT
  | MULTIANEWARRAY (p1, p2) -> ByteCode.MULTIANEWARRAY ((match p1 with | `Class_or_interface u -> ConstantPool.add_class cpool u | `Array_type t -> ConstantPool.add_array_class cpool t), p2)
  | NEW p1 -> ByteCode.NEW (ConstantPool.add_class cpool p1)
  | NEWARRAY p1 -> ByteCode.NEWARRAY (u1 (int_of_primitive_array_type p1))
  | NOP -> ByteCode.NOP
  | POP -> ByteCode.POP
  | POP2 -> ByteCode.POP2
  | PUTFIELD p1 -> ByteCode.PUTFIELD (let c, n, t = p1 in ConstantPool.add_field cpool c n t)
  | PUTSTATIC p1 -> ByteCode.PUTSTATIC (let c, n, t = p1 in ConstantPool.add_field cpool c n t)
  | RET p1 -> ByteCode.RET p1
  | RETURN -> ByteCode.RETURN
  | SALOAD -> ByteCode.SALOAD
  | SASTORE -> ByteCode.SASTORE
  | SIPUSH p1 -> ByteCode.SIPUSH p1
  | SWAP -> ByteCode.SWAP
  | TABLESWITCH (p1, p2, p3, p4) -> ByteCode.TABLESWITCH (p1, p2, p3, p4)
  | WIDE_ALOAD p1 -> ByteCode.WIDE_ALOAD p1
  | WIDE_ASTORE p1 -> ByteCode.WIDE_ASTORE p1
  | WIDE_DLOAD p1 -> ByteCode.WIDE_DLOAD p1
  | WIDE_DSTORE p1 -> ByteCode.WIDE_DSTORE p1
  | WIDE_FLOAD p1 -> ByteCode.WIDE_FLOAD p1
  | WIDE_FSTORE p1 -> ByteCode.WIDE_FSTORE p1
  | WIDE_IINC (p1, p2) -> ByteCode.WIDE_IINC (p1, p2)
  | WIDE_ILOAD p1 -> ByteCode.WIDE_ILOAD p1
  | WIDE_ISTORE p1 -> ByteCode.WIDE_ISTORE p1
  | WIDE_LLOAD p1 -> ByteCode.WIDE_LLOAD p1
  | WIDE_LSTORE p1 -> ByteCode.WIDE_LSTORE p1
  | WIDE_RET p1 -> ByteCode.WIDE_RET p1


(* Compilation functions *)

type parameter =
  | Int_constant of int64
  | Offset of int32
  | Float_constant of float
  | String_constant of UTF8.t
  | Class_name of Name.for_class
  | Array_type of UTF8.t
  | Primitive_type of Descriptor.java_type
  | Field of Name.for_class * Name.for_field * Descriptor.for_field
  | Dynamic_method of Bootstrap.method_specifier * Name.for_method * Descriptor.for_method
  | Method of Name.for_class * Name.for_method * Descriptor.for_method
  | Array_method of Descriptor.array_type * Name.for_method * Descriptor.for_method
  | Method_type_constant of Descriptor.for_method
  | Method_handle_constant of Bootstrap.method_handle

type parameters_tail =
  | No_tail
  | Match_offset_pairs of (s4 * long_offset) list
  | Long_offsets of long_offset list

module StringSet = Set.Make (struct type t = string let compare = Pervasives.compare end)

let known_instructions =
  List.fold_left (fun s e -> StringSet.add e s) StringSet.empty [
    "aaload";
    "aastore";
    "aconst_null";
    "aload";
    "aload_0";
    "aload_1";
    "aload_2";
    "aload_3";
    "anewarray";
    "areturn";
    "arraylength";
    "astore";
    "astore_0";
    "astore_1";
    "astore_2";
    "astore_3";
    "athrow";
    "baload";
    "bastore";
    "bipush";
    "caload";
    "castore";
    "checkcast";
    "d2f";
    "d2i";
    "d2l";
    "dadd";
    "daload";
    "dastore";
    "dcmpg";
    "dcmpl";
    "dconst_0";
    "dconst_1";
    "ddiv";
    "dload";
    "dload_0";
    "dload_1";
    "dload_2";
    "dload_3";
    "dmul";
    "dneg";
    "drem";
    "dreturn";
    "dstore";
    "dstore_0";
    "dstore_1";
    "dstore_2";
    "dstore_3";
    "dsub";
    "dup";
    "dup2";
    "dup2_x1";
    "dup2_x2";
    "dup_x1";
    "dup_x2";
    "f2d";
    "f2i";
    "f2l";
    "fadd";
    "faload";
    "fastore";
    "fcmpg";
    "fcmpl";
    "fconst_0";
    "fconst_1";
    "fconst_2";
    "fdiv";
    "fload";
    "fload_0";
    "fload_1";
    "fload_2";
    "fload_3";
    "fmul";
    "fneg";
    "frem";
    "freturn";
    "fstore";
    "fstore_0";
    "fstore_1";
    "fstore_2";
    "fstore_3";
    "fsub";
    "getfield";
    "getstatic";
    "goto";
    "goto_w";
    "i2b";
    "i2c";
    "i2d";
    "i2f";
    "i2l";
    "i2s";
    "iadd";
    "iaload";
    "iand";
    "iastore";
    "iconst_0";
    "iconst_1";
    "iconst_2";
    "iconst_3";
    "iconst_4";
    "iconst_5";
    "iconst_m1";
    "idiv";
    "if_acmpeq";
    "if_acmpne";
    "if_icmpeq";
    "if_icmpge";
    "if_icmpgt";
    "if_icmple";
    "if_icmplt";
    "if_icmpne";
    "ifeq";
    "ifge";
    "ifgt";
    "ifle";
    "iflt";
    "ifne";
    "ifnonnull";
    "ifnull";
    "iinc";
    "iload";
    "iload_0";
    "iload_1";
    "iload_2";
    "iload_3";
    "imul";
    "ineg";
    "instanceof";
    "invokedynamic";
    "invokeinterface";
    "invokespecial";
    "invokestatic";
    "invokevirtual";
    "ior";
    "irem";
    "ireturn";
    "ishl";
    "ishr";
    "istore";
    "istore_0";
    "istore_1";
    "istore_2";
    "istore_3";
    "isub";
    "iushr";
    "ixor";
    "jsr";
    "jsr_w";
    "l2d";
    "l2f";
    "l2i";
    "ladd";
    "laload";
    "land";
    "lastore";
    "lcmp";
    "lconst_0";
    "lconst_1";
    "ldc";
    "ldc2_w";
    "ldc_w";
    "ldiv";
    "lload";
    "lload_0";
    "lload_1";
    "lload_2";
    "lload_3";
    "lmul";
    "lneg";
    "lookupswitch";
    "lor";
    "lrem";
    "lreturn";
    "lshl";
    "lshr";
    "lstore";
    "lstore_0";
    "lstore_1";
    "lstore_2";
    "lstore_3";
    "lsub";
    "lushr";
    "lxor";
    "monitorenter";
    "monitorexit";
    "multianewarray";
    "new";
    "newarray";
    "nop";
    "pop";
    "pop2";
    "putfield";
    "putstatic";
    "ret";
    "return";
    "saload";
    "sastore";
    "sipush";
    "swap";
    "tableswitch";
  ]

let compile ofs wide mnemo params param_tail =
  let array_type_of_utf8 s =
    let rec make_array n t =
      if n = 1 then
        `Array (Descriptor.filter_void Descriptor.Invalid_array_element_type t)
      else
        let sub_array = make_array (pred n) t in
        `Array (Descriptor.filter_void
                  Descriptor.Invalid_array_element_type
                  (sub_array :> Descriptor.java_type)) in
    let idx = UTF8.index_from s 0 opening_square_bracket in
    let base = UTF8.substring s 0 (pred idx) in
    make_array
      (((UTF8.length s) - idx + 1) / 2)
      (match UTF8.to_string base with
      | "boolean" -> `Boolean
      | "byte" -> `Byte
      | "char" -> `Char
      | "double" -> `Double
      | "float" -> `Float
      | "int" -> `Int
      | "long" -> `Long
      | "short" -> `Short
      | "void" -> fail Invalid_array_element
      | _ -> `Class (Name.make_for_class_from_external base)) in
  let read_class_name = function
  | Class_name cn -> cn
  | _ -> fail Invalid_class_name in
  let read_u1 = function
  | Int_constant x when x >= 0L && x <= 256L -> u1 (Int64.to_int x)
  | _ -> fail Invalid_unsigned_byte in
  let read_s1 = function
  | Int_constant x when x >= -128L && x <= 127L -> s1 (Int64.to_int x)
  | _ -> fail Invalid_byte in
  let read_u2 = function
  | Int_constant x when x >= 0L && x <= 65536L -> u2 (Int64.to_int x)
  | _ -> fail Invalid_unsigned_short in
  let read_s2 = function
  | Int_constant x when x >= -32768L && x <= 32767L -> s2 (Int64.to_int x)
  | _ -> fail Invalid_signed_short in
  let read_s4 = function
  | Int_constant x when x >= (Int64.of_int32 Int32.min_int) && x <= (Int64.of_int32 Int32.max_int) -> s4 (Int64.to_int32 x)
  | _ -> fail Invalid_signed_long in
  let read_short_offset = function
  | Offset x when x >= -32768l && x <= 32767l -> s2 (Int32.to_int x)
  | _ -> fail Invalid_short_offset in
  let read_long_offset = function
  | Offset x -> s4 x
  | _ -> fail Invalid_long_offset in
  match wide, mnemo, params, param_tail with
  | false, "aaload", [], No_tail -> 1, AALOAD
  | false, "aastore", [], No_tail -> 1, AASTORE
  | false, "aconst_null", [], No_tail -> 1, ACONST_NULL
  | false, "aload", [p1], No_tail -> 1 + 1, ALOAD (read_u1 p1)
  | false, "aload_0", [], No_tail -> 1, ALOAD_0
  | false, "aload_1", [], No_tail -> 1, ALOAD_1
  | false, "aload_2", [], No_tail -> 1, ALOAD_2
  | false, "aload_3", [], No_tail -> 1, ALOAD_3
  | false, "anewarray", [p1], No_tail -> 1 + 2, ANEWARRAY ((match p1 with | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | _ -> fail Invalid_parameter))
  | false, "areturn", [], No_tail -> 1, ARETURN
  | false, "arraylength", [], No_tail -> 1, ARRAYLENGTH
  | false, "astore", [p1], No_tail -> 1 + 1, ASTORE (read_u1 p1)
  | false, "astore_0", [], No_tail -> 1, ASTORE_0
  | false, "astore_1", [], No_tail -> 1, ASTORE_1
  | false, "astore_2", [], No_tail -> 1, ASTORE_2
  | false, "astore_3", [], No_tail -> 1, ASTORE_3
  | false, "athrow", [], No_tail -> 1, ATHROW
  | false, "baload", [], No_tail -> 1, BALOAD
  | false, "bastore", [], No_tail -> 1, BASTORE
  | false, "bipush", [p1], No_tail -> 1 + 1, BIPUSH (read_s1 p1)
  | false, "caload", [], No_tail -> 1, CALOAD
  | false, "castore", [], No_tail -> 1, CASTORE
  | false, "checkcast", [p1], No_tail -> 1 + 2, CHECKCAST ((match p1 with | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | _ -> fail Invalid_parameter))
  | false, "d2f", [], No_tail -> 1, D2F
  | false, "d2i", [], No_tail -> 1, D2I
  | false, "d2l", [], No_tail -> 1, D2L
  | false, "dadd", [], No_tail -> 1, DADD
  | false, "daload", [], No_tail -> 1, DALOAD
  | false, "dastore", [], No_tail -> 1, DASTORE
  | false, "dcmpg", [], No_tail -> 1, DCMPG
  | false, "dcmpl", [], No_tail -> 1, DCMPL
  | false, "dconst_0", [], No_tail -> 1, DCONST_0
  | false, "dconst_1", [], No_tail -> 1, DCONST_1
  | false, "ddiv", [], No_tail -> 1, DDIV
  | false, "dload", [p1], No_tail -> 1 + 1, DLOAD (read_u1 p1)
  | false, "dload_0", [], No_tail -> 1, DLOAD_0
  | false, "dload_1", [], No_tail -> 1, DLOAD_1
  | false, "dload_2", [], No_tail -> 1, DLOAD_2
  | false, "dload_3", [], No_tail -> 1, DLOAD_3
  | false, "dmul", [], No_tail -> 1, DMUL
  | false, "dneg", [], No_tail -> 1, DNEG
  | false, "drem", [], No_tail -> 1, DREM
  | false, "dreturn", [], No_tail -> 1, DRETURN
  | false, "dstore", [p1], No_tail -> 1 + 1, DSTORE (read_u1 p1)
  | false, "dstore_0", [], No_tail -> 1, DSTORE_0
  | false, "dstore_1", [], No_tail -> 1, DSTORE_1
  | false, "dstore_2", [], No_tail -> 1, DSTORE_2
  | false, "dstore_3", [], No_tail -> 1, DSTORE_3
  | false, "dsub", [], No_tail -> 1, DSUB
  | false, "dup", [], No_tail -> 1, DUP
  | false, "dup2", [], No_tail -> 1, DUP2
  | false, "dup2_x1", [], No_tail -> 1, DUP2_X1
  | false, "dup2_x2", [], No_tail -> 1, DUP2_X2
  | false, "dup_x1", [], No_tail -> 1, DUP_X1
  | false, "dup_x2", [], No_tail -> 1, DUP_X2
  | false, "f2d", [], No_tail -> 1, F2D
  | false, "f2i", [], No_tail -> 1, F2I
  | false, "f2l", [], No_tail -> 1, F2L
  | false, "fadd", [], No_tail -> 1, FADD
  | false, "faload", [], No_tail -> 1, FALOAD
  | false, "fastore", [], No_tail -> 1, FASTORE
  | false, "fcmpg", [], No_tail -> 1, FCMPG
  | false, "fcmpl", [], No_tail -> 1, FCMPL
  | false, "fconst_0", [], No_tail -> 1, FCONST_0
  | false, "fconst_1", [], No_tail -> 1, FCONST_1
  | false, "fconst_2", [], No_tail -> 1, FCONST_2
  | false, "fdiv", [], No_tail -> 1, FDIV
  | false, "fload", [p1], No_tail -> 1 + 1, FLOAD (read_u1 p1)
  | false, "fload_0", [], No_tail -> 1, FLOAD_0
  | false, "fload_1", [], No_tail -> 1, FLOAD_1
  | false, "fload_2", [], No_tail -> 1, FLOAD_2
  | false, "fload_3", [], No_tail -> 1, FLOAD_3
  | false, "fmul", [], No_tail -> 1, FMUL
  | false, "fneg", [], No_tail -> 1, FNEG
  | false, "frem", [], No_tail -> 1, FREM
  | false, "freturn", [], No_tail -> 1, FRETURN
  | false, "fstore", [p1], No_tail -> 1 + 1, FSTORE (read_u1 p1)
  | false, "fstore_0", [], No_tail -> 1, FSTORE_0
  | false, "fstore_1", [], No_tail -> 1, FSTORE_1
  | false, "fstore_2", [], No_tail -> 1, FSTORE_2
  | false, "fstore_3", [], No_tail -> 1, FSTORE_3
  | false, "fsub", [], No_tail -> 1, FSUB
  | false, "getfield", [p1], No_tail -> 1 + 2, GETFIELD (match p1 with Field (x, y, z) -> (x, y, z) | _ -> fail Invalid_field)
  | false, "getstatic", [p1], No_tail -> 1 + 2, GETSTATIC (match p1 with Field (x, y, z) -> (x, y, z) | _ -> fail Invalid_field)
  | false, "goto", [p1], No_tail -> 1 + 2, GOTO (read_short_offset p1)
  | false, "goto_w", [p1], No_tail -> 1 + 4, GOTO_W (read_long_offset p1)
  | false, "i2b", [], No_tail -> 1, I2B
  | false, "i2c", [], No_tail -> 1, I2C
  | false, "i2d", [], No_tail -> 1, I2D
  | false, "i2f", [], No_tail -> 1, I2F
  | false, "i2l", [], No_tail -> 1, I2L
  | false, "i2s", [], No_tail -> 1, I2S
  | false, "iadd", [], No_tail -> 1, IADD
  | false, "iaload", [], No_tail -> 1, IALOAD
  | false, "iand", [], No_tail -> 1, IAND
  | false, "iastore", [], No_tail -> 1, IASTORE
  | false, "iconst_0", [], No_tail -> 1, ICONST_0
  | false, "iconst_1", [], No_tail -> 1, ICONST_1
  | false, "iconst_2", [], No_tail -> 1, ICONST_2
  | false, "iconst_3", [], No_tail -> 1, ICONST_3
  | false, "iconst_4", [], No_tail -> 1, ICONST_4
  | false, "iconst_5", [], No_tail -> 1, ICONST_5
  | false, "iconst_m1", [], No_tail -> 1, ICONST_M1
  | false, "idiv", [], No_tail -> 1, IDIV
  | false, "if_acmpeq", [p1], No_tail -> 1 + 2, IF_ACMPEQ (read_short_offset p1)
  | false, "if_acmpne", [p1], No_tail -> 1 + 2, IF_ACMPNE (read_short_offset p1)
  | false, "if_icmpeq", [p1], No_tail -> 1 + 2, IF_ICMPEQ (read_short_offset p1)
  | false, "if_icmpge", [p1], No_tail -> 1 + 2, IF_ICMPGE (read_short_offset p1)
  | false, "if_icmpgt", [p1], No_tail -> 1 + 2, IF_ICMPGT (read_short_offset p1)
  | false, "if_icmple", [p1], No_tail -> 1 + 2, IF_ICMPLE (read_short_offset p1)
  | false, "if_icmplt", [p1], No_tail -> 1 + 2, IF_ICMPLT (read_short_offset p1)
  | false, "if_icmpne", [p1], No_tail -> 1 + 2, IF_ICMPNE (read_short_offset p1)
  | false, "ifeq", [p1], No_tail -> 1 + 2, IFEQ (read_short_offset p1)
  | false, "ifge", [p1], No_tail -> 1 + 2, IFGE (read_short_offset p1)
  | false, "ifgt", [p1], No_tail -> 1 + 2, IFGT (read_short_offset p1)
  | false, "ifle", [p1], No_tail -> 1 + 2, IFLE (read_short_offset p1)
  | false, "iflt", [p1], No_tail -> 1 + 2, IFLT (read_short_offset p1)
  | false, "ifne", [p1], No_tail -> 1 + 2, IFNE (read_short_offset p1)
  | false, "ifnonnull", [p1], No_tail -> 1 + 2, IFNONNULL (read_short_offset p1)
  | false, "ifnull", [p1], No_tail -> 1 + 2, IFNULL (read_short_offset p1)
  | false, "iinc", [p1; p2], No_tail -> 1 + 1 + 1, IINC ((read_u1 p1), (read_s1 p2))
  | false, "iload", [p1], No_tail -> 1 + 1, ILOAD (read_u1 p1)
  | false, "iload_0", [], No_tail -> 1, ILOAD_0
  | false, "iload_1", [], No_tail -> 1, ILOAD_1
  | false, "iload_2", [], No_tail -> 1, ILOAD_2
  | false, "iload_3", [], No_tail -> 1, ILOAD_3
  | false, "imul", [], No_tail -> 1, IMUL
  | false, "ineg", [], No_tail -> 1, INEG
  | false, "instanceof", [p1], No_tail -> 1 + 2, INSTANCEOF ((match p1 with | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | _ -> fail Invalid_parameter))
  | false, "invokedynamic", [p1], No_tail -> 1 + 2 + 2, INVOKEDYNAMIC (match p1 with Dynamic_method (x, y, z) -> (x, y, z) | _ -> fail Invalid_parameter)
  | false, "invokeinterface", [p1; p2], No_tail -> 1 + 1 + 2 + 1, INVOKEINTERFACE ((match p1 with Method (x, y, z) -> (x, y, z) | _ -> fail Invalid_interface_method), (read_u1 p2))
  | false, "invokespecial", [p1], No_tail -> 1 + 2, INVOKESPECIAL (match p1 with Method (x, y, z) -> (x, y, z) | _ -> fail Invalid_method)
  | false, "invokestatic", [p1], No_tail -> 1 + 2, INVOKESTATIC (match p1 with Method (x, y, z) -> (x, y, z) | _ -> fail Invalid_method)
  | false, "invokevirtual", [p1], No_tail -> 1 + 2, INVOKEVIRTUAL (match p1 with Method (x, y, z) -> ((`Class_or_interface x), y, z) | Array_method (x, y, z) -> ((`Array_type x), y, z) | _ -> fail Invalid_method)
  | false, "ior", [], No_tail -> 1, IOR
  | false, "irem", [], No_tail -> 1, IREM
  | false, "ireturn", [], No_tail -> 1, IRETURN
  | false, "ishl", [], No_tail -> 1, ISHL
  | false, "ishr", [], No_tail -> 1, ISHR
  | false, "istore", [p1], No_tail -> 1 + 1, ISTORE (read_u1 p1)
  | false, "istore_0", [], No_tail -> 1, ISTORE_0
  | false, "istore_1", [], No_tail -> 1, ISTORE_1
  | false, "istore_2", [], No_tail -> 1, ISTORE_2
  | false, "istore_3", [], No_tail -> 1, ISTORE_3
  | false, "isub", [], No_tail -> 1, ISUB
  | false, "iushr", [], No_tail -> 1, IUSHR
  | false, "ixor", [], No_tail -> 1, IXOR
  | false, "jsr", [p1], No_tail -> 1 + 2, JSR (read_short_offset p1)
  | false, "jsr_w", [p1], No_tail -> 1 + 4, JSR_W (read_long_offset p1)
  | false, "l2d", [], No_tail -> 1, L2D
  | false, "l2f", [], No_tail -> 1, L2F
  | false, "l2i", [], No_tail -> 1, L2I
  | false, "ladd", [], No_tail -> 1, LADD
  | false, "laload", [], No_tail -> 1, LALOAD
  | false, "land", [], No_tail -> 1, LAND
  | false, "lastore", [], No_tail -> 1, LASTORE
  | false, "lcmp", [], No_tail -> 1, LCMP
  | false, "lconst_0", [], No_tail -> 1, LCONST_0
  | false, "lconst_1", [], No_tail -> 1, LCONST_1
  | false, "ldc", [p1], No_tail -> 1 + 1, LDC ((match p1 with | Int_constant _ -> `Int ((read_s4 p1) :> int32) | Float_constant x -> `Float x | String_constant x -> `String x | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | Method_type_constant x -> `Method_type x | Method_handle_constant x -> `Method_handle x | _ -> fail Invalid_parameter))
  | false, "ldc2_w", [p1], No_tail -> 1 + 2, LDC2_W ((match p1 with | Int_constant x -> `Long x | Float_constant x -> `Double x | _ -> fail Invalid_parameter))
  | false, "ldc_w", [p1], No_tail -> 1 + 2, LDC_W ((match p1 with | Int_constant _ -> `Int ((read_s4 p1) :> int32) | Float_constant x -> `Float x | String_constant x -> `String x | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | Method_type_constant x -> `Method_type x | Method_handle_constant x -> `Method_handle x | _ -> fail Invalid_parameter))
  | false, "ldiv", [], No_tail -> 1, LDIV
  | false, "lload", [p1], No_tail -> 1 + 1, LLOAD (read_u1 p1)
  | false, "lload_0", [], No_tail -> 1, LLOAD_0
  | false, "lload_1", [], No_tail -> 1, LLOAD_1
  | false, "lload_2", [], No_tail -> 1, LLOAD_2
  | false, "lload_3", [], No_tail -> 1, LLOAD_3
  | false, "lmul", [], No_tail -> 1, LMUL
  | false, "lneg", [], No_tail -> 1, LNEG
  | false, "lookupswitch", [p1; p2], (Match_offset_pairs t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + (8 * (List.length t)), LOOKUPSWITCH ((read_long_offset p1), (read_s4 p2), (if ((read_s4 p2) :> int32) <> Int32.of_int (List.length t) then fail Invalid_switch_cases else t))
  | false, "lor", [], No_tail -> 1, LOR
  | false, "lrem", [], No_tail -> 1, LREM
  | false, "lreturn", [], No_tail -> 1, LRETURN
  | false, "lshl", [], No_tail -> 1, LSHL
  | false, "lshr", [], No_tail -> 1, LSHR
  | false, "lstore", [p1], No_tail -> 1 + 1, LSTORE (read_u1 p1)
  | false, "lstore_0", [], No_tail -> 1, LSTORE_0
  | false, "lstore_1", [], No_tail -> 1, LSTORE_1
  | false, "lstore_2", [], No_tail -> 1, LSTORE_2
  | false, "lstore_3", [], No_tail -> 1, LSTORE_3
  | false, "lsub", [], No_tail -> 1, LSUB
  | false, "lushr", [], No_tail -> 1, LUSHR
  | false, "lxor", [], No_tail -> 1, LXOR
  | false, "monitorenter", [], No_tail -> 1, MONITORENTER
  | false, "monitorexit", [], No_tail -> 1, MONITOREXIT
  | false, "multianewarray", [p1; p2], No_tail -> 1 + 2 + 1, MULTIANEWARRAY (((match p1 with | Class_name x -> `Class_or_interface x | Array_type x -> `Array_type (array_type_of_utf8 x) | _ -> fail Invalid_parameter)), (read_u1 p2))
  | false, "new", [p1], No_tail -> 1 + 2, NEW ((read_class_name p1))
  | false, "newarray", [p1], No_tail -> 1 + 1, NEWARRAY (match p1 with | Primitive_type pt when Descriptor.is_primitive pt -> pt | _ -> fail Invalid_primitive_type)
  | false, "nop", [], No_tail -> 1, NOP
  | false, "pop", [], No_tail -> 1, POP
  | false, "pop2", [], No_tail -> 1, POP2
  | false, "putfield", [p1], No_tail -> 1 + 2, PUTFIELD (match p1 with Field (x, y, z) -> (x, y, z) | _ -> fail Invalid_field)
  | false, "putstatic", [p1], No_tail -> 1 + 2, PUTSTATIC (match p1 with Field (x, y, z) -> (x, y, z) | _ -> fail Invalid_field)
  | false, "ret", [p1], No_tail -> 1 + 1, RET (read_u1 p1)
  | false, "return", [], No_tail -> 1, RETURN
  | false, "saload", [], No_tail -> 1, SALOAD
  | false, "sastore", [], No_tail -> 1, SASTORE
  | false, "sipush", [p1], No_tail -> 1 + 2, SIPUSH (read_s2 p1)
  | false, "swap", [], No_tail -> 1, SWAP
  | false, "tableswitch", [p1; p2; p3], (Long_offsets t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + 4 + (4 * (List.length t)), TABLESWITCH ((read_long_offset p1), (read_s4 p2), (read_s4 p3), (if (Int32.succ (Int32.sub ((read_s4 p3) :> int32) ((read_s4 p2) :> int32))) <> Int32.of_int (List.length t) then fail Invalid_switch_cases else t))
  | true, "aload", [p1], No_tail -> 2 + 2, WIDE_ALOAD (read_u2 p1)
  | true, "astore", [p1], No_tail -> 2 + 2, WIDE_ASTORE (read_u2 p1)
  | true, "dload", [p1], No_tail -> 2 + 2, WIDE_DLOAD (read_u2 p1)
  | true, "dstore", [p1], No_tail -> 2 + 2, WIDE_DSTORE (read_u2 p1)
  | true, "fload", [p1], No_tail -> 2 + 2, WIDE_FLOAD (read_u2 p1)
  | true, "fstore", [p1], No_tail -> 2 + 2, WIDE_FSTORE (read_u2 p1)
  | true, "iinc", [p1; p2], No_tail -> 2 + 2 + 2, WIDE_IINC ((read_u2 p1), (read_s2 p2))
  | true, "iload", [p1], No_tail -> 2 + 2, WIDE_ILOAD (read_u2 p1)
  | true, "istore", [p1], No_tail -> 2 + 2, WIDE_ISTORE (read_u2 p1)
  | true, "lload", [p1], No_tail -> 2 + 2, WIDE_LLOAD (read_u2 p1)
  | true, "lstore", [p1], No_tail -> 2 + 2, WIDE_LSTORE (read_u2 p1)
  | true, "ret", [p1], No_tail -> 2 + 2, WIDE_RET (read_u2 p1)
  | _ -> fail (if StringSet.mem mnemo known_instructions then Invalid_number_of_arguments else Unknown_instruction)

let decompile ofs i =
  let rec utf8_of_java_type = function
    | `Boolean -> UTF8.of_string "boolean"
    | `Byte -> UTF8.of_string "byte"
    | `Char -> UTF8.of_string "char"
    | `Double -> UTF8.of_string "double"
    | `Float -> UTF8.of_string "float"
    | `Int -> UTF8.of_string "int"
    | `Long -> UTF8.of_string "long"
    | `Short -> UTF8.of_string "short"
    | `Void -> UTF8.of_string "void"
    | `Class c -> Name.external_utf8_for_class c
    | `Array jt -> (utf8_of_java_type jt) ++ UTF8.of_string "[]" in
  let utf8_of_array_type = function
    | `Array jt -> (utf8_of_java_type jt) ++ UTF8.of_string "[]" in
  match i with
  | AALOAD -> 1, false, "aaload", [], No_tail
  | AASTORE -> 1, false, "aastore", [], No_tail
  | ACONST_NULL -> 1, false, "aconst_null", [], No_tail
  | ALOAD p1 -> 1 + 1, false, "aload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | ALOAD_0 -> 1, false, "aload_0", [], No_tail
  | ALOAD_1 -> 1, false, "aload_1", [], No_tail
  | ALOAD_2 -> 1, false, "aload_2", [], No_tail
  | ALOAD_3 -> 1, false, "aload_3", [], No_tail
  | ANEWARRAY p1 -> 1 + 2, false, "anewarray", [((match p1 with | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x)))], No_tail
  | ARETURN -> 1, false, "areturn", [], No_tail
  | ARRAYLENGTH -> 1, false, "arraylength", [], No_tail
  | ASTORE p1 -> 1 + 1, false, "astore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | ASTORE_0 -> 1, false, "astore_0", [], No_tail
  | ASTORE_1 -> 1, false, "astore_1", [], No_tail
  | ASTORE_2 -> 1, false, "astore_2", [], No_tail
  | ASTORE_3 -> 1, false, "astore_3", [], No_tail
  | ATHROW -> 1, false, "athrow", [], No_tail
  | BALOAD -> 1, false, "baload", [], No_tail
  | BASTORE -> 1, false, "bastore", [], No_tail
  | BIPUSH p1 -> 1 + 1, false, "bipush", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | CALOAD -> 1, false, "caload", [], No_tail
  | CASTORE -> 1, false, "castore", [], No_tail
  | CHECKCAST p1 -> 1 + 2, false, "checkcast", [((match p1 with | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x)))], No_tail
  | D2F -> 1, false, "d2f", [], No_tail
  | D2I -> 1, false, "d2i", [], No_tail
  | D2L -> 1, false, "d2l", [], No_tail
  | DADD -> 1, false, "dadd", [], No_tail
  | DALOAD -> 1, false, "daload", [], No_tail
  | DASTORE -> 1, false, "dastore", [], No_tail
  | DCMPG -> 1, false, "dcmpg", [], No_tail
  | DCMPL -> 1, false, "dcmpl", [], No_tail
  | DCONST_0 -> 1, false, "dconst_0", [], No_tail
  | DCONST_1 -> 1, false, "dconst_1", [], No_tail
  | DDIV -> 1, false, "ddiv", [], No_tail
  | DLOAD p1 -> 1 + 1, false, "dload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | DLOAD_0 -> 1, false, "dload_0", [], No_tail
  | DLOAD_1 -> 1, false, "dload_1", [], No_tail
  | DLOAD_2 -> 1, false, "dload_2", [], No_tail
  | DLOAD_3 -> 1, false, "dload_3", [], No_tail
  | DMUL -> 1, false, "dmul", [], No_tail
  | DNEG -> 1, false, "dneg", [], No_tail
  | DREM -> 1, false, "drem", [], No_tail
  | DRETURN -> 1, false, "dreturn", [], No_tail
  | DSTORE p1 -> 1 + 1, false, "dstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | DSTORE_0 -> 1, false, "dstore_0", [], No_tail
  | DSTORE_1 -> 1, false, "dstore_1", [], No_tail
  | DSTORE_2 -> 1, false, "dstore_2", [], No_tail
  | DSTORE_3 -> 1, false, "dstore_3", [], No_tail
  | DSUB -> 1, false, "dsub", [], No_tail
  | DUP -> 1, false, "dup", [], No_tail
  | DUP2 -> 1, false, "dup2", [], No_tail
  | DUP2_X1 -> 1, false, "dup2_x1", [], No_tail
  | DUP2_X2 -> 1, false, "dup2_x2", [], No_tail
  | DUP_X1 -> 1, false, "dup_x1", [], No_tail
  | DUP_X2 -> 1, false, "dup_x2", [], No_tail
  | F2D -> 1, false, "f2d", [], No_tail
  | F2I -> 1, false, "f2i", [], No_tail
  | F2L -> 1, false, "f2l", [], No_tail
  | FADD -> 1, false, "fadd", [], No_tail
  | FALOAD -> 1, false, "faload", [], No_tail
  | FASTORE -> 1, false, "fastore", [], No_tail
  | FCMPG -> 1, false, "fcmpg", [], No_tail
  | FCMPL -> 1, false, "fcmpl", [], No_tail
  | FCONST_0 -> 1, false, "fconst_0", [], No_tail
  | FCONST_1 -> 1, false, "fconst_1", [], No_tail
  | FCONST_2 -> 1, false, "fconst_2", [], No_tail
  | FDIV -> 1, false, "fdiv", [], No_tail
  | FLOAD p1 -> 1 + 1, false, "fload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | FLOAD_0 -> 1, false, "fload_0", [], No_tail
  | FLOAD_1 -> 1, false, "fload_1", [], No_tail
  | FLOAD_2 -> 1, false, "fload_2", [], No_tail
  | FLOAD_3 -> 1, false, "fload_3", [], No_tail
  | FMUL -> 1, false, "fmul", [], No_tail
  | FNEG -> 1, false, "fneg", [], No_tail
  | FREM -> 1, false, "frem", [], No_tail
  | FRETURN -> 1, false, "freturn", [], No_tail
  | FSTORE p1 -> 1 + 1, false, "fstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | FSTORE_0 -> 1, false, "fstore_0", [], No_tail
  | FSTORE_1 -> 1, false, "fstore_1", [], No_tail
  | FSTORE_2 -> 1, false, "fstore_2", [], No_tail
  | FSTORE_3 -> 1, false, "fstore_3", [], No_tail
  | FSUB -> 1, false, "fsub", [], No_tail
  | GETFIELD p1 -> 1 + 2, false, "getfield", [(match p1 with (x, y, z) -> Field (x, y, z))], No_tail
  | GETSTATIC p1 -> 1 + 2, false, "getstatic", [(match p1 with (x, y, z) -> Field (x, y, z))], No_tail
  | GOTO p1 -> 1 + 2, false, "goto", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | GOTO_W p1 -> 1 + 4, false, "goto_w", [(Offset (p1 :> int32))], No_tail
  | I2B -> 1, false, "i2b", [], No_tail
  | I2C -> 1, false, "i2c", [], No_tail
  | I2D -> 1, false, "i2d", [], No_tail
  | I2F -> 1, false, "i2f", [], No_tail
  | I2L -> 1, false, "i2l", [], No_tail
  | I2S -> 1, false, "i2s", [], No_tail
  | IADD -> 1, false, "iadd", [], No_tail
  | IALOAD -> 1, false, "iaload", [], No_tail
  | IAND -> 1, false, "iand", [], No_tail
  | IASTORE -> 1, false, "iastore", [], No_tail
  | ICONST_0 -> 1, false, "iconst_0", [], No_tail
  | ICONST_1 -> 1, false, "iconst_1", [], No_tail
  | ICONST_2 -> 1, false, "iconst_2", [], No_tail
  | ICONST_3 -> 1, false, "iconst_3", [], No_tail
  | ICONST_4 -> 1, false, "iconst_4", [], No_tail
  | ICONST_5 -> 1, false, "iconst_5", [], No_tail
  | ICONST_M1 -> 1, false, "iconst_m1", [], No_tail
  | IDIV -> 1, false, "idiv", [], No_tail
  | IF_ACMPEQ p1 -> 1 + 2, false, "if_acmpeq", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ACMPNE p1 -> 1 + 2, false, "if_acmpne", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPEQ p1 -> 1 + 2, false, "if_icmpeq", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPGE p1 -> 1 + 2, false, "if_icmpge", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPGT p1 -> 1 + 2, false, "if_icmpgt", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPLE p1 -> 1 + 2, false, "if_icmple", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPLT p1 -> 1 + 2, false, "if_icmplt", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IF_ICMPNE p1 -> 1 + 2, false, "if_icmpne", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFEQ p1 -> 1 + 2, false, "ifeq", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFGE p1 -> 1 + 2, false, "ifge", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFGT p1 -> 1 + 2, false, "ifgt", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFLE p1 -> 1 + 2, false, "ifle", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFLT p1 -> 1 + 2, false, "iflt", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFNE p1 -> 1 + 2, false, "ifne", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFNONNULL p1 -> 1 + 2, false, "ifnonnull", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IFNULL p1 -> 1 + 2, false, "ifnull", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | IINC (p1, p2) -> 1 + 1 + 1, false, "iinc", [(Int_constant (Int64.of_int (p1 :> int))); (Int_constant (Int64.of_int (p2 :> int)))], No_tail
  | ILOAD p1 -> 1 + 1, false, "iload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | ILOAD_0 -> 1, false, "iload_0", [], No_tail
  | ILOAD_1 -> 1, false, "iload_1", [], No_tail
  | ILOAD_2 -> 1, false, "iload_2", [], No_tail
  | ILOAD_3 -> 1, false, "iload_3", [], No_tail
  | IMUL -> 1, false, "imul", [], No_tail
  | INEG -> 1, false, "ineg", [], No_tail
  | INSTANCEOF p1 -> 1 + 2, false, "instanceof", [((match p1 with | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x)))], No_tail
  | INVOKEDYNAMIC p1 -> 1 + 2 + 2, false, "invokedynamic", [(match p1 with (x, y, z) -> Dynamic_method (x, y, z))], No_tail
  | INVOKEINTERFACE (p1, p2) -> 1 + 1 + 2 + 1, false, "invokeinterface", [(match p1 with (x, y, z) -> Method (x, y, z)); (Int_constant (Int64.of_int (p2 :> int)))], No_tail
  | INVOKESPECIAL p1 -> 1 + 2, false, "invokespecial", [(match p1 with (x, y, z) -> Method (x, y, z))], No_tail
  | INVOKESTATIC p1 -> 1 + 2, false, "invokestatic", [(match p1 with (x, y, z) -> Method (x, y, z))], No_tail
  | INVOKEVIRTUAL p1 -> 1 + 2, false, "invokevirtual", [(match p1 with ((`Class_or_interface x), y, z) -> Method (x, y, z) | ((`Array_type x), y, z) -> Array_method (x, y, z))], No_tail
  | IOR -> 1, false, "ior", [], No_tail
  | IREM -> 1, false, "irem", [], No_tail
  | IRETURN -> 1, false, "ireturn", [], No_tail
  | ISHL -> 1, false, "ishl", [], No_tail
  | ISHR -> 1, false, "ishr", [], No_tail
  | ISTORE p1 -> 1 + 1, false, "istore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | ISTORE_0 -> 1, false, "istore_0", [], No_tail
  | ISTORE_1 -> 1, false, "istore_1", [], No_tail
  | ISTORE_2 -> 1, false, "istore_2", [], No_tail
  | ISTORE_3 -> 1, false, "istore_3", [], No_tail
  | ISUB -> 1, false, "isub", [], No_tail
  | IUSHR -> 1, false, "iushr", [], No_tail
  | IXOR -> 1, false, "ixor", [], No_tail
  | JSR p1 -> 1 + 2, false, "jsr", [(Offset (Int32.of_int (p1 :> int)))], No_tail
  | JSR_W p1 -> 1 + 4, false, "jsr_w", [(Offset (p1 :> int32))], No_tail
  | L2D -> 1, false, "l2d", [], No_tail
  | L2F -> 1, false, "l2f", [], No_tail
  | L2I -> 1, false, "l2i", [], No_tail
  | LADD -> 1, false, "ladd", [], No_tail
  | LALOAD -> 1, false, "laload", [], No_tail
  | LAND -> 1, false, "land", [], No_tail
  | LASTORE -> 1, false, "lastore", [], No_tail
  | LCMP -> 1, false, "lcmp", [], No_tail
  | LCONST_0 -> 1, false, "lconst_0", [], No_tail
  | LCONST_1 -> 1, false, "lconst_1", [], No_tail
  | LDC p1 -> 1 + 1, false, "ldc", [((match p1 with | `Int x -> Int_constant (Int64.of_int32 x) | `Float x -> Float_constant x | `String x -> String_constant x | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x) | `Method_type x -> Method_type_constant x | `Method_handle x -> Method_handle_constant x))], No_tail
  | LDC2_W p1 -> 1 + 2, false, "ldc2_w", [((match p1 with | `Long x -> Int_constant x | `Double x -> Float_constant x))], No_tail
  | LDC_W p1 -> 1 + 2, false, "ldc_w", [((match p1 with | `Int x -> Int_constant (Int64.of_int32 x) | `Float x -> Float_constant x | `String x -> String_constant x | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x) | `Method_type x -> Method_type_constant x | `Method_handle x -> Method_handle_constant x))], No_tail
  | LDIV -> 1, false, "ldiv", [], No_tail
  | LLOAD p1 -> 1 + 1, false, "lload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | LLOAD_0 -> 1, false, "lload_0", [], No_tail
  | LLOAD_1 -> 1, false, "lload_1", [], No_tail
  | LLOAD_2 -> 1, false, "lload_2", [], No_tail
  | LLOAD_3 -> 1, false, "lload_3", [], No_tail
  | LMUL -> 1, false, "lmul", [], No_tail
  | LNEG -> 1, false, "lneg", [], No_tail
  | LOOKUPSWITCH (p1, p2, t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + (8 * (List.length t)), false, "lookupswitch", [(Offset (p1 :> int32)); (Int_constant (Int64.of_int32 (p2 :> int32)))], (if (p2 :> int32) <> Int32.of_int (List.length t) then fail Invalid_switch_cases else (Match_offset_pairs t))
  | LOR -> 1, false, "lor", [], No_tail
  | LREM -> 1, false, "lrem", [], No_tail
  | LRETURN -> 1, false, "lreturn", [], No_tail
  | LSHL -> 1, false, "lshl", [], No_tail
  | LSHR -> 1, false, "lshr", [], No_tail
  | LSTORE p1 -> 1 + 1, false, "lstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | LSTORE_0 -> 1, false, "lstore_0", [], No_tail
  | LSTORE_1 -> 1, false, "lstore_1", [], No_tail
  | LSTORE_2 -> 1, false, "lstore_2", [], No_tail
  | LSTORE_3 -> 1, false, "lstore_3", [], No_tail
  | LSUB -> 1, false, "lsub", [], No_tail
  | LUSHR -> 1, false, "lushr", [], No_tail
  | LXOR -> 1, false, "lxor", [], No_tail
  | MONITORENTER -> 1, false, "monitorenter", [], No_tail
  | MONITOREXIT -> 1, false, "monitorexit", [], No_tail
  | MULTIANEWARRAY (p1, p2) -> 1 + 2 + 1, false, "multianewarray", [((match p1 with | `Class_or_interface x -> Class_name x | `Array_type x -> Array_type (utf8_of_array_type x))); (Int_constant (Int64.of_int (p2 :> int)))], No_tail
  | NEW p1 -> 1 + 2, false, "new", [(Class_name p1)], No_tail
  | NEWARRAY p1 -> 1 + 1, false, "newarray", [(Primitive_type p1)], No_tail
  | NOP -> 1, false, "nop", [], No_tail
  | POP -> 1, false, "pop", [], No_tail
  | POP2 -> 1, false, "pop2", [], No_tail
  | PUTFIELD p1 -> 1 + 2, false, "putfield", [(match p1 with (x, y, z) -> Field (x, y, z))], No_tail
  | PUTSTATIC p1 -> 1 + 2, false, "putstatic", [(match p1 with (x, y, z) -> Field (x, y, z))], No_tail
  | RET p1 -> 1 + 1, false, "ret", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | RETURN -> 1, false, "return", [], No_tail
  | SALOAD -> 1, false, "saload", [], No_tail
  | SASTORE -> 1, false, "sastore", [], No_tail
  | SIPUSH p1 -> 1 + 2, false, "sipush", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | SWAP -> 1, false, "swap", [], No_tail
  | TABLESWITCH (p1, p2, p3, t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + 4 + (4 * (List.length t)), false, "tableswitch", [(Offset (p1 :> int32)); (Int_constant (Int64.of_int32 (p2 :> int32))); (Int_constant (Int64.of_int32 (p3 :> int32)))], (if (Int32.succ (Int32.sub (p3 :> int32) (p2 :> int32))) <> Int32.of_int (List.length t) then fail Invalid_switch_cases else (Long_offsets t))
  | WIDE_ALOAD p1 -> 2 + 2, true, "aload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_ASTORE p1 -> 2 + 2, true, "astore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_DLOAD p1 -> 2 + 2, true, "dload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_DSTORE p1 -> 2 + 2, true, "dstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_FLOAD p1 -> 2 + 2, true, "fload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_FSTORE p1 -> 2 + 2, true, "fstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_IINC (p1, p2) -> 2 + 2 + 2, true, "iinc", [(Int_constant (Int64.of_int (p1 :> int))); (Int_constant (Int64.of_int (p2 :> int)))], No_tail
  | WIDE_ILOAD p1 -> 2 + 2, true, "iload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_ISTORE p1 -> 2 + 2, true, "istore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_LLOAD p1 -> 2 + 2, true, "lload", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_LSTORE p1 -> 2 + 2, true, "lstore", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail
  | WIDE_RET p1 -> 2 + 2, true, "ret", [(Int_constant (Int64.of_int (p1 :> int)))], No_tail


(* Sizes *)

let size_of ofs = function
  | AALOAD -> 1
  | AASTORE -> 1
  | ACONST_NULL -> 1
  | ALOAD _ -> 1 + 1
  | ALOAD_0 -> 1
  | ALOAD_1 -> 1
  | ALOAD_2 -> 1
  | ALOAD_3 -> 1
  | ANEWARRAY _ -> 1 + 2
  | ARETURN -> 1
  | ARRAYLENGTH -> 1
  | ASTORE _ -> 1 + 1
  | ASTORE_0 -> 1
  | ASTORE_1 -> 1
  | ASTORE_2 -> 1
  | ASTORE_3 -> 1
  | ATHROW -> 1
  | BALOAD -> 1
  | BASTORE -> 1
  | BIPUSH _ -> 1 + 1
  | CALOAD -> 1
  | CASTORE -> 1
  | CHECKCAST _ -> 1 + 2
  | D2F -> 1
  | D2I -> 1
  | D2L -> 1
  | DADD -> 1
  | DALOAD -> 1
  | DASTORE -> 1
  | DCMPG -> 1
  | DCMPL -> 1
  | DCONST_0 -> 1
  | DCONST_1 -> 1
  | DDIV -> 1
  | DLOAD _ -> 1 + 1
  | DLOAD_0 -> 1
  | DLOAD_1 -> 1
  | DLOAD_2 -> 1
  | DLOAD_3 -> 1
  | DMUL -> 1
  | DNEG -> 1
  | DREM -> 1
  | DRETURN -> 1
  | DSTORE _ -> 1 + 1
  | DSTORE_0 -> 1
  | DSTORE_1 -> 1
  | DSTORE_2 -> 1
  | DSTORE_3 -> 1
  | DSUB -> 1
  | DUP -> 1
  | DUP2 -> 1
  | DUP2_X1 -> 1
  | DUP2_X2 -> 1
  | DUP_X1 -> 1
  | DUP_X2 -> 1
  | F2D -> 1
  | F2I -> 1
  | F2L -> 1
  | FADD -> 1
  | FALOAD -> 1
  | FASTORE -> 1
  | FCMPG -> 1
  | FCMPL -> 1
  | FCONST_0 -> 1
  | FCONST_1 -> 1
  | FCONST_2 -> 1
  | FDIV -> 1
  | FLOAD _ -> 1 + 1
  | FLOAD_0 -> 1
  | FLOAD_1 -> 1
  | FLOAD_2 -> 1
  | FLOAD_3 -> 1
  | FMUL -> 1
  | FNEG -> 1
  | FREM -> 1
  | FRETURN -> 1
  | FSTORE _ -> 1 + 1
  | FSTORE_0 -> 1
  | FSTORE_1 -> 1
  | FSTORE_2 -> 1
  | FSTORE_3 -> 1
  | FSUB -> 1
  | GETFIELD _ -> 1 + 2
  | GETSTATIC _ -> 1 + 2
  | GOTO _ -> 1 + 2
  | GOTO_W _ -> 1 + 4
  | I2B -> 1
  | I2C -> 1
  | I2D -> 1
  | I2F -> 1
  | I2L -> 1
  | I2S -> 1
  | IADD -> 1
  | IALOAD -> 1
  | IAND -> 1
  | IASTORE -> 1
  | ICONST_0 -> 1
  | ICONST_1 -> 1
  | ICONST_2 -> 1
  | ICONST_3 -> 1
  | ICONST_4 -> 1
  | ICONST_5 -> 1
  | ICONST_M1 -> 1
  | IDIV -> 1
  | IF_ACMPEQ _ -> 1 + 2
  | IF_ACMPNE _ -> 1 + 2
  | IF_ICMPEQ _ -> 1 + 2
  | IF_ICMPGE _ -> 1 + 2
  | IF_ICMPGT _ -> 1 + 2
  | IF_ICMPLE _ -> 1 + 2
  | IF_ICMPLT _ -> 1 + 2
  | IF_ICMPNE _ -> 1 + 2
  | IFEQ _ -> 1 + 2
  | IFGE _ -> 1 + 2
  | IFGT _ -> 1 + 2
  | IFLE _ -> 1 + 2
  | IFLT _ -> 1 + 2
  | IFNE _ -> 1 + 2
  | IFNONNULL _ -> 1 + 2
  | IFNULL _ -> 1 + 2
  | IINC _ -> 1 + 1 + 1
  | ILOAD _ -> 1 + 1
  | ILOAD_0 -> 1
  | ILOAD_1 -> 1
  | ILOAD_2 -> 1
  | ILOAD_3 -> 1
  | IMUL -> 1
  | INEG -> 1
  | INSTANCEOF _ -> 1 + 2
  | INVOKEDYNAMIC _ -> 1 + 2 + 2
  | INVOKEINTERFACE _ -> 1 + 1 + 2 + 1
  | INVOKESPECIAL _ -> 1 + 2
  | INVOKESTATIC _ -> 1 + 2
  | INVOKEVIRTUAL _ -> 1 + 2
  | IOR -> 1
  | IREM -> 1
  | IRETURN -> 1
  | ISHL -> 1
  | ISHR -> 1
  | ISTORE _ -> 1 + 1
  | ISTORE_0 -> 1
  | ISTORE_1 -> 1
  | ISTORE_2 -> 1
  | ISTORE_3 -> 1
  | ISUB -> 1
  | IUSHR -> 1
  | IXOR -> 1
  | JSR _ -> 1 + 2
  | JSR_W _ -> 1 + 4
  | L2D -> 1
  | L2F -> 1
  | L2I -> 1
  | LADD -> 1
  | LALOAD -> 1
  | LAND -> 1
  | LASTORE -> 1
  | LCMP -> 1
  | LCONST_0 -> 1
  | LCONST_1 -> 1
  | LDC _ -> 1 + 1
  | LDC2_W _ -> 1 + 2
  | LDC_W _ -> 1 + 2
  | LDIV -> 1
  | LLOAD _ -> 1 + 1
  | LLOAD_0 -> 1
  | LLOAD_1 -> 1
  | LLOAD_2 -> 1
  | LLOAD_3 -> 1
  | LMUL -> 1
  | LNEG -> 1
  | LOOKUPSWITCH (_, _, t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + (8 * (List.length t))
  | LOR -> 1
  | LREM -> 1
  | LRETURN -> 1
  | LSHL -> 1
  | LSHR -> 1
  | LSTORE _ -> 1 + 1
  | LSTORE_0 -> 1
  | LSTORE_1 -> 1
  | LSTORE_2 -> 1
  | LSTORE_3 -> 1
  | LSUB -> 1
  | LUSHR -> 1
  | LXOR -> 1
  | MONITORENTER -> 1
  | MONITOREXIT -> 1
  | MULTIANEWARRAY _ -> 1 + 2 + 1
  | NEW _ -> 1 + 2
  | NEWARRAY _ -> 1 + 1
  | NOP -> 1
  | POP -> 1
  | POP2 -> 1
  | PUTFIELD _ -> 1 + 2
  | PUTSTATIC _ -> 1 + 2
  | RET _ -> 1 + 1
  | RETURN -> 1
  | SALOAD -> 1
  | SASTORE -> 1
  | SIPUSH _ -> 1 + 2
  | SWAP -> 1
  | TABLESWITCH (_, _, _, t) -> 1 + (3 - (ofs mod 4)) + 4 + 4 + 4 + (4 * (List.length t))
  | WIDE_ALOAD _ -> 2 + 2
  | WIDE_ASTORE _ -> 2 + 2
  | WIDE_DLOAD _ -> 2 + 2
  | WIDE_DSTORE _ -> 2 + 2
  | WIDE_FLOAD _ -> 2 + 2
  | WIDE_FSTORE _ -> 2 + 2
  | WIDE_IINC _ -> 2 + 2 + 2
  | WIDE_ILOAD _ -> 2 + 2
  | WIDE_ISTORE _ -> 2 + 2
  | WIDE_LLOAD _ -> 2 + 2
  | WIDE_LSTORE _ -> 2 + 2
  | WIDE_RET _ -> 2 + 2

let size_of_list ofs l =
  let ofs' = List.fold_left (fun acc elem -> acc + size_of acc elem) ofs l in
  ofs' - ofs


(* Versions *)

let version_bounds = function
  | AALOAD -> Version.make_bounds "'AALOAD' instruction" Version.Java_1_0 None
  | AASTORE -> Version.make_bounds "'AASTORE' instruction" Version.Java_1_0 None
  | ACONST_NULL -> Version.make_bounds "'ACONST_NULL' instruction" Version.Java_1_0 None
  | ALOAD _ -> Version.make_bounds "'ALOAD' instruction" Version.Java_1_0 None
  | ALOAD_0 -> Version.make_bounds "'ALOAD_0' instruction" Version.Java_1_0 None
  | ALOAD_1 -> Version.make_bounds "'ALOAD_1' instruction" Version.Java_1_0 None
  | ALOAD_2 -> Version.make_bounds "'ALOAD_2' instruction" Version.Java_1_0 None
  | ALOAD_3 -> Version.make_bounds "'ALOAD_3' instruction" Version.Java_1_0 None
  | ANEWARRAY _ -> Version.make_bounds "'ANEWARRAY' instruction" Version.Java_1_0 None
  | ARETURN -> Version.make_bounds "'ARETURN' instruction" Version.Java_1_0 None
  | ARRAYLENGTH -> Version.make_bounds "'ARRAYLENGTH' instruction" Version.Java_1_0 None
  | ASTORE _ -> Version.make_bounds "'ASTORE' instruction" Version.Java_1_0 None
  | ASTORE_0 -> Version.make_bounds "'ASTORE_0' instruction" Version.Java_1_0 None
  | ASTORE_1 -> Version.make_bounds "'ASTORE_1' instruction" Version.Java_1_0 None
  | ASTORE_2 -> Version.make_bounds "'ASTORE_2' instruction" Version.Java_1_0 None
  | ASTORE_3 -> Version.make_bounds "'ASTORE_3' instruction" Version.Java_1_0 None
  | ATHROW -> Version.make_bounds "'ATHROW' instruction" Version.Java_1_0 None
  | BALOAD -> Version.make_bounds "'BALOAD' instruction" Version.Java_1_0 None
  | BASTORE -> Version.make_bounds "'BASTORE' instruction" Version.Java_1_0 None
  | BIPUSH _ -> Version.make_bounds "'BIPUSH' instruction" Version.Java_1_0 None
  | CALOAD -> Version.make_bounds "'CALOAD' instruction" Version.Java_1_0 None
  | CASTORE -> Version.make_bounds "'CASTORE' instruction" Version.Java_1_0 None
  | CHECKCAST _ -> Version.make_bounds "'CHECKCAST' instruction" Version.Java_1_0 None
  | D2F -> Version.make_bounds "'D2F' instruction" Version.Java_1_0 None
  | D2I -> Version.make_bounds "'D2I' instruction" Version.Java_1_0 None
  | D2L -> Version.make_bounds "'D2L' instruction" Version.Java_1_0 None
  | DADD -> Version.make_bounds "'DADD' instruction" Version.Java_1_0 None
  | DALOAD -> Version.make_bounds "'DALOAD' instruction" Version.Java_1_0 None
  | DASTORE -> Version.make_bounds "'DASTORE' instruction" Version.Java_1_0 None
  | DCMPG -> Version.make_bounds "'DCMPG' instruction" Version.Java_1_0 None
  | DCMPL -> Version.make_bounds "'DCMPL' instruction" Version.Java_1_0 None
  | DCONST_0 -> Version.make_bounds "'DCONST_0' instruction" Version.Java_1_0 None
  | DCONST_1 -> Version.make_bounds "'DCONST_1' instruction" Version.Java_1_0 None
  | DDIV -> Version.make_bounds "'DDIV' instruction" Version.Java_1_0 None
  | DLOAD _ -> Version.make_bounds "'DLOAD' instruction" Version.Java_1_0 None
  | DLOAD_0 -> Version.make_bounds "'DLOAD_0' instruction" Version.Java_1_0 None
  | DLOAD_1 -> Version.make_bounds "'DLOAD_1' instruction" Version.Java_1_0 None
  | DLOAD_2 -> Version.make_bounds "'DLOAD_2' instruction" Version.Java_1_0 None
  | DLOAD_3 -> Version.make_bounds "'DLOAD_3' instruction" Version.Java_1_0 None
  | DMUL -> Version.make_bounds "'DMUL' instruction" Version.Java_1_0 None
  | DNEG -> Version.make_bounds "'DNEG' instruction" Version.Java_1_0 None
  | DREM -> Version.make_bounds "'DREM' instruction" Version.Java_1_0 None
  | DRETURN -> Version.make_bounds "'DRETURN' instruction" Version.Java_1_0 None
  | DSTORE _ -> Version.make_bounds "'DSTORE' instruction" Version.Java_1_0 None
  | DSTORE_0 -> Version.make_bounds "'DSTORE_0' instruction" Version.Java_1_0 None
  | DSTORE_1 -> Version.make_bounds "'DSTORE_1' instruction" Version.Java_1_0 None
  | DSTORE_2 -> Version.make_bounds "'DSTORE_2' instruction" Version.Java_1_0 None
  | DSTORE_3 -> Version.make_bounds "'DSTORE_3' instruction" Version.Java_1_0 None
  | DSUB -> Version.make_bounds "'DSUB' instruction" Version.Java_1_0 None
  | DUP -> Version.make_bounds "'DUP' instruction" Version.Java_1_0 None
  | DUP2 -> Version.make_bounds "'DUP2' instruction" Version.Java_1_0 None
  | DUP2_X1 -> Version.make_bounds "'DUP2_X1' instruction" Version.Java_1_0 None
  | DUP2_X2 -> Version.make_bounds "'DUP2_X2' instruction" Version.Java_1_0 None
  | DUP_X1 -> Version.make_bounds "'DUP_X1' instruction" Version.Java_1_0 None
  | DUP_X2 -> Version.make_bounds "'DUP_X2' instruction" Version.Java_1_0 None
  | F2D -> Version.make_bounds "'F2D' instruction" Version.Java_1_0 None
  | F2I -> Version.make_bounds "'F2I' instruction" Version.Java_1_0 None
  | F2L -> Version.make_bounds "'F2L' instruction" Version.Java_1_0 None
  | FADD -> Version.make_bounds "'FADD' instruction" Version.Java_1_0 None
  | FALOAD -> Version.make_bounds "'FALOAD' instruction" Version.Java_1_0 None
  | FASTORE -> Version.make_bounds "'FASTORE' instruction" Version.Java_1_0 None
  | FCMPG -> Version.make_bounds "'FCMPG' instruction" Version.Java_1_0 None
  | FCMPL -> Version.make_bounds "'FCMPL' instruction" Version.Java_1_0 None
  | FCONST_0 -> Version.make_bounds "'FCONST_0' instruction" Version.Java_1_0 None
  | FCONST_1 -> Version.make_bounds "'FCONST_1' instruction" Version.Java_1_0 None
  | FCONST_2 -> Version.make_bounds "'FCONST_2' instruction" Version.Java_1_0 None
  | FDIV -> Version.make_bounds "'FDIV' instruction" Version.Java_1_0 None
  | FLOAD _ -> Version.make_bounds "'FLOAD' instruction" Version.Java_1_0 None
  | FLOAD_0 -> Version.make_bounds "'FLOAD_0' instruction" Version.Java_1_0 None
  | FLOAD_1 -> Version.make_bounds "'FLOAD_1' instruction" Version.Java_1_0 None
  | FLOAD_2 -> Version.make_bounds "'FLOAD_2' instruction" Version.Java_1_0 None
  | FLOAD_3 -> Version.make_bounds "'FLOAD_3' instruction" Version.Java_1_0 None
  | FMUL -> Version.make_bounds "'FMUL' instruction" Version.Java_1_0 None
  | FNEG -> Version.make_bounds "'FNEG' instruction" Version.Java_1_0 None
  | FREM -> Version.make_bounds "'FREM' instruction" Version.Java_1_0 None
  | FRETURN -> Version.make_bounds "'FRETURN' instruction" Version.Java_1_0 None
  | FSTORE _ -> Version.make_bounds "'FSTORE' instruction" Version.Java_1_0 None
  | FSTORE_0 -> Version.make_bounds "'FSTORE_0' instruction" Version.Java_1_0 None
  | FSTORE_1 -> Version.make_bounds "'FSTORE_1' instruction" Version.Java_1_0 None
  | FSTORE_2 -> Version.make_bounds "'FSTORE_2' instruction" Version.Java_1_0 None
  | FSTORE_3 -> Version.make_bounds "'FSTORE_3' instruction" Version.Java_1_0 None
  | FSUB -> Version.make_bounds "'FSUB' instruction" Version.Java_1_0 None
  | GETFIELD _ -> Version.make_bounds "'GETFIELD' instruction" Version.Java_1_0 None
  | GETSTATIC _ -> Version.make_bounds "'GETSTATIC' instruction" Version.Java_1_0 None
  | GOTO _ -> Version.make_bounds "'GOTO' instruction" Version.Java_1_0 None
  | GOTO_W _ -> Version.make_bounds "'GOTO_W' instruction" Version.Java_1_0 None
  | I2B -> Version.make_bounds "'I2B' instruction" Version.Java_1_0 None
  | I2C -> Version.make_bounds "'I2C' instruction" Version.Java_1_0 None
  | I2D -> Version.make_bounds "'I2D' instruction" Version.Java_1_0 None
  | I2F -> Version.make_bounds "'I2F' instruction" Version.Java_1_0 None
  | I2L -> Version.make_bounds "'I2L' instruction" Version.Java_1_0 None
  | I2S -> Version.make_bounds "'I2S' instruction" Version.Java_1_0 None
  | IADD -> Version.make_bounds "'IADD' instruction" Version.Java_1_0 None
  | IALOAD -> Version.make_bounds "'IALOAD' instruction" Version.Java_1_0 None
  | IAND -> Version.make_bounds "'IAND' instruction" Version.Java_1_0 None
  | IASTORE -> Version.make_bounds "'IASTORE' instruction" Version.Java_1_0 None
  | ICONST_0 -> Version.make_bounds "'ICONST_0' instruction" Version.Java_1_0 None
  | ICONST_1 -> Version.make_bounds "'ICONST_1' instruction" Version.Java_1_0 None
  | ICONST_2 -> Version.make_bounds "'ICONST_2' instruction" Version.Java_1_0 None
  | ICONST_3 -> Version.make_bounds "'ICONST_3' instruction" Version.Java_1_0 None
  | ICONST_4 -> Version.make_bounds "'ICONST_4' instruction" Version.Java_1_0 None
  | ICONST_5 -> Version.make_bounds "'ICONST_5' instruction" Version.Java_1_0 None
  | ICONST_M1 -> Version.make_bounds "'ICONST_M1' instruction" Version.Java_1_0 None
  | IDIV -> Version.make_bounds "'IDIV' instruction" Version.Java_1_0 None
  | IF_ACMPEQ _ -> Version.make_bounds "'IF_ACMPEQ' instruction" Version.Java_1_0 None
  | IF_ACMPNE _ -> Version.make_bounds "'IF_ACMPNE' instruction" Version.Java_1_0 None
  | IF_ICMPEQ _ -> Version.make_bounds "'IF_ICMPEQ' instruction" Version.Java_1_0 None
  | IF_ICMPGE _ -> Version.make_bounds "'IF_ICMPGE' instruction" Version.Java_1_0 None
  | IF_ICMPGT _ -> Version.make_bounds "'IF_ICMPGT' instruction" Version.Java_1_0 None
  | IF_ICMPLE _ -> Version.make_bounds "'IF_ICMPLE' instruction" Version.Java_1_0 None
  | IF_ICMPLT _ -> Version.make_bounds "'IF_ICMPLT' instruction" Version.Java_1_0 None
  | IF_ICMPNE _ -> Version.make_bounds "'IF_ICMPNE' instruction" Version.Java_1_0 None
  | IFEQ _ -> Version.make_bounds "'IFEQ' instruction" Version.Java_1_0 None
  | IFGE _ -> Version.make_bounds "'IFGE' instruction" Version.Java_1_0 None
  | IFGT _ -> Version.make_bounds "'IFGT' instruction" Version.Java_1_0 None
  | IFLE _ -> Version.make_bounds "'IFLE' instruction" Version.Java_1_0 None
  | IFLT _ -> Version.make_bounds "'IFLT' instruction" Version.Java_1_0 None
  | IFNE _ -> Version.make_bounds "'IFNE' instruction" Version.Java_1_0 None
  | IFNONNULL _ -> Version.make_bounds "'IFNONNULL' instruction" Version.Java_1_0 None
  | IFNULL _ -> Version.make_bounds "'IFNULL' instruction" Version.Java_1_0 None
  | IINC _ -> Version.make_bounds "'IINC' instruction" Version.Java_1_0 None
  | ILOAD _ -> Version.make_bounds "'ILOAD' instruction" Version.Java_1_0 None
  | ILOAD_0 -> Version.make_bounds "'ILOAD_0' instruction" Version.Java_1_0 None
  | ILOAD_1 -> Version.make_bounds "'ILOAD_1' instruction" Version.Java_1_0 None
  | ILOAD_2 -> Version.make_bounds "'ILOAD_2' instruction" Version.Java_1_0 None
  | ILOAD_3 -> Version.make_bounds "'ILOAD_3' instruction" Version.Java_1_0 None
  | IMUL -> Version.make_bounds "'IMUL' instruction" Version.Java_1_0 None
  | INEG -> Version.make_bounds "'INEG' instruction" Version.Java_1_0 None
  | INSTANCEOF _ -> Version.make_bounds "'INSTANCEOF' instruction" Version.Java_1_0 None
  | INVOKEDYNAMIC _ -> Version.make_bounds "'INVOKEDYNAMIC' instruction" Version.Java_1_7 None
  | INVOKEINTERFACE _ -> Version.make_bounds "'INVOKEINTERFACE' instruction" Version.Java_1_0 None
  | INVOKESPECIAL _ -> Version.make_bounds "'INVOKESPECIAL' instruction" Version.Java_1_0 None
  | INVOKESTATIC _ -> Version.make_bounds "'INVOKESTATIC' instruction" Version.Java_1_0 None
  | INVOKEVIRTUAL _ -> Version.make_bounds "'INVOKEVIRTUAL' instruction" Version.Java_1_0 None
  | IOR -> Version.make_bounds "'IOR' instruction" Version.Java_1_0 None
  | IREM -> Version.make_bounds "'IREM' instruction" Version.Java_1_0 None
  | IRETURN -> Version.make_bounds "'IRETURN' instruction" Version.Java_1_0 None
  | ISHL -> Version.make_bounds "'ISHL' instruction" Version.Java_1_0 None
  | ISHR -> Version.make_bounds "'ISHR' instruction" Version.Java_1_0 None
  | ISTORE _ -> Version.make_bounds "'ISTORE' instruction" Version.Java_1_0 None
  | ISTORE_0 -> Version.make_bounds "'ISTORE_0' instruction" Version.Java_1_0 None
  | ISTORE_1 -> Version.make_bounds "'ISTORE_1' instruction" Version.Java_1_0 None
  | ISTORE_2 -> Version.make_bounds "'ISTORE_2' instruction" Version.Java_1_0 None
  | ISTORE_3 -> Version.make_bounds "'ISTORE_3' instruction" Version.Java_1_0 None
  | ISUB -> Version.make_bounds "'ISUB' instruction" Version.Java_1_0 None
  | IUSHR -> Version.make_bounds "'IUSHR' instruction" Version.Java_1_0 None
  | IXOR -> Version.make_bounds "'IXOR' instruction" Version.Java_1_0 None
  | JSR _ -> Version.make_bounds "'JSR' instruction" Version.Java_1_0 (Some Version.Java_1_5)
  | JSR_W _ -> Version.make_bounds "'JSR_W' instruction" Version.Java_1_0 (Some Version.Java_1_5)
  | L2D -> Version.make_bounds "'L2D' instruction" Version.Java_1_0 None
  | L2F -> Version.make_bounds "'L2F' instruction" Version.Java_1_0 None
  | L2I -> Version.make_bounds "'L2I' instruction" Version.Java_1_0 None
  | LADD -> Version.make_bounds "'LADD' instruction" Version.Java_1_0 None
  | LALOAD -> Version.make_bounds "'LALOAD' instruction" Version.Java_1_0 None
  | LAND -> Version.make_bounds "'LAND' instruction" Version.Java_1_0 None
  | LASTORE -> Version.make_bounds "'LASTORE' instruction" Version.Java_1_0 None
  | LCMP -> Version.make_bounds "'LCMP' instruction" Version.Java_1_0 None
  | LCONST_0 -> Version.make_bounds "'LCONST_0' instruction" Version.Java_1_0 None
  | LCONST_1 -> Version.make_bounds "'LCONST_1' instruction" Version.Java_1_0 None
  | LDC x -> Version.make_bounds "'LDC' instruction" (match x with `Class_or_interface _ | `Array_type _ -> Version.Java_1_5 | `Method_type _ -> Version.Java_1_7 | `Method_handle _ -> Version.Java_1_7 | _ -> Version.Java_1_0) None
  | LDC2_W _ -> Version.make_bounds "'LDC2_W' instruction" Version.Java_1_0 None
  | LDC_W x -> Version.make_bounds "'LDC_W' instruction" (match x with `Class_or_interface _ | `Array_type _ -> Version.Java_1_5 | `Method_type _ -> Version.Java_1_7 | `Method_handle _ -> Version.Java_1_7 | _ -> Version.Java_1_0) None
  | LDIV -> Version.make_bounds "'LDIV' instruction" Version.Java_1_0 None
  | LLOAD _ -> Version.make_bounds "'LLOAD' instruction" Version.Java_1_0 None
  | LLOAD_0 -> Version.make_bounds "'LLOAD_0' instruction" Version.Java_1_0 None
  | LLOAD_1 -> Version.make_bounds "'LLOAD_1' instruction" Version.Java_1_0 None
  | LLOAD_2 -> Version.make_bounds "'LLOAD_2' instruction" Version.Java_1_0 None
  | LLOAD_3 -> Version.make_bounds "'LLOAD_3' instruction" Version.Java_1_0 None
  | LMUL -> Version.make_bounds "'LMUL' instruction" Version.Java_1_0 None
  | LNEG -> Version.make_bounds "'LNEG' instruction" Version.Java_1_0 None
  | LOOKUPSWITCH _ -> Version.make_bounds "'LOOKUPSWITCH' instruction" Version.Java_1_0 None
  | LOR -> Version.make_bounds "'LOR' instruction" Version.Java_1_0 None
  | LREM -> Version.make_bounds "'LREM' instruction" Version.Java_1_0 None
  | LRETURN -> Version.make_bounds "'LRETURN' instruction" Version.Java_1_0 None
  | LSHL -> Version.make_bounds "'LSHL' instruction" Version.Java_1_0 None
  | LSHR -> Version.make_bounds "'LSHR' instruction" Version.Java_1_0 None
  | LSTORE _ -> Version.make_bounds "'LSTORE' instruction" Version.Java_1_0 None
  | LSTORE_0 -> Version.make_bounds "'LSTORE_0' instruction" Version.Java_1_0 None
  | LSTORE_1 -> Version.make_bounds "'LSTORE_1' instruction" Version.Java_1_0 None
  | LSTORE_2 -> Version.make_bounds "'LSTORE_2' instruction" Version.Java_1_0 None
  | LSTORE_3 -> Version.make_bounds "'LSTORE_3' instruction" Version.Java_1_0 None
  | LSUB -> Version.make_bounds "'LSUB' instruction" Version.Java_1_0 None
  | LUSHR -> Version.make_bounds "'LUSHR' instruction" Version.Java_1_0 None
  | LXOR -> Version.make_bounds "'LXOR' instruction" Version.Java_1_0 None
  | MONITORENTER -> Version.make_bounds "'MONITORENTER' instruction" Version.Java_1_0 None
  | MONITOREXIT -> Version.make_bounds "'MONITOREXIT' instruction" Version.Java_1_0 None
  | MULTIANEWARRAY _ -> Version.make_bounds "'MULTIANEWARRAY' instruction" Version.Java_1_0 None
  | NEW _ -> Version.make_bounds "'NEW' instruction" Version.Java_1_0 None
  | NEWARRAY _ -> Version.make_bounds "'NEWARRAY' instruction" Version.Java_1_0 None
  | NOP -> Version.make_bounds "'NOP' instruction" Version.Java_1_0 None
  | POP -> Version.make_bounds "'POP' instruction" Version.Java_1_0 None
  | POP2 -> Version.make_bounds "'POP2' instruction" Version.Java_1_0 None
  | PUTFIELD _ -> Version.make_bounds "'PUTFIELD' instruction" Version.Java_1_0 None
  | PUTSTATIC _ -> Version.make_bounds "'PUTSTATIC' instruction" Version.Java_1_0 None
  | RET _ -> Version.make_bounds "'RET' instruction" Version.Java_1_0 (Some Version.Java_1_5)
  | RETURN -> Version.make_bounds "'RETURN' instruction" Version.Java_1_0 None
  | SALOAD -> Version.make_bounds "'SALOAD' instruction" Version.Java_1_0 None
  | SASTORE -> Version.make_bounds "'SASTORE' instruction" Version.Java_1_0 None
  | SIPUSH _ -> Version.make_bounds "'SIPUSH' instruction" Version.Java_1_0 None
  | SWAP -> Version.make_bounds "'SWAP' instruction" Version.Java_1_0 None
  | TABLESWITCH _ -> Version.make_bounds "'TABLESWITCH' instruction" Version.Java_1_0 None
  | WIDE_ALOAD _ -> Version.make_bounds "'WIDE_ALOAD' instruction" Version.Java_1_0 None
  | WIDE_ASTORE _ -> Version.make_bounds "'WIDE_ASTORE' instruction" Version.Java_1_0 None
  | WIDE_DLOAD _ -> Version.make_bounds "'WIDE_DLOAD' instruction" Version.Java_1_0 None
  | WIDE_DSTORE _ -> Version.make_bounds "'WIDE_DSTORE' instruction" Version.Java_1_0 None
  | WIDE_FLOAD _ -> Version.make_bounds "'WIDE_FLOAD' instruction" Version.Java_1_0 None
  | WIDE_FSTORE _ -> Version.make_bounds "'WIDE_FSTORE' instruction" Version.Java_1_0 None
  | WIDE_IINC _ -> Version.make_bounds "'WIDE_IINC' instruction" Version.Java_1_0 None
  | WIDE_ILOAD _ -> Version.make_bounds "'WIDE_ILOAD' instruction" Version.Java_1_0 None
  | WIDE_ISTORE _ -> Version.make_bounds "'WIDE_ISTORE' instruction" Version.Java_1_0 None
  | WIDE_LLOAD _ -> Version.make_bounds "'WIDE_LLOAD' instruction" Version.Java_1_0 None
  | WIDE_LSTORE _ -> Version.make_bounds "'WIDE_LSTORE' instruction" Version.Java_1_0 None
  | WIDE_RET _ -> Version.make_bounds "'WIDE_RET' instruction" Version.Java_1_0 (Some Version.Java_1_5)
