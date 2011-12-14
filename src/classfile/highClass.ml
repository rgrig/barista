(* open modules *) (* {{{ *)
open Consts
(* }}} *)

(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module C = Consts
module CF = ClassFile
module U = Utils

(* }}} *)

(* errors *) (* {{{ *)
type error =
  | Invalid_attribute_name
  | Invalid_class_name
  | Invalid_code_attribute
  | Invalid_code_length
  | Invalid_constant_value
  | Invalid_descriptor
  | Invalid_exception_name
  | Invalid_method_name
  | Invalid_module

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

(* }}} *)
module Instruction = struct (* {{{ *)
  type label = int
type t =
  | AALOAD
  | AASTORE
  | ACONST_NULL
  | ALOAD of int
  | ANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | ARETURN
  | ARRAYLENGTH
  | ASTORE of int
  | ATHROW
  | BALOAD
  | BASTORE
  | BIPUSH of int
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
  | DLOAD of int
  | DMUL
  | DNEG
  | DREM
  | DRETURN
  | DSTORE of int
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
  | FLOAD of int
  | FMUL
  | FNEG
  | FREM
  | FRETURN
  | FSTORE of int
  | FSUB
  | GETFIELD of (Name.for_class * Name.for_field * Descriptor.for_field)
  | GETSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field)
  | GOTO of label
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
  | IF_ACMPEQ of label
  | IF_ACMPNE of label
  | IF_ICMPEQ of label
  | IF_ICMPGE of label
  | IF_ICMPGT of label
  | IF_ICMPLE of label
  | IF_ICMPLT of label
  | IF_ICMPNE of label
  | IFEQ of label
  | IFGE of label
  | IFGT of label
  | IFLE of label
  | IFLT of label
  | IFNE of label
  | IFNONNULL of label
  | IFNULL of label
  | IINC of int * int
  | ILOAD of int
  | IMUL
  | INEG
  | INSTANCEOF of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  | INVOKEDYNAMIC of (Bootstrap.method_specifier * Name.for_method * Descriptor.for_method)
  | INVOKEINTERFACE of (Name.for_class * Name.for_method * Descriptor.for_method) * U.u1
  | INVOKESPECIAL of (Name.for_class * Name.for_method * Descriptor.for_method)
  | INVOKESTATIC of (Name.for_class * Name.for_method * Descriptor.for_method)
  | INVOKEVIRTUAL of ([`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * Name.for_method * Descriptor.for_method)
  | IOR
  | IREM
  | IRETURN
  | ISHL
  | ISHR
  | ISTORE of int
  | ISUB
  | IUSHR
  | IXOR
  | JSR of label
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
  | LDC of [`Int of int32 | `Float of float | `String of U.UTF8.t | `Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type | `Method_type of Descriptor.for_method | `Method_handle of Bootstrap.method_handle]
  | LDC2_W of [`Long of int64 | `Double of float]
  | LDIV
  | LLOAD of int
  | LMUL
  | LNEG
  | LOOKUPSWITCH of label * U.s4 * ((U.s4 * label) list)
  | LOR
  | LREM
  | LRETURN
  | LSHL
  | LSHR
  | LSTORE of int
  | LSUB
  | LUSHR
  | LXOR
  | MONITORENTER
  | MONITOREXIT
  | MULTIANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * int
  | NEW of Name.for_class
  | NEWARRAY of Descriptor.java_type
  | NOP
  | POP
  | POP2
  | PUTFIELD of (Name.for_class * Name.for_field * Descriptor.for_field)
  | PUTSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field)
  | RET of int
  | RETURN
  | SALOAD
  | SASTORE
  | SIPUSH of int
  | SWAP
  | TABLESWITCH of label * U.s4 * U.s4 * (label list)

  module LabelHash = Hashtbl.Make (struct
    type t = label
    let equal = (=)
    let hash x = x
  end)
    
  let s1_to_int (s : U.s1) = (s :> int)
  let s2_to_int (s : U.s2) = (s :> int)

  let u1_to_int (u : U.u1) = (u :> int)
  let u2_to_int (u : U.u2) = (u :> int)

  let s2_to_lbl (s : U.s2) = (s :> label)
  (* TODO: Int32 is larger than int, so this may not be ok *)
  (* Check specification to see if label should perhaps be Int32 *)
  (* (since there is a GOTO_W of s4 in ByteCode.instruction) *)
  let s4_to_lbl (s : U.s4) = ((Int32.to_int (s :> Int32.t)) :> label)

  let decode cpool ofs_to_lbl =
    let s_ofs_to_lbl (s : U.s2) = ofs_to_lbl (Int32.of_int (s :> int)) in
    let l_ofs_to_lbl (s : U.s4) = ofs_to_lbl (s :> Int32.t) in
  let get_entry idx =
    try
      ConstantPool.get_entry cpool idx
    with _ -> fail Invalid_pool_index in
  let get_utf8 idx = match get_entry idx with
    | ConstantPool.UTF8 v -> v
    | _ -> fail Invalid_pool_entry in
  let get_class_or_array idx = match get_entry idx with
    | ConstantPool.UTF8 v ->
        if U.UChar.equal opening_square_bracket (U.UTF8.get v 0) then
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
      when U.UTF8.equal (get_utf8 i2) class_constructor ->
      (Name.make_for_class_from_internal (get_utf8 i1)),
      (fst (Descriptor.method_of_utf8 (get_utf8 i3)))
    | _ -> fail Invalid_pool_entry in
(* TODO *)
  let dummy_element =
    `getField (Name.make_for_class_from_external (U.UTF8.of_string "dummy_package.DummyClass"),
               Name.make_for_field (U.UTF8.of_string "dummyField"),
               `Boolean),
    [] in
  let bsi = ExtendableArray.make 0 128 dummy_element in
  let get_dynamic idx nat = match get_entry nat with
    | ConstantPool.NameAndType (idx2, idx3)
        when ((idx : U.u2 :> int) < ExtendableArray.length bsi) ->
      ExtendableArray.get bsi idx,
          (Name.make_for_method (get_utf8 idx2)),
          (Descriptor.method_of_utf8 (get_utf8 idx3))
    | _ -> fail Invalid_pool_entry in
  let get_array_method_ref cls nat = match (get_entry cls), (get_entry nat) with
    | (ConstantPool.Class i1), (ConstantPool.NameAndType (i2, i3)) ->
      let s = get_utf8 i1 in
      (if not (U.UChar.equal opening_square_bracket (U.UTF8.get s 0)) then
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
 function
  | ByteCode.AALOAD -> AALOAD
  | ByteCode.AASTORE -> AASTORE
  | ByteCode.ACONST_NULL -> ACONST_NULL
  | ByteCode.ALOAD p1 -> ALOAD (u1_to_int p1)
  | ByteCode.ALOAD_0 -> ALOAD 0
  | ByteCode.ALOAD_1 -> ALOAD 1
  | ByteCode.ALOAD_2 -> ALOAD 2
  | ByteCode.ALOAD_3 -> ALOAD 3
  | ByteCode.ANEWARRAY p1 -> ANEWARRAY (match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
  | ByteCode.ARETURN -> ARETURN
  | ByteCode.ARRAYLENGTH -> ARRAYLENGTH
  | ByteCode.ASTORE p1 -> ASTORE (u1_to_int p1)
  | ByteCode.ASTORE_0 -> ASTORE 0
  | ByteCode.ASTORE_1 -> ASTORE 1
  | ByteCode.ASTORE_2 -> ASTORE 2
  | ByteCode.ASTORE_3 -> ASTORE 3
  | ByteCode.ATHROW -> ATHROW
  | ByteCode.BALOAD -> BALOAD
  | ByteCode.BASTORE -> BASTORE
  | ByteCode.BIPUSH p1 -> BIPUSH (s1_to_int p1)
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
  | ByteCode.DLOAD p1 -> DLOAD (u1_to_int p1)
  | ByteCode.DLOAD_0 -> DLOAD 0
  | ByteCode.DLOAD_1 -> DLOAD 1
  | ByteCode.DLOAD_2 -> DLOAD 2
  | ByteCode.DLOAD_3 -> DLOAD 3
  | ByteCode.DMUL -> DMUL
  | ByteCode.DNEG -> DNEG
  | ByteCode.DREM -> DREM
  | ByteCode.DRETURN -> DRETURN
  | ByteCode.DSTORE p1 -> DSTORE (u1_to_int p1)
  | ByteCode.DSTORE_0 -> DSTORE 0
  | ByteCode.DSTORE_1 -> DSTORE 1
  | ByteCode.DSTORE_2 -> DSTORE 2
  | ByteCode.DSTORE_3 -> DSTORE 3
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
  | ByteCode.FLOAD p1 -> FLOAD (u1_to_int p1)
  | ByteCode.FLOAD_0 -> FLOAD 0
  | ByteCode.FLOAD_1 -> FLOAD 1
  | ByteCode.FLOAD_2 -> FLOAD 2
  | ByteCode.FLOAD_3 -> FLOAD 3
  | ByteCode.FMUL -> FMUL
  | ByteCode.FNEG -> FNEG
  | ByteCode.FREM -> FREM
  | ByteCode.FRETURN -> FRETURN
  | ByteCode.FSTORE p1 -> FSTORE (u1_to_int p1)
  | ByteCode.FSTORE_0 -> FSTORE 0
  | ByteCode.FSTORE_1 -> FSTORE 1
  | ByteCode.FSTORE_2 -> FSTORE 2
  | ByteCode.FSTORE_3 -> FSTORE 3
  | ByteCode.FSUB -> FSUB
  | ByteCode.GETFIELD p1 -> GETFIELD (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.GETSTATIC p1 -> GETSTATIC (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.GOTO p1 -> GOTO (s_ofs_to_lbl p1)
  | ByteCode.GOTO_W p1 -> GOTO (l_ofs_to_lbl p1)
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
  | ByteCode.IF_ACMPEQ p1 -> IF_ACMPEQ (s_ofs_to_lbl p1)
  | ByteCode.IF_ACMPNE p1 -> IF_ACMPNE (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPEQ p1 -> IF_ICMPEQ (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPGE p1 -> IF_ICMPGE (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPGT p1 -> IF_ICMPGT (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPLE p1 -> IF_ICMPLE (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPLT p1 -> IF_ICMPLT (s_ofs_to_lbl p1)
  | ByteCode.IF_ICMPNE p1 -> IF_ICMPNE (s_ofs_to_lbl p1)
  | ByteCode.IFEQ p1 -> IFEQ (s_ofs_to_lbl p1)
  | ByteCode.IFGE p1 -> IFGE (s_ofs_to_lbl p1)
  | ByteCode.IFGT p1 -> IFGT (s_ofs_to_lbl p1)
  | ByteCode.IFLE p1 -> IFLE (s_ofs_to_lbl p1)
  | ByteCode.IFLT p1 -> IFLT (s_ofs_to_lbl p1)
  | ByteCode.IFNE p1 -> IFNE (s_ofs_to_lbl p1)
  | ByteCode.IFNONNULL p1 -> IFNONNULL (s_ofs_to_lbl p1)
  | ByteCode.IFNULL p1 -> IFNULL (s_ofs_to_lbl p1)
  | ByteCode.IINC (p1, p2) -> IINC (u1_to_int p1, s1_to_int p2)
  | ByteCode.ILOAD p1 -> ILOAD (u1_to_int p1)
  | ByteCode.ILOAD_0 -> ILOAD 0
  | ByteCode.ILOAD_1 -> ILOAD 1
  | ByteCode.ILOAD_2 -> ILOAD 2
  | ByteCode.ILOAD_3 -> ILOAD 3
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
  | ByteCode.ISTORE p1 -> ISTORE (u1_to_int p1)
  | ByteCode.ISTORE_0 -> ISTORE 0
  | ByteCode.ISTORE_1 -> ISTORE 1
  | ByteCode.ISTORE_2 -> ISTORE 2
  | ByteCode.ISTORE_3 -> ISTORE 3
  | ByteCode.ISUB -> ISUB
  | ByteCode.IUSHR -> IUSHR
  | ByteCode.IXOR -> IXOR
  | ByteCode.JSR p1 -> JSR (s_ofs_to_lbl p1)
  | ByteCode.JSR_W p1 -> JSR (l_ofs_to_lbl p1)
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
  | ByteCode.LDC p1 -> LDC (match get_entry (U.u2_of_u1 p1) with | ConstantPool.Integer v -> `Int v | ConstantPool.Float v -> `Float (Int32.float_of_bits v) | ConstantPool.String idx -> `String (get_utf8 idx) | ConstantPool.Class idx -> get_class_or_array idx | ConstantPool.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (get_utf8 idx)) | ConstantPool.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
  | ByteCode.LDC2_W p1 -> LDC2_W (match get_entry p1 with | ConstantPool.Long (hi, lo) -> `Long (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo)) | ConstantPool.Double (hi, lo) -> `Double (Int64.float_of_bits (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo))) | _ -> fail Invalid_pool_element)
  | ByteCode.LDC_W p1 -> LDC (match get_entry p1 with | ConstantPool.Integer v -> `Int v | ConstantPool.Float v -> `Float (Int32.float_of_bits v) | ConstantPool.String idx -> `String (get_utf8 idx) | ConstantPool.Class idx -> get_class_or_array idx | ConstantPool.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (get_utf8 idx)) | ConstantPool.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
  | ByteCode.LDIV -> LDIV
  | ByteCode.LLOAD p1 -> LLOAD (u1_to_int p1)
  | ByteCode.LLOAD_0 -> LLOAD 0
  | ByteCode.LLOAD_1 -> LLOAD 1
  | ByteCode.LLOAD_2 -> LLOAD 2
  | ByteCode.LLOAD_3 -> LLOAD 3
  | ByteCode.LMUL -> LMUL
  | ByteCode.LNEG -> LNEG
  | ByteCode.LOOKUPSWITCH (p1, p2, p3) ->
    let keys, offsets = List.split p3 in
    let labels = List.map l_ofs_to_lbl offsets in
    LOOKUPSWITCH (s4_to_lbl p1, p2, List.combine keys labels)
  | ByteCode.LOR -> LOR
  | ByteCode.LREM -> LREM
  | ByteCode.LRETURN -> LRETURN
  | ByteCode.LSHL -> LSHL
  | ByteCode.LSHR -> LSHR
  | ByteCode.LSTORE p1 -> LSTORE (u1_to_int p1)
  | ByteCode.LSTORE_0 -> LSTORE 0
  | ByteCode.LSTORE_1 -> LSTORE 1
  | ByteCode.LSTORE_2 -> LSTORE 2
  | ByteCode.LSTORE_3 -> LSTORE 3
  | ByteCode.LSUB -> LSUB
  | ByteCode.LUSHR -> LUSHR
  | ByteCode.LXOR -> LXOR
  | ByteCode.MONITORENTER -> MONITORENTER
  | ByteCode.MONITOREXIT -> MONITOREXIT
  | ByteCode.MULTIANEWARRAY (p1, p2) -> MULTIANEWARRAY ((match get_entry p1 with | ConstantPool.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element), u1_to_int p2)
  | ByteCode.NEW p1 -> NEW (match get_entry p1 with | ConstantPool.Class idx -> (Name.make_for_class_from_internal (get_utf8 idx)) | _ -> fail Invalid_pool_element)
  | ByteCode.NEWARRAY p1 -> NEWARRAY (primitive_array_type_of_int (p1 :> int))
  | ByteCode.NOP -> NOP
  | ByteCode.POP -> POP
  | ByteCode.POP2 -> POP2
  | ByteCode.PUTFIELD p1 -> PUTFIELD (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.PUTSTATIC p1 -> PUTSTATIC (match get_entry p1 with | ConstantPool.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
  | ByteCode.RET p1 -> RET (u1_to_int p1)
  | ByteCode.RETURN -> RETURN
  | ByteCode.SALOAD -> SALOAD
  | ByteCode.SASTORE -> SASTORE
  | ByteCode.SIPUSH p1 -> SIPUSH (s2_to_int p1)
  | ByteCode.SWAP -> SWAP
  | ByteCode.TABLESWITCH (p1, p2, p3, p4) -> TABLESWITCH (l_ofs_to_lbl p1, p2, p3, List.map l_ofs_to_lbl p4)
  | ByteCode.WIDE_ALOAD p1 -> ALOAD (u2_to_int p1)
  | ByteCode.WIDE_ASTORE p1 -> ASTORE (u2_to_int p1)
  | ByteCode.WIDE_DLOAD p1 -> DLOAD (u2_to_int p1)
  | ByteCode.WIDE_DSTORE p1 -> DSTORE (u2_to_int p1)
  | ByteCode.WIDE_FLOAD p1 -> FLOAD (u2_to_int p1)
  | ByteCode.WIDE_FSTORE p1 -> FSTORE (u2_to_int p1)
  | ByteCode.WIDE_IINC (p1, p2) -> IINC (u2_to_int p1, s2_to_int p2)
  | ByteCode.WIDE_ILOAD p1 -> ILOAD (u2_to_int p1)
  | ByteCode.WIDE_ISTORE p1 -> ISTORE (u2_to_int p1)
  | ByteCode.WIDE_LLOAD p1 -> LLOAD (u2_to_int p1)
  | ByteCode.WIDE_LSTORE p1 -> LSTORE (u2_to_int p1)
  | ByteCode.WIDE_RET p1 -> RET (u2_to_int p1)
end (* }}} *)

module A = Attribute
module HighAttribute = struct (* {{{ *)
  open Consts

  (* high-level *)

  type constant_value =
    | Long_value of int64
    | Float_value of float
    | Double_value of float
    | Boolean_value of bool
    | Byte_value of int
    | Character_value of int
    | Short_value of int
    | Integer_value of int32
    | String_value of Utils.UTF8.t

  type inner_class_element = {
      inner_class : Name.for_class option;
      outer_class : Name.for_class option;
      inner_name : Utils.UTF8.t option;
      inner_flags : AccessFlag.for_inner_class list;
    }

  type enclosing_method_value = {
      innermost_class : Name.for_class;
      enclosing_method : (Name.for_method * Descriptor.for_method) option;
    }

  type code_attribute = [
    | `LineNumberTable of int Instruction.LabelHash.t
    | `Unknown of Utils.UTF8.t * string ]
(* TODO
    | `LocalVariableTable of unit (** types for local variables *)
    | `LocalVariableTypeTable of local_variable_type_table_element list (** signatures for local variables *)
    | `StackMapTable of stack_map_frame list
 *)

  type exception_table_element = {
      try_start : Instruction.label;
      try_end : Instruction.label;
      catch : Instruction.label;
      caught : Name.for_class option;
    }

  type code_value = {
      code : Instruction.t list;
      exception_table : exception_table_element list;
      attributes : code_attribute list;
    }

  type for_class =
    [ `InnerClasses of inner_class_element list
    | `EnclosingMethod of enclosing_method_value
    | `Synthetic (** auto-generated element *)
    | `ClassSignature of Signature.class_signature
    | `SourceFile of Utils.UTF8.t
    | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
    | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
    | `Unknown of Utils.UTF8.t * string ]

  type for_method =
    [ `Code of code_value
    | `Exceptions of Name.for_class list
    | `Synthetic (** auto-generated element *)
    | `MethodSignature of Signature.method_signature
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleParameterAnnotations of Annotation.t list list
    | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `AnnotationDefault of Annotation.element_value
    | `Unknown of Utils.UTF8.t * string ]

  type for_field =
    [ `ConstantValue of constant_value
    | `Synthetic
    | `Signature of [`Field of Signature.field_type_signature]
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `Unknown of Utils.UTF8.t * string ]

  type t = [ for_class | for_method | for_field ]

  (* helper functions *)

  let get_utf8 pool idx err =
    match ConstantPool.get_entry pool idx with
    | ConstantPool.UTF8 v -> v
    | _ -> fail err

  let get_class_name pool idx err =
    match ConstantPool.get_entry pool idx with
      | ConstantPool.Class idx ->
	let n = get_utf8 pool idx err in
	Name.make_for_class_from_internal n
      | _ -> fail err

  let read_annotations pool st =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_info st in
        Annotation.decode pool a)

  let read_extended_annotations pool st =
    InputStream.read_elements
      st
      (fun st ->
        let a = Annotation.read_extended_info st in
        Annotation.decode_extended pool a)

  let read_annotations_list pool st =
    let nb = InputStream.read_u1 st in
    let res = ref [] in
    for i = 1 to (nb :> int) do
      let local =
        InputStream.read_elements
          st
          (fun st ->
            let a = Annotation.read_info st in
            Annotation.decode pool a) in
      res := local :: !res
    done;
    List.rev !res

  let read_module_info pool st =
    let module_index = InputStream.read_u2 st in
    let name_index, version_index =
      match ConstantPool.get_entry pool module_index with
      | ConstantPool.ModuleId (n, v) -> n, v
      | _ -> fail Invalid_module in
    let name = get_utf8 pool name_index Invalid_module in
    let version = get_utf8 pool version_index Invalid_module in
    name, version

  let read_info st =
    let name = InputStream.read_u2 st in
    let len = InputStream.read_u4 st in
    if (len :> int64) > (Int64.of_int max_int) then
      raise (InputStream.Exception InputStream.Data_is_too_large)
    else
      let dat = InputStream.read_bytes st (Int64.to_int (len :> int64)) in
      { A.name_index = name;
	length = len;
	data = dat; }

  let check_code_attributes l =
    let map = function
      | (`LineNumberTable _ as x)
(*TODO| (`LocalVariableTable _ as x)
      | (`LocalVariableTypeTable _ as x)
      | (`StackMapTable _ as x) *)
      | (`Unknown _ as x) -> x
      | #t -> fail Invalid_code_attribute in
    List.map map l
      
  let decode_attr_constant_value _ _ pool _ st =
        let const_index = InputStream.read_u2 st in
        match ConstantPool.get_entry pool const_index with
        | ConstantPool.Long (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Long_value v)
        | ConstantPool.Float v ->
            `ConstantValue (Float_value (Int32.float_of_bits v))
        | ConstantPool.Double (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Double_value (Int64.float_of_bits v))
        | ConstantPool.Integer v ->
            `ConstantValue (Integer_value v)
        | ConstantPool.String idx ->
            `ConstantValue (String_value (get_utf8 pool idx Invalid_constant_value))
        | _ -> fail Invalid_constant_value

  let decode_attr_code decode element pool i st =
    (* read these anyway to get into the stream *)
    let (*mx_stack*) _ = InputStream.read_u2 st in
    let (*mx_locals*) _ = InputStream.read_u2 st in
    let code_len' = InputStream.read_u4 st in
    let code_len =
      if (code_len' :> int64) < 65536L then
        Int64.to_int (code_len' :> int64)
      else
        fail Invalid_code_length in
    let code_content = InputStream.read_bytes st code_len in
    let code_stream = InputStream.make_of_string code_content in
    let instr_codes = ByteCode.read code_stream 0 in
    let fold_size (l, ofs, lbl) inst =
      let s = ByteCode.size_of ofs inst in
      (inst, ofs, lbl) :: l, ofs + s, lbl + 1 in
    let instr_codes_annot, _, _ = List.fold_left fold_size ([], 0, 0) instr_codes in
    (* TODO: faster structure for this? *)
    let ofs_to_label ofs =
      let (_, _, lbl) = List.find (fun (_, o, _) -> o = ofs) instr_codes_annot in
      lbl in
    let u2_ofs_to_label ofs = ofs_to_label (ofs : Utils.u2 :> int) in
    (* TODO: make offsets Int32? *)
    let l_ofs_to_label ofs = ofs_to_label (Int32.to_int ofs) in
    let instrs = List.map (Instruction.decode pool l_ofs_to_label) instr_codes in
    let exceptions =
      InputStream.read_elements
        st
        (fun st ->
          let start_pc = InputStream.read_u2 st in
          let end_pc = InputStream.read_u2 st in
          let handler_pc = InputStream.read_u2 st in
          let catch_index = InputStream.read_u2 st in
          let catch_type =
            if (catch_index :> int) <> 0 then
              Some (get_class_name pool catch_index Invalid_exception_name)
            else
              None in
          { try_start = u2_ofs_to_label start_pc;
            try_end = u2_ofs_to_label end_pc;
            catch = u2_ofs_to_label handler_pc;
            caught = catch_type; }) in
    let attrs =
      InputStream.read_elements
        st
        (fun st ->
          let a = read_info st in
          decode element pool a) in
    `Code { code = instrs;
            exception_table = exceptions;
            attributes = check_code_attributes attrs; }
      (* TODO: should code attributes be checked here? *)

  let decode_attr_exceptions _ = failwith "todo"
  let decode_attr_inner_classes _ = failwith "todo"
  let decode_attr_enclosing_method _ = failwith "todo"
  let decode_attr_synthetic _ = failwith "todo"
  let decode_attr_signature _ = failwith "todo"
  let decode_attr_source_file _ = failwith "todo"
  let decode_attr_source_debug_extension _ = failwith "todo"
  let decode_attr_line_number_table _ = failwith "todo"
  let decode_attr_local_variable_table _ = failwith "todo"
  let decode_attr_local_variable_type_table _ = failwith "todo"
  let decode_attr_deprecated _ = failwith "todo"
  let decode_attr_runtime_visible_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_annotations _ = failwith "todo"
  let decode_attr_runtime_visible_parameter_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_parameter_annotations _ = failwith "todo"
  let decode_attr_runtime_visible_type_annotations _ = failwith "todo"
  let decode_attr_runtime_invisible_type_annotations _ = failwith "todo"
  let decode_attr_annotation_default _ = failwith "todo"
  let decode_attr_stack_map_table _ = failwith "todo"
  let decode_attr_bootstrap_methods _ = failwith "todo"
  let decode_attr_module _ = failwith "todo"
  let decode_attr_module_requires _ = failwith "todo"
  let decode_attr_module_permits _ = failwith "todo"
  let decode_attr_module_provides _ = failwith "todo"

  module UTF8Hashtbl = Hashtbl.Make (Utils.UTF8)

  let decoders :
      ((A.enclosing_element ->
	ConstantPool.t ->
	A.info -> for_class) ->
       A.enclosing_element ->
       ConstantPool.t ->
       A.info -> InputStream.t -> t) UTF8Hashtbl.t =
    let ds = [
      attr_constant_value, decode_attr_constant_value;
      attr_code, decode_attr_code;
      attr_exceptions, decode_attr_exceptions;
      attr_inner_classes, decode_attr_inner_classes;
      attr_enclosing_method, decode_attr_enclosing_method;
      attr_synthetic, decode_attr_synthetic;
      attr_signature, decode_attr_signature;
      attr_source_file, decode_attr_source_file;
      attr_source_debug_extension, decode_attr_source_debug_extension;
      attr_line_number_table, decode_attr_line_number_table;
      attr_local_variable_table, decode_attr_local_variable_table;
      attr_local_variable_type_table, decode_attr_local_variable_type_table;
      attr_deprecated, decode_attr_deprecated;
      attr_runtime_visible_annotations, decode_attr_runtime_visible_annotations;
      attr_runtime_invisible_annotations, decode_attr_runtime_invisible_annotations;
      attr_runtime_visible_parameter_annotations, decode_attr_runtime_visible_parameter_annotations;
      attr_runtime_invisible_parameter_annotations, decode_attr_runtime_invisible_parameter_annotations;
      attr_runtime_visible_type_annotations, decode_attr_runtime_visible_type_annotations;
      attr_runtime_invisible_type_annotations, decode_attr_runtime_invisible_type_annotations;
      attr_annotation_default, decode_attr_annotation_default;
      attr_stack_map_table, decode_attr_stack_map_table;
      attr_bootstrap_methods, decode_attr_bootstrap_methods;
      attr_module, decode_attr_module;
      attr_module_requires, decode_attr_module_requires;
      attr_module_permits, decode_attr_module_permits;
      attr_module_provides, decode_attr_module_provides
    ] in
    let r = UTF8Hashtbl.create 61 in
    List.iter (fun (k, v) -> UTF8Hashtbl.add r k v) ds;
    r

  (* visible functions *)

  let for_class _ = failwith "todo"

  let decode_method _ _  = failwith "todo"

  let rec decode element pool i =
    let st = InputStream.make_of_string i.A.data in
    let attr_name = get_utf8 pool i.A.name_index Invalid_attribute_name in
    try
      for_class (UTF8Hashtbl.find decoders attr_name decode element pool i st)
    with Not_found ->
      `Unknown (attr_name, i.A.data)
end (* }}} *)

module M = Method
module HighMethod = struct (* {{{ *)
  type regular = {
      flags : AccessFlag.for_method list;
      name : Name.for_method;
      descriptor : Descriptor.for_method;
      attributes : HighAttribute.for_method list;
    }

  type constructor = {
      cstr_flags : AccessFlag.for_constructor list;
      cstr_descriptor : Descriptor.for_parameter list;
      cstr_attributes : HighAttribute.for_method list;
    }

  type class_initializer = {
      init_flags : AccessFlag.for_initializer list;
      init_attributes : HighAttribute.for_method list;
    }

  type t =
    | Regular of regular
    | Constructor of constructor
    | Initializer of class_initializer

  let decode is_interface pool m =
    let name = match ConstantPool.get_entry pool m.M.name_index with
    | ConstantPool.UTF8 n ->
        if Name.is_valid_for_method n then
          n
        else
          fail Invalid_method_name
    | _ -> fail Invalid_method_name in
    let descriptor = match ConstantPool.get_entry pool m.M.descriptor_index with
     | ConstantPool.UTF8 d -> Descriptor.method_of_utf8 d
     | _ -> fail Invalid_descriptor in
     let attributes =
         U.map_array_to_list
            (HighAttribute.decode_method Attribute.Method pool)
            m.M.attributes_array in
     U.switch
       U.UTF8.equal
       [ C.class_initializer,
         (fun _ ->
           let flags =
             AccessFlag.check_initializer_flags (AccessFlag.from_u2 true m.M.access_flags) in
           Initializer { init_flags = flags; init_attributes = attributes });

         C.class_constructor,
         (fun _ ->
           let flags =
             AccessFlag.check_constructor_flags (AccessFlag.from_u2 true m.M.access_flags) in
           Constructor { cstr_flags = flags; cstr_descriptor = fst descriptor; cstr_attributes = attributes }) ]
       (fun _ ->
         let flags =
           AccessFlag.check_method_flags is_interface
            (AccessFlag.from_u2 true m.M.access_flags) in
         let name = Name.make_for_method name in
         Regular { flags; name; descriptor; attributes })
       name
end (* }}} *)

module HA = HighAttribute
module HM = HighMethod

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : HM.t list;
    attributes : HA.for_class list;
  }

let check_version_high ?(version = Version.default) c =
  ignore version;
  if true then failwith "todo";
  c

let decode ?(version = Version.default) cf =
  let pool = cf.CF.constant_pool in
  let check_version v =
    let v' = cf.CF.major_version, cf.CF.minor_version in
    let v' = Version.version_of_major_minor v' in
    Version.at_most "class file version" v v';
    (* TODO: The following line should be [ClassFile.check ...]. *)
    ConstantPool.check_version v' pool in
  let get_class_name idx = match ConstantPool.get_entry pool idx with
    | ConstantPool.Class idx' ->
        (match ConstantPool.get_entry pool idx' with
          | ConstantPool.UTF8 n ->
              Name.make_for_class_from_internal n
          | _ -> fail Invalid_class_name)
    | _ -> fail Invalid_class_name in
  check_version version;
  let flags = AF.check_class_flags (AF.from_u2 false cf.CF.access_flags) in
  let extends =
    if cf.CF.super_class = U.u2 0
    then None
    else Some (get_class_name cf.ClassFile.super_class) in
  let is_interface = List.mem `Interface flags in
  let field_decode = Field.decode is_interface pool in
  let method_decode = HM.decode is_interface pool in
  let attribute_decode = HA.decode A.Class pool in
  check_version_high ~version:version { access_flags = flags;
    name = get_class_name cf.CF.this_class;
    extends = extends;
    implements = List.map get_class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }

let encode ?(version = Version.default) _ = ignore version; failwith "todo"

