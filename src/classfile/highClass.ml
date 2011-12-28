(* open modules *) (* {{{ *)
open Consts
(* }}} *)
(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module CF = ClassFile
module CP = ConstantPool
module IS = InputStream
module U = Utils

(* }}} *)
(* errors *) (* {{{ *)
type error =
  | Invalid_attribute
  | Invalid_code_length
  | Invalid_constant_value
  | Invalid_enclosing_method
  | Invalid_method_handle
  | Invalid_module
  | Invalid_name
  | Invalid_pool_element
  | Invalid_pool_entry
  | Invalid_primitive_array_type
  | Misplaced_attribute of (string * string)
  | Too_many of string
  | Unsupported_instruction of string

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unsupported_instruction s -> "unsupported instruction: " ^ s
  | Misplaced_attribute (a, e) -> "attribute " ^ a ^ " appears on " ^ e
  | Too_many s -> "number of " ^ s ^ " exceeds " ^ (string_of_int (U.max_u2 :> int))
  | _ -> "undescribed error (todo)"

(* }}} *)
(* HighInstruction, HighAttribute, and HighMethod *) (* {{{ *)
module HighInstruction = struct (* {{{ *)
  type label = int

  type iinc = { ii_var: int; ii_inc: int }
  type lookupswitch = { ls_def: label; ls_branches: (int * label) list }
  type tableswitch = { ts_lbl: label; ts_low: int; ts_high: int; ts_ofss: label list }

  type instruction =
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
    | IINC of iinc
    | ILOAD of int
    | IMUL
    | INEG
    | INSTANCEOF of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
  (*  | INVOKEDYNAMIC of (Bootstrap.method_specifier * Name.for_method * Descriptor.for_method) *)
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
    | LDC of [ `Int of int32
	     | `Float of float
	     | `String of U.UTF8.t
	     | `Class_or_interface of Name.for_class
	     | `Array_type of Descriptor.array_type
	     | `Method_type of Descriptor.for_method
	     | `Method_handle of Bootstrap.method_handle ]
    | LDC2_W of [ `Long of int64 | `Double of float ]
    | LDIV
    | LLOAD of int
    | LMUL
    | LNEG
    | LOOKUPSWITCH of lookupswitch
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
    | TABLESWITCH of tableswitch
	
  type t = label * instruction
      
  module LabelHash = Hashtbl.Make (struct
    type t = label
    let equal = (=)
    let hash x = x
  end)

  (* TODO(rgrig): Why is [s1_to_int x] any better than [(x : U.s1 :> int)] ? *)
  let s1_to_int (s : U.s1) = (s :> int)
  let s2_to_int (s : U.s2) = (s :> int)
  let s4_to_int (s : U.s4) = ((Int32.to_int (s :> Int32.t)) :> int)

  let u1_to_int (u : U.u1) = (u :> int)
  let u2_to_int (u : U.u2) = (u :> int)

  let decode pool ofs_to_lbl ofs =
    let abs_s_ofs_to_lbl (s : U.s2) = ofs_to_lbl (s :> int) in
    let abs_l_ofs_to_lbl (s : U.s4) = ofs_to_lbl (Int32.to_int (s :> Int32.t)) in
    let rel_l_ofs_to_lbl (s : U.s4) = ofs_to_lbl (ofs + Int32.to_int (s :> Int32.t)) in
  let entry = CP.get_entry pool in
  let utf8 = CP.get_utf8_entry pool in
  let get_class_or_array i =
    let s = utf8 i in
    if U.UChar.equal opening_square_bracket (U.UTF8.get s 0) then
      let t = Descriptor.java_type_of_internal_utf8 s in
      `Array_type (Descriptor.filter_non_array Descriptor.Invalid_array_element_type t)
    else
      `Class_or_interface (Name.make_for_class_from_internal s) in
  let get_field_ref cls nat = match entry cls, entry nat with
    | CP.Class i1, CP.NameAndType (i2, i3) ->
        (Name.make_for_class_from_internal (utf8 i1),
         Name.make_for_field (utf8 i2),
         Descriptor.field_of_utf8 (utf8 i3))
    | _ -> fail Invalid_pool_entry in
  let get_method_ref cls nat = match entry cls, entry nat with
    | CP.Class i1, CP.NameAndType (i2, i3) ->
        (Name.make_for_class_from_internal (utf8 i1),
          Name.make_for_method (utf8 i2),
          Descriptor.method_of_utf8 (utf8 i3))
    | _ -> fail Invalid_pool_entry in
  let get_special_ref cls nat = match entry cls, entry nat with
    | CP.Class i1, CP.NameAndType (i2, i3)
      when U.UTF8.equal (utf8 i2) class_constructor ->
        (Name.make_for_class_from_internal (utf8 i1),
          fst (Descriptor.method_of_utf8 (utf8 i3)))
    | _ -> fail Invalid_pool_entry in
  let get_array_method_ref cls nat = match entry cls, entry nat with
    | CP.Class i1, CP.NameAndType (i2, i3) ->
        (get_class_or_array i1,
          Name.make_for_method (utf8 i2),
          Descriptor.method_of_utf8 (utf8 i3))
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
    match kind, entry idx with
    | CP.REF_getField, CP.Fieldref (fc, nt) ->
      `getField (get_field_ref fc nt)
    | CP.REF_getStatic, CP.Fieldref (fc, nt) ->
      `getStatic (get_field_ref fc nt)
    | CP.REF_putField, CP.Fieldref (fc, nt) ->
      `putField (get_field_ref fc nt)
    | CP.REF_putStatic, CP.Fieldref (fc, nt) ->
      `putStatic (get_field_ref fc nt)
    | CP.REF_invokeVirtual, CP.Methodref (mc, mt) ->
      `invokeVirtual (get_method_ref mc mt)
    | CP.REF_invokeStatic, CP.Methodref (mc, mt) ->
      `invokeStatic (get_method_ref mc mt)
    | CP.REF_invokeSpecial, CP.Methodref (mc, mt) ->
      `invokeSpecial (get_method_ref mc mt)
    | CP.REF_newInvokeSpecial, CP.Methodref (mc, mt) ->
      `newInvokeSpecial (get_special_ref mc mt)
    | CP.REF_invokeInterface, CP.Methodref (mc, mt) ->
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
    | ByteCode.ANEWARRAY p1 -> ANEWARRAY (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
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
    | ByteCode.CHECKCAST p1 -> CHECKCAST (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
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
    | ByteCode.GETFIELD p1 -> GETFIELD (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.GETSTATIC p1 -> GETSTATIC (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.GOTO p1 -> GOTO (abs_s_ofs_to_lbl p1)
    | ByteCode.GOTO_W p1 -> GOTO (abs_l_ofs_to_lbl p1)
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
    | ByteCode.IF_ACMPEQ p1 -> IF_ACMPEQ (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ACMPNE p1 -> IF_ACMPNE (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPEQ p1 -> IF_ICMPEQ (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPGE p1 -> IF_ICMPGE (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPGT p1 -> IF_ICMPGT (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPLE p1 -> IF_ICMPLE (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPLT p1 -> IF_ICMPLT (abs_s_ofs_to_lbl p1)
    | ByteCode.IF_ICMPNE p1 -> IF_ICMPNE (abs_s_ofs_to_lbl p1)
    | ByteCode.IFEQ p1 -> IFEQ (abs_s_ofs_to_lbl p1)
    | ByteCode.IFGE p1 -> IFGE (abs_s_ofs_to_lbl p1)
    | ByteCode.IFGT p1 -> IFGT (abs_s_ofs_to_lbl p1)
    | ByteCode.IFLE p1 -> IFLE (abs_s_ofs_to_lbl p1)
    | ByteCode.IFLT p1 -> IFLT (abs_s_ofs_to_lbl p1)
    | ByteCode.IFNE p1 -> IFNE (abs_s_ofs_to_lbl p1)
    | ByteCode.IFNONNULL p1 -> IFNONNULL (abs_s_ofs_to_lbl p1)
    | ByteCode.IFNULL p1 -> IFNULL (abs_s_ofs_to_lbl p1)
    | ByteCode.IINC (p1, p2) -> IINC { ii_var = u1_to_int p1; ii_inc = s1_to_int p2 }
    | ByteCode.ILOAD p1 -> ILOAD (u1_to_int p1)
    | ByteCode.ILOAD_0 -> ILOAD 0
    | ByteCode.ILOAD_1 -> ILOAD 1
    | ByteCode.ILOAD_2 -> ILOAD 2
    | ByteCode.ILOAD_3 -> ILOAD 3
    | ByteCode.IMUL -> IMUL
    | ByteCode.INEG -> INEG
    | ByteCode.INSTANCEOF p1 -> INSTANCEOF (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
    | ByteCode.INVOKEDYNAMIC p1 -> fail (Unsupported_instruction "INVOKEDYNAMIC")
    | ByteCode.INVOKEINTERFACE (p1, p2) -> INVOKEINTERFACE ((match entry p1 with | CP.InterfaceMethodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element), p2)
    | ByteCode.INVOKESPECIAL p1 -> INVOKESPECIAL (match entry p1 with | CP.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.INVOKESTATIC p1 -> INVOKESTATIC (match entry p1 with | CP.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.INVOKEVIRTUAL p1 -> INVOKEVIRTUAL (match entry p1 with | CP.Methodref (cls, nat) -> (get_array_method_ref cls nat) | _ -> fail Invalid_pool_element)
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
    | ByteCode.JSR p1 -> JSR (abs_s_ofs_to_lbl p1)
    | ByteCode.JSR_W p1 -> JSR (abs_l_ofs_to_lbl p1)
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
    | ByteCode.LDC p1 -> LDC (match entry (U.u2_of_u1 p1) with | CP.Integer v -> `Int v | CP.Float v -> `Float (Int32.float_of_bits v) | CP.String idx -> `String (utf8 idx) | CP.Class idx -> get_class_or_array idx | CP.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (utf8 idx)) | CP.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
    | ByteCode.LDC2_W p1 -> LDC2_W (match entry p1 with | CP.Long (hi, lo) -> `Long (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo)) | CP.Double (hi, lo) -> `Double (Int64.float_of_bits (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo))) | _ -> fail Invalid_pool_element)
    | ByteCode.LDC_W p1 -> LDC (match entry p1 with | CP.Integer v -> `Int v | CP.Float v -> `Float (Int32.float_of_bits v) | CP.String idx -> `String (utf8 idx) | CP.Class idx -> get_class_or_array idx | CP.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (utf8 idx)) | CP.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
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
      let labels = List.map rel_l_ofs_to_lbl offsets in
      let items = List.map s4_to_int keys in
    (* bytecode parser should ensure this *)
      assert (s4_to_int p2 = List.length p3);
      LOOKUPSWITCH { ls_def = rel_l_ofs_to_lbl p1
		   ; ls_branches = List.combine items labels }
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
    | ByteCode.MULTIANEWARRAY (p1, p2) -> MULTIANEWARRAY ((match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element), u1_to_int p2)
    | ByteCode.NEW p1 -> NEW (match entry p1 with | CP.Class idx -> (Name.make_for_class_from_internal (utf8 idx)) | _ -> fail Invalid_pool_element)
    | ByteCode.NEWARRAY p1 -> NEWARRAY (primitive_array_type_of_int (p1 :> int))
    | ByteCode.NOP -> NOP
    | ByteCode.POP -> POP
    | ByteCode.POP2 -> POP2
    | ByteCode.PUTFIELD p1 -> PUTFIELD (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.PUTSTATIC p1 -> PUTSTATIC (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
    | ByteCode.RET p1 -> RET (u1_to_int p1)
    | ByteCode.RETURN -> RETURN
    | ByteCode.SALOAD -> SALOAD
    | ByteCode.SASTORE -> SASTORE
    | ByteCode.SIPUSH p1 -> SIPUSH (s2_to_int p1)
    | ByteCode.SWAP -> SWAP
    | ByteCode.TABLESWITCH (p1, p2, p3, p4) ->
      TABLESWITCH {
	ts_lbl = abs_l_ofs_to_lbl p1;
	ts_low = s4_to_int p2;
	ts_high = s4_to_int p3;
	ts_ofss = List.map abs_l_ofs_to_lbl p4 }
    | ByteCode.WIDE_ALOAD p1 -> ALOAD (u2_to_int p1)
    | ByteCode.WIDE_ASTORE p1 -> ASTORE (u2_to_int p1)
    | ByteCode.WIDE_DLOAD p1 -> DLOAD (u2_to_int p1)
    | ByteCode.WIDE_DSTORE p1 -> DSTORE (u2_to_int p1)
    | ByteCode.WIDE_FLOAD p1 -> FLOAD (u2_to_int p1)
    | ByteCode.WIDE_FSTORE p1 -> FSTORE (u2_to_int p1)
    | ByteCode.WIDE_IINC (p1, p2) -> IINC { ii_var = u2_to_int p1
					  ; ii_inc = s2_to_int p2 }
    | ByteCode.WIDE_ILOAD p1 -> ILOAD (u2_to_int p1)
    | ByteCode.WIDE_ISTORE p1 -> ISTORE (u2_to_int p1)
    | ByteCode.WIDE_LLOAD p1 -> LLOAD (u2_to_int p1)
    | ByteCode.WIDE_LSTORE p1 -> LSTORE (u2_to_int p1)
    | ByteCode.WIDE_RET p1 -> RET (u2_to_int p1)
      
  let version_bounds (_, inst) = match inst with
    | AALOAD -> Version.make_bounds "'AALOAD' instruction" Version.Java_1_0 None
    | AASTORE -> Version.make_bounds "'AASTORE' instruction" Version.Java_1_0 None
    | ACONST_NULL -> Version.make_bounds "'ACONST_NULL' instruction" Version.Java_1_0 None
    | ALOAD _ -> Version.make_bounds "'ALOAD' instruction" Version.Java_1_0 None
    | ANEWARRAY _ -> Version.make_bounds "'ANEWARRAY' instruction" Version.Java_1_0 None
    | ARETURN -> Version.make_bounds "'ARETURN' instruction" Version.Java_1_0 None
    | ARRAYLENGTH -> Version.make_bounds "'ARRAYLENGTH' instruction" Version.Java_1_0 None
    | ASTORE _ -> Version.make_bounds "'ASTORE' instruction" Version.Java_1_0 None
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
    | DMUL -> Version.make_bounds "'DMUL' instruction" Version.Java_1_0 None
    | DNEG -> Version.make_bounds "'DNEG' instruction" Version.Java_1_0 None
    | DREM -> Version.make_bounds "'DREM' instruction" Version.Java_1_0 None
    | DRETURN -> Version.make_bounds "'DRETURN' instruction" Version.Java_1_0 None
    | DSTORE _ -> Version.make_bounds "'DSTORE' instruction" Version.Java_1_0 None
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
    | FMUL -> Version.make_bounds "'FMUL' instruction" Version.Java_1_0 None
    | FNEG -> Version.make_bounds "'FNEG' instruction" Version.Java_1_0 None
    | FREM -> Version.make_bounds "'FREM' instruction" Version.Java_1_0 None
    | FRETURN -> Version.make_bounds "'FRETURN' instruction" Version.Java_1_0 None
    | FSTORE _ -> Version.make_bounds "'FSTORE' instruction" Version.Java_1_0 None
    | FSUB -> Version.make_bounds "'FSUB' instruction" Version.Java_1_0 None
    | GETFIELD _ -> Version.make_bounds "'GETFIELD' instruction" Version.Java_1_0 None
    | GETSTATIC _ -> Version.make_bounds "'GETSTATIC' instruction" Version.Java_1_0 None
    | GOTO _ -> Version.make_bounds "'GOTO' instruction" Version.Java_1_0 None
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
    | IMUL -> Version.make_bounds "'IMUL' instruction" Version.Java_1_0 None
    | INEG -> Version.make_bounds "'INEG' instruction" Version.Java_1_0 None
    | INSTANCEOF _ -> Version.make_bounds "'INSTANCEOF' instruction" Version.Java_1_0 None
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
    | ISUB -> Version.make_bounds "'ISUB' instruction" Version.Java_1_0 None
    | IUSHR -> Version.make_bounds "'IUSHR' instruction" Version.Java_1_0 None
    | IXOR -> Version.make_bounds "'IXOR' instruction" Version.Java_1_0 None
    | JSR _ -> Version.make_bounds "'JSR' instruction" Version.Java_1_0 (Some Version.Java_1_5)
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
    | LDIV -> Version.make_bounds "'LDIV' instruction" Version.Java_1_0 None
    | LLOAD _ -> Version.make_bounds "'LLOAD' instruction" Version.Java_1_0 None
    | LMUL -> Version.make_bounds "'LMUL' instruction" Version.Java_1_0 None
    | LNEG -> Version.make_bounds "'LNEG' instruction" Version.Java_1_0 None
    | LOOKUPSWITCH _ -> Version.make_bounds "'LOOKUPSWITCH' instruction" Version.Java_1_0 None
    | LOR -> Version.make_bounds "'LOR' instruction" Version.Java_1_0 None
    | LREM -> Version.make_bounds "'LREM' instruction" Version.Java_1_0 None
    | LRETURN -> Version.make_bounds "'LRETURN' instruction" Version.Java_1_0 None
    | LSHL -> Version.make_bounds "'LSHL' instruction" Version.Java_1_0 None
    | LSHR -> Version.make_bounds "'LSHR' instruction" Version.Java_1_0 None
    | LSTORE _ -> Version.make_bounds "'LSTORE' instruction" Version.Java_1_0 None
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
end (* }}} *)
module HI = HighInstruction

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
    | `LineNumberTable of int HI.LabelHash.t
    | `Unknown of Utils.UTF8.t * string

    (* TODO: Treating these properly requires some symbolic execution. *)
    | `LocalVariableTable of unit (** types for local variables *)
    | `LocalVariableTypeTable of unit (** signatures for local variables *)
  ]

  type exception_table_element = {
      try_start : HI.label;
      try_end : HI.label;
      catch : HI.label;
      caught : Name.for_class option;
    }

  type code_value = {
      code : HI.t list;
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
    | `FieldSignature of Signature.field_type_signature
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `Unknown of Utils.UTF8.t * string ]

  type t = [ for_class | for_method | for_field | code_attribute ]

  let name_of_attribute (a : t) = U.UTF8.to_string (match a with
    | `AnnotationDefault _ -> attr_annotation_default
    | `BootstrapMethods _ -> attr_bootstrap_methods
    | `ClassSignature _ -> attr_signature
    | `Code _ -> attr_code
    | `ConstantValue _ -> attr_constant_value
    | `Deprecated -> attr_deprecated
    | `EnclosingMethod _ -> attr_enclosing_method
    | `Exceptions _ -> attr_exceptions
    | `FieldSignature _ -> attr_signature
    | `InnerClasses _ -> attr_inner_classes
    | `LineNumberTable _ -> attr_line_number_table
    | `LocalVariableTable _ -> attr_local_variable_table
    | `LocalVariableTypeTable _ -> attr_local_variable_type_table
    | `MethodSignature _ -> attr_signature
    | `Module _ -> attr_module
    | `RuntimeInvisibleAnnotations _ -> attr_runtime_invisible_annotations
    | `RuntimeInvisibleParameterAnnotations _ -> attr_runtime_invisible_parameter_annotations
    | `RuntimeInvisibleTypeAnnotations _ -> attr_runtime_invisible_type_annotations
    | `RuntimeVisibleAnnotations _ -> attr_runtime_visible_annotations
    | `RuntimeVisibleParameterAnnotations _ -> attr_runtime_visible_parameter_annotations
    | `RuntimeVisibleTypeAnnotations _ -> attr_runtime_visible_type_annotations
    | `SourceDebugExtension _ -> attr_source_debug_extension
    | `SourceFile _ -> attr_source_file
    | `Synthetic -> attr_synthetic
    | `Unknown _ -> attr_unknown)

  (* helper functions *)

  let hash_of_list create add xs =
    let h = create 61 in
    List.iter (fun (k, v) -> add h k v) xs;
    h

  let read_annotations pool st =
    IS.read_elements
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
    let nb = IS.read_u1 st in
    let res = ref [] in
    for i = 1 to (nb :> int) do
      let local =
        IS.read_elements
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
      match CP.get_entry pool module_index with
      | CP.ModuleId (n, v) -> n, v
      | _ -> fail Invalid_module in
    let name = CP.get_utf8_entry pool name_index in
    let version = CP.get_utf8_entry pool version_index in
    (name, version)

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

  let check_code_attribute = function
    | #code_attribute as g -> g
    | b -> fail (Misplaced_attribute (name_of_attribute b, "code"))

  let option_of_u2 f (i : U.u2) =
    if (i :> int) = 0 then None else Some (f i)

  (* actual decoders for attributes *)

  type decoding_arguments =
    { da_element : A.enclosing_element
    ; da_pool : CP.t
    ; da_i : A.info }

  let decode_attr_constant_value _ r st =
        let const_index = InputStream.read_u2 st in
        match CP.get_entry r.da_pool const_index with
        | CP.Long (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Long_value v)
        | CP.Float v ->
            `ConstantValue (Float_value (Int32.float_of_bits v))
        | CP.Double (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (Double_value (Int64.float_of_bits v))
        | CP.Integer v ->
            `ConstantValue (Integer_value v)
        | CP.String idx ->
            `ConstantValue (String_value (CP.get_utf8_entry r.da_pool idx))
        | _ -> fail Invalid_constant_value

  let decode_attr_code decode r st =
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
    let decode_inst_code (inst, ofs, lbl) = lbl, HI.decode r.da_pool ofs_to_label ofs inst in
    let instrs = List.map decode_inst_code instr_codes_annot in
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
              Some (CP.get_class_name r.da_pool catch_index)
            else
              None in
	  let u2_ofs_to_label ofs = ofs_to_label (ofs : Utils.u2 :> int) in
          { try_start = u2_ofs_to_label start_pc;
            try_end = u2_ofs_to_label end_pc;
            catch = u2_ofs_to_label handler_pc;
            caught = catch_type; }) in
    let attrs = IS.read_elements
        st
        (fun st -> decode { r with da_i = read_info st }) in
    let update_lnt = function
      | `LineNumberTable h ->
          let r = HI.LabelHash.create 13 in
          let f ofs ln =
            let label = ofs_to_label ofs in
            assert (not (HI.LabelHash.mem r label));
            HI.LabelHash.add r label ln in
          HI.LabelHash.iter f h;
          `LineNumberTable r
      | x -> x in
    let attrs = List.map check_code_attribute attrs in
    let attrs = List.map update_lnt attrs in
    `Code { code = instrs;
            exception_table = exceptions;
            attributes = attrs }

  let decode_attr_exceptions _ r st : t =
    let f st = CP.get_class_name r.da_pool (IS.read_u2 st) in
    `Exceptions (IS.read_elements st f)

  let decode_attr_inner_classes _ r st : t =
    let one st =
      let inner_class = IS.read_u2 st in
      let outer_class = IS.read_u2 st in
      let inner_name = IS.read_u2 st in
      let inner_flags = IS.read_u2 st in
      let cn = CP.get_class_name r.da_pool in
      let utf8 = CP.get_utf8_entry r.da_pool in
      let inner_class = option_of_u2 cn inner_class in
      let outer_class = option_of_u2 cn outer_class in
      let inner_name = option_of_u2 utf8 inner_name in
      let inner_flags = AF.from_u2 false inner_flags in
      let inner_flags = AF.check_inner_class_flags inner_flags in
      { inner_class; outer_class; inner_name; inner_flags } in
    `InnerClasses (IS.read_elements st one)

  let decode_attr_enclosing_method _ r st : t =
    let f i = match CP.get_entry r.da_pool i with
      | CP.NameAndType (name, desc) ->
          let utf8 = CP.get_utf8_entry r.da_pool in
          (Name.make_for_method (utf8 name),
            Descriptor.method_of_utf8 (utf8 desc))
      | _ -> fail Invalid_enclosing_method in
    let innermost_class = IS.read_u2 st in
    let enclosing_method = IS.read_u2 st in
    let innermost_class = CP.get_class_name r.da_pool innermost_class in
    let enclosing_method = option_of_u2 f enclosing_method in
    `EnclosingMethod { innermost_class; enclosing_method }

  let decode_attr_synthetic _ _ _ : t =
    `Synthetic

  let decode_attr_signature _ r st : t =
    let signature_index = InputStream.read_u2 st in
    let s = CP.get_utf8_entry r.da_pool signature_index in
    match r.da_element with
      | A.Class -> `ClassSignature (Signature.class_signature_of_utf8 s)
      | A.Method -> `MethodSignature (Signature.method_signature_of_utf8 s)
      | A.Field -> `FieldSignature (Signature.field_type_signature_of_utf8 s)
      | _ -> fail Invalid_attribute

  let decode_attr_source_file _ r st =
    let sourcefile_index = IS.read_u2 st in
    `SourceFile (CP.get_utf8_entry r.da_pool sourcefile_index)

  let decode_attr_source_debug_extension _ = failwith "todo:decode_attr_source_debug_extension"

  (* We provisionally build a table using program counters instead of labels,
  because we might have not yet seen the code. The decoding of methods must
  post-process to change program counters into labels. *)
  let decode_attr_line_number_table _ _ st =
    let h = IS.read_elements
      st
      (fun st ->
        let start_pc = IS.read_u2 st in
        let line_number = IS.read_u2 st in
        ((start_pc :> HI.label), (line_number :> int))) in
    let h = hash_of_list HI.LabelHash.create HI.LabelHash.add h in
    `LineNumberTable h

  let decode_attr_local_variable_table _ _ _ =
    (* See TODO note on type code_attribute. *)
    `LocalVariableTable ()

  let decode_attr_local_variable_type_table _ _ _ =
    (* See TODO note on type code_attribute. *)
    `LocalVariableTypeTable ()

  let decode_attr_deprecated _ _ _ : t = `Deprecated

  let decode_attr_runtime_visible_annotations _ r st : t =
    `RuntimeVisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_invisible_annotations _ r st : t =
    `RuntimeInvisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_visible_parameter_annotations _ r st : t =
    `RuntimeVisibleParameterAnnotations (read_annotations_list r.da_pool st)
  let decode_attr_runtime_invisible_parameter_annotations _ r st : t =
    `RuntimeInvisibleParameterAnnotations (read_annotations_list r.da_pool st)

  let decode_attr_runtime_visible_type_annotations _ = failwith "todo:decode_attr_runtime_visible_type_annotations"
  let decode_attr_runtime_invisible_type_annotations _ = failwith "todo:decode_attr_runtime_invisible_type_annotations"

  let decode_attr_annotation_default _ r st : t =
    let eiv = Annotation.read_info_element_value st in
    `AnnotationDefault (Annotation.decode_element_value r.da_pool eiv)

  let decode_attr_bootstrap_methods _ = failwith "todo:decode_attr_bootstrap_methods"
  let decode_attr_module _ = failwith "todo:decode_attr_module"
  let decode_attr_module_requires _ = failwith "todo:decode_attr_module_requires"
  let decode_attr_module_permits _ = failwith "todo:decode_attr_module_permits"
  let decode_attr_module_provides _ = failwith "todo:decode_attr_module_provides"

  module UTF8Hashtbl = Hashtbl.Make (Utils.UTF8)

  let decoders :
      ((decoding_arguments -> t) ->
        decoding_arguments -> InputStream.t -> t)
      UTF8Hashtbl.t =
    let ds = [
      attr_annotation_default, decode_attr_annotation_default;
      attr_bootstrap_methods, decode_attr_bootstrap_methods;
      attr_code, decode_attr_code;
      attr_constant_value, decode_attr_constant_value;
      attr_deprecated, decode_attr_deprecated;
      attr_enclosing_method, decode_attr_enclosing_method;
      attr_exceptions, decode_attr_exceptions;
      attr_inner_classes, decode_attr_inner_classes;
      attr_line_number_table, decode_attr_line_number_table;
      attr_local_variable_table, decode_attr_local_variable_table;
      attr_local_variable_type_table, decode_attr_local_variable_type_table;
      attr_module, decode_attr_module;
      attr_module_permits, decode_attr_module_permits;
      attr_module_provides, decode_attr_module_provides;
      attr_module_requires, decode_attr_module_requires;
      attr_runtime_invisible_annotations, decode_attr_runtime_invisible_annotations;
      attr_runtime_invisible_parameter_annotations, decode_attr_runtime_invisible_parameter_annotations;
      attr_runtime_invisible_type_annotations, decode_attr_runtime_invisible_type_annotations;
      attr_runtime_visible_annotations, decode_attr_runtime_visible_annotations;
      attr_runtime_visible_parameter_annotations, decode_attr_runtime_visible_parameter_annotations;
      attr_runtime_visible_type_annotations, decode_attr_runtime_visible_type_annotations;
      attr_signature, decode_attr_signature;
      attr_source_debug_extension, decode_attr_source_debug_extension;
      attr_source_file, decode_attr_source_file;
      attr_synthetic, decode_attr_synthetic;

(*  This one is too low-level to be decoded. It needs to be reconstructed
    during encoding.
      attr_stack_map_table, decode_attr_stack_map_table; *)
    ] in
    hash_of_list UTF8Hashtbl.create UTF8Hashtbl.add ds

  (* knot tying and visible functions *)

  let rec decode r =
    let st = IS.make_of_string r.da_i.A.data in
    let attr_name =
      CP.get_utf8_entry r.da_pool r.da_i.A.name_index in
    try UTF8Hashtbl.find decoders attr_name decode r st
    with Not_found -> `Unknown (attr_name, r.da_i.A.data)

  let decode_class da_pool da_i =
    match decode { da_element = A.Class; da_pool; da_i } with
      | #for_class as g -> g
      | b -> fail (Misplaced_attribute (name_of_attribute b, "class"))

  let decode_field da_pool da_i =
    match decode { da_element = A.Field; da_pool; da_i } with
      | #for_field as g -> g
      | b -> fail (Misplaced_attribute (name_of_attribute b, "field"))

  let decode_method da_pool da_i =
    match decode { da_element = A.Method; da_pool; da_i } with
      | #for_method as g -> g
      | b -> fail (Misplaced_attribute (name_of_attribute b, "method"))

  (* TODO(rgrig): Sort alphabetically. *)
  let rec version_bounds : t -> Version.bounds = function
    | `ConstantValue _ ->
        Version.make_bounds "'ConstantValue' attribute" Version.Java_1_0 None
    | `ClassSignature _ ->
        Version.make_bounds "'ClassSignature' attribute" Version.Java_1_5 None
    | `Code cv ->
        let instrs_bounds = List.map HI.version_bounds cv.code in
        let attrs_bounds = List.map version_bounds (cv.attributes :> t list) in
        Version.intersect_list (instrs_bounds @ attrs_bounds)
    | `Exceptions _ ->
        Version.make_bounds "'Exceptions' attribute" Version.Java_1_0 None
    | `InnerClasses _ ->
        Version.make_bounds "'InnerClasses' attribute" Version.Java_1_1 None
    | `EnclosingMethod _ ->
        Version.make_bounds "'EnclosingMethod' attribute" Version.Java_1_5 None
    | `Synthetic ->
        Version.make_bounds "'Synthetic' attribute" Version.Java_1_1 None
    | `FieldSignature _ ->
        Version.make_bounds "'Signature' attribute" Version.Java_1_5 None
    | `SourceFile _ ->
        Version.make_bounds "'SourceFile' attribute" Version.Java_1_0 None
    | `SourceDebugExtension _ ->
        Version.make_bounds "'SourceDebugExtension' attribute" Version.Java_1_5 None
    | `LineNumberTable _ ->
        Version.make_bounds "'LineNumberTable' attribute" Version.Java_1_0 None
    | `LocalVariableTable _ ->
        Version.make_bounds "'LocalVariableTable' attribute" Version.Java_1_0 None
    | `LocalVariableTypeTable _ ->
        Version.make_bounds "'LocalVariableTypeTable' attribute" Version.Java_1_5 None
    | `MethodSignature _ ->
        Version.make_bounds "'MethodSignature' attribute" Version.Java_1_5 None
    | `Deprecated ->
        Version.make_bounds "'Deprecated' attribute" Version.Java_1_1 None
    | `RuntimeVisibleAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeInvisibleAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeVisibleParameterAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleParameterAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeInvisibleParameterAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleParameterAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeVisibleTypeAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleTypeAnnotations' attribute" Version.Java_1_7 None
    | `RuntimeInvisibleTypeAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleTypeAnnotations' attribute" Version.Java_1_7 None
    | `AnnotationDefault _ ->
        Version.make_bounds "'AnnotationDefault' attribute" Version.Java_1_5 None
    | `BootstrapMethods _ ->
        Version.make_bounds "'BootstrapMethods' attribute" Version.Java_1_7 None
    | `Module _ ->
        Version.make_bounds "'Module' attribute" Version.Java_1_8 None
    | `Unknown _ ->
        Version.make_bounds "'Unknown' attribute" Version.Java_1_0 None

  type encoder = { en_pool : CP.extendable
		 ; en_buffer : Buffer.t
		 ; en_st : OutputStream.t }

  let make_encoder pool n =
    let buffer = Buffer.create 64 in
    let st = OutputStream.make_of_buffer buffer in
    { en_pool = pool; en_buffer = buffer; en_st = st }

  let enc_return enc n =
    let name_idx = CP.add_utf8 enc.en_pool n in
    let content = Buffer.contents enc.en_buffer in
    { A.name_index = name_idx;
      length = U.u4 (Int64.of_int (String.length content));
      data = content; }

  let encode_constant_value enc = function
      | Long_value l ->
          let idx = CP.add_long enc.en_pool l in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | Float_value f ->
          let idx = CP.add_float enc.en_pool f in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | Double_value d ->
          let idx = CP.add_double enc.en_pool d in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | Boolean_value b ->
          let idx = CP.add_integer enc.en_pool (if b then 1l else 0l) in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | Byte_value v | Character_value v | Short_value v ->
          let idx = CP.add_integer enc.en_pool (Int32.of_int v) in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | Integer_value i ->
          let idx = CP.add_integer enc.en_pool i in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | String_value s ->
          let idx = CP.add_string enc.en_pool s in
          OutputStream.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value

  let encode_code enc c = failwith "todo"
    (* first compute maps:
       instruction -> offset
       label -> offset
       for this we need instructon -> size
    *)
(* not ready yet
    let fold_size (l, ofs) inst = 
      let s = ByteCode.render_size ofs inst in
      (inst, ofs) :: l, ofs + s in
    let inst_ofs = List.fold_left fold_size ([], 0) c.code in
    let label_to_ofs lbl =
      let ((_, _), ofs) = List.find (fun ((l, _), _) -> l = lbl) inst_ofs in
      ofs in

      let code_content =
        List.map
          (HI.encode enc.en_pool)
          c.code in

      let code_enc = make_encoder 16 in
      ByteCode.write code_enc.st 0 code_content;
      OutputStream.close code_enc.st;

      let actual_code = Buffer.contents code_buffer in
      OutputStream.write_u2 st c.max_stack;
      OutputStream.write_u2 st c.max_locals;
      let code_length = String.length actual_code in
      if code_length > max_u2 then fail Invalid_code_length;
      OutputStream.write_u4 st (u4 (Int64.of_int code_length));
      OutputStream.write_bytes st actual_code;
      OutputStream.write_elements
        checked_length
        st
        (fun st elem ->
          let catch_idx = match elem.caught with
          | Some exn_name -> ConstantPool.add_class pool exn_name
          | None -> u2 0 in
          OutputStream.write_u2 st elem.try_start;
          OutputStream.write_u2 st elem.try_end;
          OutputStream.write_u2 st elem.catch;
          OutputStream.write_u2 st catch_idx)
        c.exception_table;
      let len' = checked_length c.attributes in
      OutputStream.write_u2 st len';
      let sub_buffer = Buffer.create 16 in
      let sub_st = OutputStream.make_of_buffer sub_buffer in
      List.iter
        (fun a ->
          let res = encode bsm pool (a :> t) in
          write_info sub_st res)
        c.attributes;
      OutputStream.close sub_st;
      OutputStream.write_bytes st (Buffer.contents sub_buffer);
      return attr_code
*)

  let encode pool =
    let enc = make_encoder pool 64 in
  function
    | `ConstantValue v -> encode_constant_value enc v
    | `Code c -> encode_code enc c
    | _ -> failwith "todo"

  let encode_class pool a = encode pool (a : for_class :> t)
  let encode_field pool a = encode pool (a : for_field :> t)
  let encode_method pool a = encode pool (a : for_method :> t)
end
(* }}} *)
module HA = HighAttribute

module F = Field
module HighField = struct (* {{{ *)
  type t = {
      flags : AF.for_field list; 
      name : Name.for_field;
      descriptor : Descriptor.for_field;
      attributes : HA.for_field list;
    }

  let decode is_interface pool i =
    let flags = AF.from_u2 false i.F.access_flags in
    let flags = AF.check_field_flags is_interface flags in
    let name = CP.get_utf8_entry pool i.F.name_index in
    if not (Name.is_valid_unqualified name) then fail Invalid_name;
    let name = Name.make_for_field name in
    let descriptor = CP.get_utf8_entry pool i.F.descriptor_index in
    let descriptor = Descriptor.field_of_utf8 descriptor in
    let attributes =
      U.map_array_to_list (HA.decode_field pool) i.F.attributes_array in
    { flags; name; descriptor; attributes }

  let encode _ = failwith "todo"
end (* }}} *)
module HF = HighField

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

  let decode_initializer init_attributes flags _ =
    let init_flags = AccessFlag.check_initializer_flags flags in
    Initializer { init_flags; init_attributes }

  let decode_constructor (cstr_descriptor,_) cstr_attributes flags _ =
    let cstr_flags = AccessFlag.check_constructor_flags flags in
    Constructor { cstr_flags; cstr_descriptor; cstr_attributes }

  let decode_regular i name descriptor attributes flags _ =
    let flags = AccessFlag.check_method_flags i flags in
    Regular { flags; name; descriptor; attributes}

  let decode is_interface pool m =
    let utf8 = CP.get_utf8_entry pool in (* (local) short name *)
    let utf8_name = utf8 m.M.name_index in
    let name = Name.make_for_method utf8_name in
    let descriptor =
      Descriptor.method_of_utf8 (utf8 m.M.descriptor_index) in
    let attributes =
      U.map_array_to_list (HA.decode_method pool) m.M.attributes_array in
    let flags = AccessFlag.from_u2 true m.M.access_flags in
    U.switch U.UTF8.equal
      [ class_initializer, decode_initializer attributes flags
      ; class_constructor, decode_constructor descriptor attributes flags ]
      (decode_regular is_interface name descriptor attributes flags)
      utf8_name

  let encode pool m =
    let flags, name, desc, attrs = match m with
      | Regular r ->
	r.flags,
	r.name,
	r.descriptor,
	r.attributes
      | Constructor c ->
	(c.cstr_flags :> AF.for_method list),
	(Name.make_for_method class_constructor),
	(c.cstr_descriptor, `Void),
	c.cstr_attributes
      | Initializer i ->
	(i.init_flags :> AF.for_method list),
	(Name.make_for_method class_initializer),
	([], `Void),
	i.init_attributes in
    let acc_flags = AF.list_to_u2 (flags :> AF.t list) in
    let name_idx = CP.add_utf8 pool (Name.utf8_for_method name) in
    let desc_utf8 = Descriptor.utf8_of_method desc in
    let desc_idx = CP.add_utf8 pool desc_utf8 in
    { M.access_flags = acc_flags;
      name_index = name_idx;
      descriptor_index = desc_idx;
      attributes_count = U.u2 (List.length attrs);
      attributes_array = U.map_list_to_array (HA.encode_method pool) (attrs :> HA.for_method list); }
end
(* }}} *)
module HM = HighMethod
(* }}} *)
(* rest/most of HighClass *) (* {{{ *)
type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : HF.t list;
    methods : HM.t list;
    attributes : HA.for_class list;
  }

let check_version_high ?(version = Version.default) c =
  let check_flag x = Version.check (AF.version_bounds x) version in
  let check_attribute x = Version.check (HA.version_bounds x) version in
  let check_field x =
    List.iter check_flag (x.HF.flags :> AF.t list);
    List.iter check_attribute (x.HF.attributes :> HA.t list) in
  let check_method =
    let cfa f a =
      List.iter check_flag (f :> AF.t list);
      List.iter check_attribute (a :> HA.t list) in
    function
      | HM.Regular { HM.flags = f; attributes = a; _ } -> cfa f a
      | HM.Constructor { HM.cstr_flags = f; cstr_attributes = a; _ } -> cfa f a
      | HM.Initializer { HM.init_flags = f; init_attributes = a; _ } -> cfa f a
  in
  List.iter check_field c.fields;
  List.iter check_method c.methods;
  List.iter check_attribute (c.attributes :> HA.t list);
  c

let decode ?(version = Version.default) cf =
  let pool = cf.CF.constant_pool in
  let check_version v =
    let v' = cf.CF.major_version, cf.CF.minor_version in
    let v' = Version.version_of_major_minor v' in
    Version.at_least "class file version" v' v;
    (* TODO: The following line should be [ClassFile.check ...]. *)
    CP.check_version v' pool in
  check_version version;
  let flags = AF.check_class_flags (AF.from_u2 false cf.CF.access_flags) in
  let class_name = CP.get_class_name pool in
  let extends =
    if cf.CF.super_class = U.u2 0
    then None
    else Some (class_name cf.ClassFile.super_class) in
  let is_interface = List.mem `Interface flags in
  let field_decode = HF.decode is_interface pool in
  let method_decode = HM.decode is_interface pool in
  let attribute_decode = HA.decode_class pool in
  check_version_high ~version:version { access_flags = flags;
    name = class_name cf.CF.this_class;
    extends = extends;
    implements = List.map class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }

(* TODO should this be in consts? *)
let no_super_class = U.u2 0

let encode ?(version = Version.default) cd =
  ignore (check_version_high ~version:version cd);
  let major, minor = Version.major_minor_of_version version in
  let pool = CP.make_extendable () in
  let this_index = CP.add_class pool cd.name in
  let super_index = match cd.extends with
  | Some n -> CP.add_class pool n
  | None -> no_super_class in
  let itfs = U.map_list_to_array (fun s -> CP.add_class pool s) cd.implements in
  let flds = U.map_list_to_array (HF.encode pool) cd.fields in
  let mths = U.map_list_to_array (HM.encode pool) cd.methods in
  let atts = U.map_list_to_array (HA.encode_class pool) cd.attributes in
  let cpool = CP.to_pool pool in
  let checked_length s sz =
    if sz <= U.max_u2 then
      U.u2 sz
    else
      fail (Too_many s) in
  let checked_length_array s arr =
    let res = Array.length arr in
    checked_length s res in
  CP.check_version version cpool;
  { CF.magic = U.u4 magic_number;
    CF.minor_version = minor;
    CF.major_version = major;
    CF.constant_pool_count = CP.size cpool;
    CF.constant_pool = cpool;
    CF.access_flags = AF.list_to_u2 (cd.access_flags :> AF.t list);
    CF.this_class = this_index;
    CF.super_class = super_index;
    CF.interfaces_count = checked_length_array "interfaces" itfs;
    CF.interfaces = itfs;
    CF.fields_count = checked_length_array "fields" flds;
    CF.fields = flds;
    CF.methods_count = checked_length_array "methods" mths;
    CF.methods = mths;
    CF.attributes_count = checked_length_array "attributes" atts;
    CF.attributes = atts; }

(* }}} *)
