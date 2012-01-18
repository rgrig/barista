(* open modules *) (* {{{ *)
open Consts
open Debug
open Format
(* }}} *)
(* module shorthands *) (* {{{ *)
module AF = AccessFlag
module BC = ByteCode
module CF = ClassFile
module CP = ConstantPool
module IS = InputStream
module OS = OutputStream
module U = Utils

(* }}} *)
(* errors *) (* {{{ *)
type error =
  | Invalid_TABLESWITCH
  | Invalid_attribute
  | Invalid_code_length
  | Invalid_constant_value
  | Invalid_enclosing_method
  | Invalid_method_handle
  | Invalid_module
  | Invalid_name
  | Invalid_offset
  | Invalid_pool_element
  | Invalid_pool_entry
  | Invalid_primitive_array_type
  | Misplaced_attribute of (string * string)
  | SE_array_expected of string
  | SE_category1_expected of string
  | SE_category2_expected of string
  | SE_different_stack_sizes of (int * int)
  | SE_empty_stack
  | SE_invalid_label
  | SE_invalid_local_contents of (int * string * string)
  | SE_uninitialized_register of (int * int)
  | SE_invalid_stack_top of (string * string)
  | SE_missing_return
  | SE_reference_expected of string
  | Too_many of string
  | Unsupported_instruction of string

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_TABLESWITCH -> "TABLESWITCH with empty range [low..high]"
  | Invalid_offset -> "offset does not point at start of instruction"
  | Misplaced_attribute (a, e) -> "attribute " ^ a ^ " appears on " ^ e
  | SE_array_expected s -> "SE: found " ^ s ^ " where an array was expected"
  | SE_category1_expected s -> "SE: found " ^ s ^ " where a value of category 1 was expected"
  | SE_category2_expected s -> "SE: found " ^ s ^ " where a value of category 2 was expected"
  | SE_different_stack_sizes (s, s') -> "SE: saw a stack with size " ^ (string_of_int s) ^ " but expected one with size " ^ (string_of_int s')
  | SE_empty_stack -> "SE: pop from empty stack during symbolic execution"
  | SE_invalid_label -> "SE: jump to inexistent label"
  | SE_invalid_local_contents (i, s, s') -> "SE: index " ^ (string_of_int i) ^ " contains " ^ s' ^ " but " ^ s ^ " was expected"
  | SE_uninitialized_register (i, len) -> "SE: requesting uninitialized register " ^ (string_of_int i) ^ " in a pool of size " ^ (string_of_int len)
  | SE_invalid_stack_top (s, s') -> "SE: top of stack is " ^ s' ^ " but " ^ s ^ " was expected"
  | SE_missing_return -> "SE: no return at end of method"
  | SE_reference_expected s -> "SE: found " ^ s ^ " where a reference was expected"
  | Too_many s -> "number of " ^ s ^ " exceeds " ^ (string_of_int (U.max_u2 :> int))
  | Unsupported_instruction s -> "unsupported instruction: " ^ s
  | _ -> "undescribed error (todo)"

let checked_length s l =
  let res = List.length l in
  if res <= U.max_u2 then
    U.u2 res
  else
    fail (Too_many s)

let checked_length_u1 s l =
  let res = List.length l in
  if res <= U.max_u1 then
    U.u1 res
  else
    fail (Too_many s)

(* }}} *)
(* HighInstruction, SymbExe, HighAttribute, HighField and HighMethod *) (* {{{ *)
module HighInstruction = struct (* {{{ *)
  type label = int64

  let fresh_label =
    let x = ref (-1L) in
    fun () ->
      if !x = Int64.max_int then
        failwith "INTERNAL ERROR: run out of unique identifiers";
      x := Int64.succ !x;
      !x

  let invalid_label = -1L

  type iinc = { ii_var: int; ii_inc: int }
  type lookupswitch = { ls_def: label; ls_branches: (int * label) list }
  type tableswitch = { ts_def: label; ts_low: int; ts_high: int; ts_ofss: label list }

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
	     | `Method_handle of Bootstrap.method_handle
             | `Long of int64
             | `Double of float ]
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
    let hash = Hashtbl.hash
  end)

  (* TODO(rgrig): Why is [s1_to_int x] any better than [(x : U.s1 :> int)] ? *)
  let s1_to_int (s : U.s1) = (s :> int)
  let s2_to_int (s : U.s2) = (s :> int)
  let s4_to_int (s : U.s4) = ((Int32.to_int (s :> Int32.t)) :> int)

  let u1_to_int (u : U.u1) = (u :> int)
  let u2_to_int (u : U.u2) = (u :> int)

  (* TODO(rgrig): handle overflow *)
  let decode pool ofs_to_lbl ofs =
(*
    let abs_s_ofs_to_lbl (s : U.s2) = ofs_to_lbl (s :> int) in
    let abs_l_ofs_to_lbl (s : U.s4) = ofs_to_lbl (Int32.to_int (s :> Int32.t)) in
*)
    let rel_s_ofs_to_lbl (s : U.s2) = ofs_to_lbl (ofs + (s :> int)) in
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
      | BC.AALOAD -> AALOAD
      | BC.AASTORE -> AASTORE
      | BC.ACONST_NULL -> ACONST_NULL
      | BC.ALOAD p1 -> ALOAD (u1_to_int p1)
      | BC.ALOAD_0 -> ALOAD 0
      | BC.ALOAD_1 -> ALOAD 1
      | BC.ALOAD_2 -> ALOAD 2
      | BC.ALOAD_3 -> ALOAD 3
      | BC.ANEWARRAY p1 -> ANEWARRAY (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
      | BC.ARETURN -> ARETURN
      | BC.ARRAYLENGTH -> ARRAYLENGTH
      | BC.ASTORE p1 -> ASTORE (u1_to_int p1)
      | BC.ASTORE_0 -> ASTORE 0
      | BC.ASTORE_1 -> ASTORE 1
      | BC.ASTORE_2 -> ASTORE 2
      | BC.ASTORE_3 -> ASTORE 3
      | BC.ATHROW -> ATHROW
      | BC.BALOAD -> BALOAD
      | BC.BASTORE -> BASTORE
      | BC.BIPUSH p1 -> BIPUSH (s1_to_int p1)
      | BC.CALOAD -> CALOAD
      | BC.CASTORE -> CASTORE
      | BC.CHECKCAST p1 -> CHECKCAST (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
      | BC.D2F -> D2F
      | BC.D2I -> D2I
      | BC.D2L -> D2L
      | BC.DADD -> DADD
      | BC.DALOAD -> DALOAD
      | BC.DASTORE -> DASTORE
      | BC.DCMPG -> DCMPG
      | BC.DCMPL -> DCMPL
      | BC.DCONST_0 -> DCONST_0
      | BC.DCONST_1 -> DCONST_1
      | BC.DDIV -> DDIV
      | BC.DLOAD p1 -> DLOAD (u1_to_int p1)
      | BC.DLOAD_0 -> DLOAD 0
      | BC.DLOAD_1 -> DLOAD 1
      | BC.DLOAD_2 -> DLOAD 2
      | BC.DLOAD_3 -> DLOAD 3
      | BC.DMUL -> DMUL
      | BC.DNEG -> DNEG
      | BC.DREM -> DREM
      | BC.DRETURN -> DRETURN
      | BC.DSTORE p1 -> DSTORE (u1_to_int p1)
      | BC.DSTORE_0 -> DSTORE 0
      | BC.DSTORE_1 -> DSTORE 1
      | BC.DSTORE_2 -> DSTORE 2
      | BC.DSTORE_3 -> DSTORE 3
      | BC.DSUB -> DSUB
      | BC.DUP -> DUP
      | BC.DUP2 -> DUP2
      | BC.DUP2_X1 -> DUP2_X1
      | BC.DUP2_X2 -> DUP2_X2
      | BC.DUP_X1 -> DUP_X1
      | BC.DUP_X2 -> DUP_X2
      | BC.F2D -> F2D
      | BC.F2I -> F2I
      | BC.F2L -> F2L
      | BC.FADD -> FADD
      | BC.FALOAD -> FALOAD
      | BC.FASTORE -> FASTORE
      | BC.FCMPG -> FCMPG
      | BC.FCMPL -> FCMPL
      | BC.FCONST_0 -> FCONST_0
      | BC.FCONST_1 -> FCONST_1
      | BC.FCONST_2 -> FCONST_2
      | BC.FDIV -> FDIV
      | BC.FLOAD p1 -> FLOAD (u1_to_int p1)
      | BC.FLOAD_0 -> FLOAD 0
      | BC.FLOAD_1 -> FLOAD 1
      | BC.FLOAD_2 -> FLOAD 2
      | BC.FLOAD_3 -> FLOAD 3
      | BC.FMUL -> FMUL
      | BC.FNEG -> FNEG
      | BC.FREM -> FREM
      | BC.FRETURN -> FRETURN
      | BC.FSTORE p1 -> FSTORE (u1_to_int p1)
      | BC.FSTORE_0 -> FSTORE 0
      | BC.FSTORE_1 -> FSTORE 1
      | BC.FSTORE_2 -> FSTORE 2
      | BC.FSTORE_3 -> FSTORE 3
      | BC.FSUB -> FSUB
      | BC.GETFIELD p1 -> GETFIELD (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.GETSTATIC p1 -> GETSTATIC (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.GOTO p1 -> GOTO (rel_s_ofs_to_lbl p1)
      | BC.GOTO_W p1 -> GOTO (rel_l_ofs_to_lbl p1)
      | BC.I2B -> I2B
      | BC.I2C -> I2C
      | BC.I2D -> I2D
      | BC.I2F -> I2F
      | BC.I2L -> I2L
      | BC.I2S -> I2S
      | BC.IADD -> IADD
      | BC.IALOAD -> IALOAD
      | BC.IAND -> IAND
      | BC.IASTORE -> IASTORE
      | BC.ICONST_0 -> ICONST_0
      | BC.ICONST_1 -> ICONST_1
      | BC.ICONST_2 -> ICONST_2
      | BC.ICONST_3 -> ICONST_3
      | BC.ICONST_4 -> ICONST_4
      | BC.ICONST_5 -> ICONST_5
      | BC.ICONST_M1 -> ICONST_M1
      | BC.IDIV -> IDIV
      | BC.IF_ACMPEQ p1 -> IF_ACMPEQ (rel_s_ofs_to_lbl p1)
      | BC.IF_ACMPNE p1 -> IF_ACMPNE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPEQ p1 -> IF_ICMPEQ (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPGE p1 -> IF_ICMPGE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPGT p1 -> IF_ICMPGT (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPLE p1 -> IF_ICMPLE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPLT p1 -> IF_ICMPLT (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPNE p1 -> IF_ICMPNE (rel_s_ofs_to_lbl p1)
      | BC.IFEQ p1 -> IFEQ (rel_s_ofs_to_lbl p1)
      | BC.IFGE p1 -> IFGE (rel_s_ofs_to_lbl p1)
      | BC.IFGT p1 -> IFGT (rel_s_ofs_to_lbl p1)
      | BC.IFLE p1 -> IFLE (rel_s_ofs_to_lbl p1)
      | BC.IFLT p1 -> IFLT (rel_s_ofs_to_lbl p1)
      | BC.IFNE p1 -> IFNE (rel_s_ofs_to_lbl p1)
      | BC.IFNONNULL p1 -> IFNONNULL (rel_s_ofs_to_lbl p1)
      | BC.IFNULL p1 -> IFNULL (rel_s_ofs_to_lbl p1)
      | BC.IINC (p1, p2) -> IINC { ii_var = u1_to_int p1; ii_inc = s1_to_int p2 }
      | BC.ILOAD p1 -> ILOAD (u1_to_int p1)
      | BC.ILOAD_0 -> ILOAD 0
      | BC.ILOAD_1 -> ILOAD 1
      | BC.ILOAD_2 -> ILOAD 2
      | BC.ILOAD_3 -> ILOAD 3
      | BC.IMUL -> IMUL
      | BC.INEG -> INEG
      | BC.INSTANCEOF p1 -> INSTANCEOF (match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element)
      | BC.INVOKEDYNAMIC p1 -> fail (Unsupported_instruction "INVOKEDYNAMIC")
      | BC.INVOKEINTERFACE (p1, p2) -> INVOKEINTERFACE ((match entry p1 with | CP.InterfaceMethodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element), p2)
      | BC.INVOKESPECIAL p1 -> INVOKESPECIAL (match entry p1 with | CP.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.INVOKESTATIC p1 -> INVOKESTATIC (match entry p1 with | CP.Methodref (cls, nat) -> (get_method_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.INVOKEVIRTUAL p1 -> INVOKEVIRTUAL (match entry p1 with | CP.Methodref (cls, nat) -> (get_array_method_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.IOR -> IOR
      | BC.IREM -> IREM
      | BC.IRETURN -> IRETURN
      | BC.ISHL -> ISHL
      | BC.ISHR -> ISHR
      | BC.ISTORE p1 -> ISTORE (u1_to_int p1)
      | BC.ISTORE_0 -> ISTORE 0
      | BC.ISTORE_1 -> ISTORE 1
      | BC.ISTORE_2 -> ISTORE 2
      | BC.ISTORE_3 -> ISTORE 3
      | BC.ISUB -> ISUB
      | BC.IUSHR -> IUSHR
      | BC.IXOR -> IXOR
      | BC.JSR p1 -> JSR (rel_s_ofs_to_lbl p1)
      | BC.JSR_W p1 -> JSR (rel_l_ofs_to_lbl p1)
      | BC.L2D -> L2D
      | BC.L2F -> L2F
      | BC.L2I -> L2I
      | BC.LADD -> LADD
      | BC.LALOAD -> LALOAD
      | BC.LAND -> LAND
      | BC.LASTORE -> LASTORE
      | BC.LCMP -> LCMP
      | BC.LCONST_0 -> LCONST_0
      | BC.LCONST_1 -> LCONST_1
      | BC.LDC p1 -> LDC (match entry (U.u2_of_u1 p1) with | CP.Integer v -> `Int v | CP.Float v -> `Float (Int32.float_of_bits v) | CP.String idx -> `String (utf8 idx) | CP.Class idx -> get_class_or_array idx | CP.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (utf8 idx)) | CP.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
      | BC.LDC2_W p1 -> LDC (match entry p1 with | CP.Long (hi, lo) -> `Long (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo)) | CP.Double (hi, lo) -> `Double (Int64.float_of_bits (Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo))) | _ -> fail Invalid_pool_element)
      | BC.LDC_W p1 -> LDC (match entry p1 with | CP.Integer v -> `Int v | CP.Float v -> `Float (Int32.float_of_bits v) | CP.String idx -> `String (utf8 idx) | CP.Class idx -> get_class_or_array idx | CP.MethodType idx -> `Method_type (Descriptor.method_of_utf8 (utf8 idx)) | CP.MethodHandle (kind, idx) -> `Method_handle (get_method_handle kind idx) | _ -> fail Invalid_pool_element)
      | BC.LDIV -> LDIV
      | BC.LLOAD p1 -> LLOAD (u1_to_int p1)
      | BC.LLOAD_0 -> LLOAD 0
      | BC.LLOAD_1 -> LLOAD 1
      | BC.LLOAD_2 -> LLOAD 2
      | BC.LLOAD_3 -> LLOAD 3
      | BC.LMUL -> LMUL
      | BC.LNEG -> LNEG
      | BC.LOOKUPSWITCH (p1, p2, p3) ->
        let keys, offsets = List.split p3 in
        let labels = List.map rel_l_ofs_to_lbl offsets in
        let items = List.map s4_to_int keys in
      (* bytecode parser should ensure this *)
        assert (s4_to_int p2 = List.length p3);
        LOOKUPSWITCH { ls_def = rel_l_ofs_to_lbl p1
                     ; ls_branches = List.combine items labels }
      | BC.LOR -> LOR
      | BC.LREM -> LREM
      | BC.LRETURN -> LRETURN
      | BC.LSHL -> LSHL
      | BC.LSHR -> LSHR
      | BC.LSTORE p1 -> LSTORE (u1_to_int p1)
      | BC.LSTORE_0 -> LSTORE 0
      | BC.LSTORE_1 -> LSTORE 1
      | BC.LSTORE_2 -> LSTORE 2
      | BC.LSTORE_3 -> LSTORE 3
      | BC.LSUB -> LSUB
      | BC.LUSHR -> LUSHR
      | BC.LXOR -> LXOR
      | BC.MONITORENTER -> MONITORENTER
      | BC.MONITOREXIT -> MONITOREXIT
      | BC.MULTIANEWARRAY (p1, p2) -> MULTIANEWARRAY ((match entry p1 with | CP.Class idx -> get_class_or_array idx | _ -> fail Invalid_pool_element), u1_to_int p2)
      | BC.NEW p1 -> NEW (match entry p1 with | CP.Class idx -> (Name.make_for_class_from_internal (utf8 idx)) | _ -> fail Invalid_pool_element)
      | BC.NEWARRAY p1 -> NEWARRAY (primitive_array_type_of_int (p1 :> int))
      | BC.NOP -> NOP
      | BC.POP -> POP
      | BC.POP2 -> POP2
      | BC.PUTFIELD p1 -> PUTFIELD (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.PUTSTATIC p1 -> PUTSTATIC (match entry p1 with | CP.Fieldref (cls, nat) -> (get_field_ref cls nat) | _ -> fail Invalid_pool_element)
      | BC.RET p1 -> RET (u1_to_int p1)
      | BC.RETURN -> RETURN
      | BC.SALOAD -> SALOAD
      | BC.SASTORE -> SASTORE
      | BC.SIPUSH p1 -> SIPUSH (s2_to_int p1)
      | BC.SWAP -> SWAP
      | BC.TABLESWITCH (p1, p2, p3, p4) ->
        TABLESWITCH {
          ts_def = rel_l_ofs_to_lbl p1;
          ts_low = s4_to_int p2;
          ts_high = s4_to_int p3;
          ts_ofss = List.map rel_l_ofs_to_lbl p4 }
      | BC.WIDE_ALOAD p1 -> ALOAD (u2_to_int p1)
      | BC.WIDE_ASTORE p1 -> ASTORE (u2_to_int p1)
      | BC.WIDE_DLOAD p1 -> DLOAD (u2_to_int p1)
      | BC.WIDE_DSTORE p1 -> DSTORE (u2_to_int p1)
      | BC.WIDE_FLOAD p1 -> FLOAD (u2_to_int p1)
      | BC.WIDE_FSTORE p1 -> FSTORE (u2_to_int p1)
      | BC.WIDE_IINC (p1, p2) -> IINC { ii_var = u2_to_int p1
                                            ; ii_inc = s2_to_int p2 }
      | BC.WIDE_ILOAD p1 -> ILOAD (u2_to_int p1)
      | BC.WIDE_ISTORE p1 -> ISTORE (u2_to_int p1)
      | BC.WIDE_LLOAD p1 -> LLOAD (u2_to_int p1)
      | BC.WIDE_LSTORE p1 -> LSTORE (u2_to_int p1)
      | BC.WIDE_RET p1 -> RET (u2_to_int p1)

  (* NOTE: The current implementation generates suboptimal bytecode, but it
  ensures that the chosen opcodes do not depend on [ool] or on [here]. We do
  intend to generate better bytecode, but that requires calling this encode
  (from [encode_attr_code] in a sort-of fixed-point computation. *)
  let encode (ool : label -> int) here pool instruction =
    let s4 x = U.s4 (Int32.of_int x) in
    let offset l = ool l - here in
    let s2_offset l = U.s2 (offset l) in
    let s4_offset l = s4 (offset l) in
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
      | `getField x -> CP.Reference_getField x
      | `getStatic x -> CP.Reference_getStatic x
      | `putField x -> CP.Reference_putField x
      | `putStatic x -> CP.Reference_putStatic x
      | `invokeVirtual x -> CP.Reference_invokeVirtual x
      | `invokeStatic x -> CP.Reference_invokeStatic x
      | `invokeSpecial x -> CP.Reference_invokeSpecial x
      | `newInvokeSpecial x -> CP.Reference_newInvokeSpecial x
      | `invokeInterface x -> CP.Reference_invokeInterface x in
    let index_of_constant c = ((match c with
      | `Int v -> CP.add_integer pool v
      | `Float v -> CP.add_float pool v
      | `String v -> CP.add_string pool v
      | `Class_or_interface u -> CP.add_class pool u
      | `Array_type t -> CP.add_array_class pool t
      | `Method_type v -> CP.add_method_type pool v
      | `Method_handle v -> CP.add_method_handle pool (reference v)
      | `Long v -> CP.add_long pool v
      | `Double v -> CP.add_double pool v) :> int) in
    let u2_of_constant c =
      U.u2 (index_of_constant c) in
    let size_of_constant = function
      | `Int _
      | `Float _
      | `String _
      | `Class_or_interface _
      | `Array_type _
      | `Method_type _
      | `Method_handle _
          -> 1
      | `Long _
      | `Double _
          -> 2 in
    let bc_INVOKEVIRTUAL p = BC.INVOKEVIRTUAL (match p with
      | `Class_or_interface c, n, t -> CP.add_method pool c n t
      | `Array_type a, n, t -> CP.add_array_method pool a n t) in
    let bc_LOOKUPSWITCH { ls_def; ls_branches } =
      let def = s4_offset ls_def in
      let cnt = s4 (List.length ls_branches) in
      let eb (v, l) = (s4 v, s4_offset l) in
      let jmp = List.map eb (List.sort compare ls_branches) in
      BC.LOOKUPSWITCH (def, cnt, jmp) in
    let bc_TABLESWITCH { ts_def; ts_low; ts_high; ts_ofss } =
      if ts_low > ts_high then fail Invalid_TABLESWITCH;
      let def = s4_offset ts_def in
      let low, high = s4 ts_low, s4 ts_high in
      let ofss = List.map s4_offset ts_ofss in
      BC.TABLESWITCH (def, low, high, ofss) in

    (* Helpers that pick between wide and narrow versions of instructions. *)
    let bc_LDC c =
      let j = index_of_constant c in
      if size_of_constant c = 2 then
        BC.LDC2_W (U.u2 j)
      else
        BC.LDC_W (U.u2 j) in
    let bc_ALOAD x = BC.WIDE_ALOAD (U.u2 x) in
    let bc_ASTORE x = BC.WIDE_ASTORE (U.u2 x) in
    let bc_DLOAD x = BC.WIDE_DLOAD (U.u2 x) in
    let bc_DSTORE x = BC.WIDE_DSTORE (U.u2 x) in
    let bc_FLOAD x = BC.WIDE_FLOAD (U.u2 x) in
    let bc_FSTORE x = BC.WIDE_FSTORE (U.u2 x) in
    let bc_ILOAD x = BC.WIDE_ILOAD (U.u2 x) in
    let bc_ISTORE x = BC.WIDE_ISTORE (U.u2 x) in
    let bc_LLOAD x = BC.WIDE_LLOAD (U.u2 x) in
    let bc_LSTORE x = BC.WIDE_LSTORE (U.u2 x) in
    let bc_RET x = BC.WIDE_RET (U.u2 x) in
    let bc_IINC { ii_var; ii_inc } = BC.WIDE_IINC (U.u2 ii_var, U.s2 ii_inc) in
    let bc_GOTO l = BC.GOTO_W (s4_offset l) in
    let bc_JSR l = BC.JSR_W (s4_offset l) in

    match instruction with
    | AALOAD -> BC.AALOAD
    | AASTORE -> BC.AASTORE
    | ACONST_NULL -> BC.ACONST_NULL
    | ALOAD p1 -> bc_ALOAD p1
    | ANEWARRAY p1 -> BC.ANEWARRAY (u2_of_constant p1)
    | ARETURN -> BC.ARETURN
    | ARRAYLENGTH -> BC.ARRAYLENGTH
    | ASTORE p1 -> bc_ASTORE p1
    | ATHROW -> BC.ATHROW
    | BALOAD -> BC.BALOAD
    | BASTORE -> BC.BASTORE
    | BIPUSH p1 -> BC.BIPUSH (U.s1 p1)
    | CALOAD -> BC.CALOAD
    | CASTORE -> BC.CASTORE
    | CHECKCAST p1 -> BC.CHECKCAST (u2_of_constant p1)
    | D2F -> BC.D2F
    | D2I -> BC.D2I
    | D2L -> BC.D2L
    | DADD -> BC.DADD
    | DALOAD -> BC.DALOAD
    | DASTORE -> BC.DASTORE
    | DCMPG -> BC.DCMPG
    | DCMPL -> BC.DCMPL
    | DCONST_0 -> BC.DCONST_0
    | DCONST_1 -> BC.DCONST_1
    | DDIV -> BC.DDIV
    | DLOAD p1 -> bc_DLOAD p1
    | DMUL -> BC.DMUL
    | DNEG -> BC.DNEG
    | DREM -> BC.DREM
    | DRETURN -> BC.DRETURN
    | DSTORE p1 -> bc_DSTORE p1
    | DSUB -> BC.DSUB
    | DUP -> BC.DUP
    | DUP2 -> BC.DUP2
    | DUP2_X1 -> BC.DUP2_X1
    | DUP2_X2 -> BC.DUP2_X2
    | DUP_X1 -> BC.DUP_X1
    | DUP_X2 -> BC.DUP_X2
    | F2D -> BC.F2D
    | F2I -> BC.F2I
    | F2L -> BC.F2L
    | FADD -> BC.FADD
    | FALOAD -> BC.FALOAD
    | FASTORE -> BC.FASTORE
    | FCMPG -> BC.FCMPG
    | FCMPL -> BC.FCMPL
    | FCONST_0 -> BC.FCONST_0
    | FCONST_1 -> BC.FCONST_1
    | FCONST_2 -> BC.FCONST_2
    | FDIV -> BC.FDIV
    | FLOAD p1 -> bc_FLOAD p1
    | FMUL -> BC.FMUL
    | FNEG -> BC.FNEG
    | FREM -> BC.FREM
    | FRETURN -> BC.FRETURN
    | FSTORE p1 -> bc_FSTORE p1
    | FSUB -> BC.FSUB
    | GETFIELD (c, n, t) -> BC.GETFIELD (CP.add_field pool c n t)
    | GETSTATIC (c, n, t) -> BC.GETSTATIC (CP.add_field pool c n t)
    | GOTO p1 -> bc_GOTO p1
    | I2B -> BC.I2B
    | I2C -> BC.I2C
    | I2D -> BC.I2D
    | I2F -> BC.I2F
    | I2L -> BC.I2L
    | I2S -> BC.I2S
    | IADD -> BC.IADD
    | IALOAD -> BC.IALOAD
    | IAND -> BC.IAND
    | IASTORE -> BC.IASTORE
    | ICONST_0 -> BC.ICONST_0
    | ICONST_1 -> BC.ICONST_1
    | ICONST_2 -> BC.ICONST_2
    | ICONST_3 -> BC.ICONST_3
    | ICONST_4 -> BC.ICONST_4
    | ICONST_5 -> BC.ICONST_5
    | ICONST_M1 -> BC.ICONST_M1
    | IDIV -> BC.IDIV
    | IF_ACMPEQ p1 -> BC.IF_ACMPEQ (s2_offset p1)
    | IF_ACMPNE p1 -> BC.IF_ACMPNE (s2_offset p1)
    | IF_ICMPEQ p1 -> BC.IF_ICMPEQ (s2_offset p1)
    | IF_ICMPGE p1 -> BC.IF_ICMPGE (s2_offset p1)
    | IF_ICMPGT p1 -> BC.IF_ICMPGT (s2_offset p1)
    | IF_ICMPLE p1 -> BC.IF_ICMPLE (s2_offset p1)
    | IF_ICMPLT p1 -> BC.IF_ICMPLT (s2_offset p1)
    | IF_ICMPNE p1 -> BC.IF_ICMPNE (s2_offset p1)
    | IFEQ p1 -> BC.IFEQ (s2_offset p1)
    | IFGE p1 -> BC.IFGE (s2_offset p1)
    | IFGT p1 -> BC.IFGT (s2_offset p1)
    | IFLE p1 -> BC.IFLE (s2_offset p1)
    | IFLT p1 -> BC.IFLT (s2_offset p1)
    | IFNE p1 -> BC.IFNE (s2_offset p1)
    | IFNONNULL p1 -> BC.IFNONNULL (s2_offset p1)
    | IFNULL p1 -> BC.IFNULL (s2_offset p1)
    | IINC x -> bc_IINC x
    | ILOAD p1 -> bc_ILOAD p1
    | IMUL -> BC.IMUL
    | INEG -> BC.INEG
    | INSTANCEOF p1 -> BC.INSTANCEOF (u2_of_constant p1)
    | INVOKEINTERFACE ((c, n, t), p2) -> BC.INVOKEINTERFACE (CP.add_interface_method pool c n t, p2)
    | INVOKESPECIAL (c, n, t) -> BC.INVOKESPECIAL (CP.add_method pool c n t)
    | INVOKESTATIC (c, n, t) -> BC.INVOKESTATIC (CP.add_method pool c n t)
    | INVOKEVIRTUAL p1 -> bc_INVOKEVIRTUAL p1
    | IOR -> BC.IOR
    | IREM -> BC.IREM
    | IRETURN -> BC.IRETURN
    | ISHL -> BC.ISHL
    | ISHR -> BC.ISHR
    | ISTORE p1 -> bc_ISTORE p1
    | ISUB -> BC.ISUB
    | IUSHR -> BC.IUSHR
    | IXOR -> BC.IXOR
    | JSR p1 -> bc_JSR p1
    | L2D -> BC.L2D
    | L2F -> BC.L2F
    | L2I -> BC.L2I
    | LADD -> BC.LADD
    | LALOAD -> BC.LALOAD
    | LAND -> BC.LAND
    | LASTORE -> BC.LASTORE
    | LCMP -> BC.LCMP
    | LCONST_0 -> BC.LCONST_0
    | LCONST_1 -> BC.LCONST_1
    | LDC p1 -> bc_LDC p1
    | LDIV -> BC.LDIV
    | LLOAD p1 -> bc_LLOAD p1
    | LMUL -> BC.LMUL
    | LNEG -> BC.LNEG
    | LOOKUPSWITCH x -> bc_LOOKUPSWITCH x
    | LOR -> BC.LOR
    | LREM -> BC.LREM
    | LRETURN -> BC.LRETURN
    | LSHL -> BC.LSHL
    | LSHR -> BC.LSHR
    | LSTORE p1 -> bc_LSTORE p1
    | LSUB -> BC.LSUB
    | LUSHR -> BC.LUSHR
    | LXOR -> BC.LXOR
    | MONITORENTER -> BC.MONITORENTER
    | MONITOREXIT -> BC.MONITOREXIT
    | MULTIANEWARRAY (c, d) -> BC.MULTIANEWARRAY (u2_of_constant c, U.u1 d)
    | NEW c -> BC.NEW (CP.add_class pool c)
    | NEWARRAY p1 -> BC.NEWARRAY (U.u1 (int_of_primitive_array_type p1))
    | NOP -> BC.NOP
    | POP -> BC.POP
    | POP2 -> BC.POP2
    | PUTFIELD (c, n, t) -> BC.PUTFIELD (CP.add_field pool c n t)
    | PUTSTATIC (c, n, t) -> BC.PUTSTATIC (CP.add_field pool c n t)
    | RET p1 -> bc_RET p1
    | RETURN -> BC.RETURN
    | SALOAD -> BC.SALOAD
    | SASTORE -> BC.SASTORE
    | SIPUSH p1 -> BC.SIPUSH (U.s2 p1)
    | SWAP -> BC.SWAP
    | TABLESWITCH x -> bc_TABLESWITCH x

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
    | JSR _ -> Version.make_bounds "'JSR' instruction" Version.Java_1_0 (Some Version.Java_1_6)
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
    | RET _ -> Version.make_bounds "'RET' instruction" Version.Java_1_0 (Some Version.Java_1_6)
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

  type exception_table_element =
    { try_start : HI.label  (* inclusive *)
    ; try_end : HI.label  (* inclusive *)
    ; catch : HI.label
    ; caught : Name.for_class option }

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
end
(* }}} *)
module HA = HighAttribute
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

  let to_string = function
    | Regular r -> U.UTF8.to_string (Name.utf8_for_method r.name)
    | Constructor _ -> "Constructor"
    | Initializer _ -> "Initializer"
end
(* }}} *)
module HM = HighMethod
module SymbExe = struct  (* {{{ *)
  (* symbolic types {{{ *)
  module LabelSet = Set.Make(struct
    type t = HI.label
    let compare = compare
  end)
  module LS = LabelSet
  let pp_label_set pe f = LS.iter (fun e -> fprintf f "@ %a" pe e)

  type verification_type_info =
    | VI_top
    | VI_integer
    | VI_float
    | VI_long
    | VI_double
    | VI_null
    | VI_uninitialized_this
    | VI_object of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type]
    | VI_uninitialized of HI.label
    | VI_return_address of LS.t

  let pp_vi f = function
    | VI_top -> fprintf f "top"
    | VI_integer -> fprintf f "integer"
    | VI_float -> fprintf f "float"
    | VI_long -> fprintf f "long"
    | VI_double -> fprintf f "double"
    | VI_null -> fprintf f "null"
    | VI_uninitialized_this -> fprintf f "uninitialized_this"
    | VI_object _ -> fprintf f "object"
    | VI_uninitialized l -> fprintf f "uninitialized(%Ld)" l
    | VI_return_address l -> fprintf f "return_address [%a]" (pp_label_set (fun f a -> fprintf f "%Ld" a)) l

  let equal_verification_type_info x y = match (x, y) with
    | (VI_object (`Class_or_interface cn1)),
      (VI_object (`Class_or_interface cn2)) -> Name.equal_for_class cn1 cn2
    | (VI_object (`Array_type at1)),
      (VI_object (`Array_type at2)) ->
	Descriptor.equal_java_type
          (at1 :> Descriptor.java_type)
          (at2 :> Descriptor.java_type)
    | (VI_object _), (VI_object _) -> false
    | VI_return_address _, VI_return_address _ -> true
    | _ -> x = y

  let string_of_verification_type_info = function
    | VI_top -> "top"
    | VI_integer -> "int"
    | VI_float -> "float"
    | VI_long -> "long"
    | VI_double -> "double"
    | VI_null -> "null"
    | VI_uninitialized_this -> "uninit this"
    | VI_object (`Class_or_interface cn) ->
      U.UTF8.to_string_noerr (Name.external_utf8_for_class cn)
    | VI_object (`Array_type ((`Array _) as a)) ->
      let res = Descriptor.external_utf8_of_java_type (a :> Descriptor.java_type) in
      (U.UTF8.to_string_noerr res)
    | VI_uninitialized label ->
      Printf.sprintf "uninit %Ld" (label :> int64)
    | VI_return_address _ -> "returnAddress"

  let verification_type_info_of_parameter_descriptor = function
    | `Boolean -> VI_integer
    | `Byte -> VI_integer
    | `Char -> VI_integer
    | `Double -> VI_double
    | `Float -> VI_float
    | `Int -> VI_integer
    | `Long -> VI_long
    | `Short -> VI_integer
    | `Class cn -> VI_object (`Class_or_interface cn)
    | `Array e -> VI_object (`Array_type (`Array e))

  let java_lang_String = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.String")
  let java_lang_Class = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.Class")
  let java_lang_invoke_MethodType = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.invoke.MethodType")
  let java_lang_invoke_MethodHandle = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.invoke.MethodHandle")
  let verification_type_info_of_constant_descriptor = function
    | `Int _ -> VI_integer
    | `Float _ -> VI_float
(* TODO(rgrig): Does `Class_or_interface need distinct parameters here? *)
    | `String _ -> VI_object (`Class_or_interface java_lang_String)
    | `Class_or_interface _ -> VI_object (`Class_or_interface java_lang_Class)
    | `Array_type _ -> VI_object (`Class_or_interface java_lang_Class)
    | `Long _ -> VI_long
    | `Double _ -> VI_double
    | `Interface_method _ -> VI_object (`Class_or_interface java_lang_invoke_MethodHandle)
    | `Method_type _ -> VI_object (`Class_or_interface java_lang_invoke_MethodType)
    | `Method_handle _ -> VI_object (`Class_or_interface java_lang_invoke_MethodHandle)

  let enclose (x : Descriptor.array_type) =
    `Array (x :> Descriptor.for_field)

  let verification_type_info_of_array_element = function
    | `Array_type at -> VI_object (`Array_type (enclose at))
    | `Class_or_interface cn -> VI_object (`Array_type (`Array (`Class cn)))

  let verification_type_info_of_array_primitive = function
    | `Boolean -> VI_object (`Array_type (`Array `Boolean))
    | `Char -> VI_object (`Array_type (`Array `Char))
    | `Float -> VI_object (`Array_type (`Array `Float))
    | `Double -> VI_object (`Array_type (`Array `Double))
    | `Byte -> VI_object (`Array_type (`Array `Byte))
    | `Short -> VI_object (`Array_type (`Array `Short))
    | `Int -> VI_object (`Array_type (`Array `Int))
    | `Long -> VI_object (`Array_type (`Array `Long))
    | _ -> fail Invalid_primitive_array_type
  (* }}} *)
  (* error reporting {{{ *)
  let report_invalid_stack_top (v, v') =
    SE_invalid_stack_top (string_of_verification_type_info v, string_of_verification_type_info v')

  let report_reference_expected x = SE_reference_expected (string_of_verification_type_info x)

  let report_array_expected x = SE_array_expected (string_of_verification_type_info x)

  let report_invalid_local_contents (i, v, v') =
    SE_invalid_local_contents (i, string_of_verification_type_info v, string_of_verification_type_info v')

  let report_category1_expected x = SE_category1_expected (string_of_verification_type_info x)
  let report_category2_expected x = SE_category2_expected (string_of_verification_type_info x)
  (* }}} *)
  (* symbolic stack {{{ *)
  type stack = verification_type_info list (* stack top is list head *)

  let pp_stack f x = fprintf f "@[[%a ]@]" (U.pp_list pp_vi) x

  let empty () = []

  let push v s =
    if log_se then printf "+";
    v :: s

  let push_return_value x s = match x with
    | `Void -> s
    | #Descriptor.for_parameter as y ->
      push (verification_type_info_of_parameter_descriptor y) s

  let top = function
    | hd :: _ -> hd
    | [] -> fail SE_empty_stack

  let pop =
  function
    | _ :: tl -> if log_se then printf "-"; tl
    | [] -> fail SE_empty_stack

  let pop_if v s =
    let v' = top s in
    let popable = match v with
      | VI_object _ -> true
      | _ -> equal_verification_type_info v v' in
    if popable then
      pop s
    else
      fail (report_invalid_stack_top (v, v'))

  let is_category1 = function
    | VI_float
    | VI_integer
    | VI_null
    | VI_object _
    | VI_return_address _
    | VI_top
    | VI_uninitialized_this
    | VI_uninitialized _
        -> true
    | VI_double
    | VI_long
        -> false

  let pop_if_category1 = function
    | hd :: tl ->
      if is_category1 hd
      then (if log_se then printf "-"; (hd, tl))
      else fail (report_category1_expected hd)
    | [] -> fail SE_empty_stack

  let pop_if_category2 = function
    | hd :: tl ->
      if not (is_category1 hd)
      then (if log_se then printf "-"; (hd, tl))
      else fail (report_category2_expected hd)
    | [] -> fail SE_empty_stack
  (* }}} *)
  (* symbolic pool {{{ *)
  type locals = verification_type_info U.IntMap.t
  let pp_locals f ls =
    let pb r v = fprintf f "@ (%d->%a)" r pp_vi v in
    fprintf f "@[["; U.IntMap.iter pb ls; fprintf f " ]@]"

  let equal_locals a b = U.IntMap.equal (=) a b

  let intmap_width m =
    try
      let a, _ = U.IntMap.min_binding m in
      let b, _ = U.IntMap.max_binding m in
      b - a + 1
    with Not_found -> 0

  let load i l =
    try U.IntMap.find i l
    with Not_found ->
      fail (SE_uninitialized_register (i, intmap_width l))

  let check_load i l v =
    let v' = load i l in
    if not (equal_verification_type_info v v') then
      fail (report_invalid_local_contents (i, v, v'))

  let store i v l =
    let l = U.IntMap.add i v l in
    match v with
      | VI_long
      | VI_double -> U.IntMap.add (succ i) VI_top l
      | _ -> l
  (* }}} *)
  (* checks {{{ *)
  let check_reference x =
    match x with
      | VI_integer
      | VI_float
      | VI_long
      | VI_double
      | VI_return_address _
      | VI_top -> fail (report_reference_expected x)
      | VI_null
      | VI_uninitialized_this
      | VI_object _
      | VI_uninitialized _ -> ()

  let check_reference_or_return x =
    if not (equal_verification_type_info x (VI_return_address LS.empty))
    then check_reference x
  (* }}} *)
  (* symbolic execution {{{ *)

  type 'a stepper = 'a -> HI.t -> HI.label -> 'a * HI.label list
  type 'a executor =
    'a stepper -> ('a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> bool) -> 'a -> HA.code_value
    -> 'a HI.LabelHash.t

  type t = {
    locals : locals;
    stack : stack;
  }

  let pp_t f { locals; stack } =
    fprintf f "@[<1>(%a,@ %a)@]" pp_locals locals pp_stack stack

  let pp_map_t f m =
    let l = ref [] in
    HI.LabelHash.iter (fun k v -> l := (k, v) :: !l) m;
    let l = List.sort compare !l in
    let pb (l, t) = fprintf f "@\n@[%Ld -> %a@]" l pp_t t in
    fprintf f "@\n@[<2>(symbolic execution state %d" (HI.LabelHash.length m);
    List.iter pb l;
    fprintf f "@\n)@]@\n"

  let equal_t a b =
    equal_locals a.locals b.locals && a.stack = b.stack

  let java_lang_Object_name = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.Object")

  let java_lang_Object = `Class_or_interface java_lang_Object_name

  let make_empty () =
    { locals = U.IntMap.empty; stack = []; }

  let stack_size st =
    List.fold_left
      (fun acc x -> acc + (match x with VI_double | VI_long -> 2 | _ -> 1))
      0
      st.stack

  let locals_size st = intmap_width st.locals

  (*  PRE
        labels in code are distinct
      NOTATIONS
        h     cache (hash table)
        f g   small 'anonymous' function
        l m   label
        c     instruction
        s t   state
        e     exception handler (or label of)
   *)
  let execute_method step exec_throw unify eq init code =
    let module H = HI.LabelHash in
    match code.HA.code with
      | [] -> H.create 2
      | ((l, _) :: _) as cs ->
          let instruction_at =
            let h = H.create 13 in
            let f ((l, _) as c) = H.add h l c in
            List.iter f cs;
            fun l -> try H.find h l with Not_found -> fail SE_invalid_label in
          let next_label =
            let h = H.create 13 in
            let rec f = function
              | (l, _) :: (((m, _) :: _) as cs) -> H.add h l m; f cs
              | _ -> () in
            f cs;
            fun l -> try H.find h l with Not_found -> HI.invalid_label in
          let handlers_at =
            let h = H.create 13 in
            let get l = try H.find h l with Not_found -> [] in
            let add l e = H.replace h l (e :: (get l)) in
            let rec f e l m = add m e; if l <> m then f e l (next_label m) in
            let g e = f e.HA.catch e.HA.try_end e.HA.try_start in
            List.iter g code.HA.exception_table; get in
          let result = H.create 13 in
          let record_state s l =
            try
              let t = H.find result l in
              let s = unify s t in
              H.replace result l s;
              (s, not (eq s t))
            with Not_found ->
              (H.add result l s; (s, true)) in
          let exec exec' step s l = (* this is the main part *)
            if l = HI.invalid_label then fail SE_missing_return;
            let s, progress = record_state s l in
            if progress then begin
	      if log_se_full then pp_map_t std_formatter result;
              let t, ls = step s (instruction_at l) (next_label l) in
              List.iter (exec' "black" s l t) ls;
              List.iter (exec' "red" s l (exec_throw t)) (handlers_at l)
            end in
          let rec normal_exec _ _ _ = exec normal_exec step in
          let rec log_exec color s l t m =
            let sl = Hashtbl.hash (s, l) in
            let tm = Hashtbl.hash (t, m) in
            let step s c ls =
              printf "@\n@[%d [shape=box,label=\"%Ld:" tm m;
              let r = step s c ls in printf "\"];@]"; r in
            printf "@\n@[%d -> %d [color=%s];@]" sl tm color;
            exec log_exec step t m in
          let limit_exec n =
            let x = ref n in
            let rec f _ _ _ t m =
              if !x > 0 then (decr x; exec f step t m) in
            f in
          let exec = if log_se then log_exec else normal_exec in
(*           let exec = limit_exec 10000 in *)
          exec "green" init HI.invalid_label init l;
          result

  (* was StackState.update *)
  let step st (lbl, i) next_lbl =
    let continue locals stack = ({stack; locals}, [next_lbl]) in
    let jump1 locals stack jump_lbls = ({stack; locals}, jump_lbls) in
    let jump2 locals stack jump_lbl = ({stack; locals}, [jump_lbl; next_lbl]) in
    let return locals stack = ({stack; locals}, []) in
    let locals = st.locals in
    let stack = st.stack in
    match i with
      | HI.AALOAD ->
	let stack = pop_if VI_integer stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack =
          (match topv with
            | VI_null -> push VI_null stack
            | VI_object (`Array_type (`Array t)) -> push (verification_type_info_of_parameter_descriptor t) stack
            | _ -> fail (report_array_expected topv)) in
	continue locals stack
      | HI.AASTORE ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = pop_if VI_integer stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | HI.ACONST_NULL ->
	let stack = push VI_null stack in
	continue locals stack
      | HI.ALOAD parameter ->
	let loc = load parameter locals in
	check_reference loc;
	let stack = push loc stack in
	continue locals stack
      | HI.ANEWARRAY parameter ->
	let stack = pop_if VI_integer stack in
	let stack = push (verification_type_info_of_array_element parameter) stack in
	continue locals stack
      | HI.ARETURN ->
	let stack = pop stack in
	return locals stack
      | HI.ARRAYLENGTH ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ASTORE parameter ->
	let loc = top stack in
	let stack = pop stack in
	check_reference_or_return loc;
	let locals = store parameter loc locals in
	continue locals stack
      | HI.ATHROW ->
	let exc = top stack in
        check_reference exc;
	return locals stack
      | HI.BALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.BASTORE ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.BIPUSH _ ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.CALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.CASTORE ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.CHECKCAST parameter ->
	let stack = pop stack in
	let stack = push (verification_type_info_of_parameter_descriptor (match parameter with `Array_type at -> (at :> Descriptor.for_parameter) | `Class_or_interface cn -> `Class cn)) stack in
	continue locals stack
      | HI.D2F ->
	let stack = pop_if VI_double stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.D2I ->
	let stack = pop_if VI_double stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.D2L ->
	let stack = pop_if VI_double stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.DADD ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DASTORE ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.DCMPG ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.DCMPL ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.DCONST_0 ->
	let stack = push VI_double stack in
	continue locals stack
      | HI.DCONST_1 ->
	let stack = push VI_double stack in
	continue locals stack
      | HI.DDIV ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DLOAD parameter ->
	check_load parameter locals VI_double;
	let stack = push VI_double stack in
	continue locals stack
      | HI.DMUL ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DNEG ->
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DREM ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DRETURN ->
	let stack = pop_if VI_double stack in
	return locals stack
      | HI.DSTORE parameter ->
	let stack = pop_if VI_double stack in
	let locals = store parameter VI_double locals in
	continue locals stack
      | HI.DSUB ->
	let stack = pop_if VI_double stack in
	let stack = pop_if VI_double stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.DUP ->
	let v, stack = pop_if_category1 stack in
	let stack = push v stack in
	let stack = push v stack in
	continue locals stack
      | HI.DUP2 ->
	let v1 = top stack in
	let stack = pop stack in
	let stack =
          if is_category1 v1 then
            let v2, stack = pop_if_category1 stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            push v1 (push v1 stack) in
	continue locals stack
      | HI.DUP2_X1 ->
	let v1 = top stack in
	let stack =
          if is_category1 v1 then
            let stack = pop stack in
            let v2, stack = pop_if_category1 stack in
            let v3, stack = pop_if_category1 stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            let stack = pop stack in
            let v2, stack = pop_if_category1 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack in
	continue locals stack
      | HI.DUP2_X2 ->
	let v1 = top stack in
	let stack =
          if is_category1 v1 then begin
            let stack = pop stack in
            let v2, stack = pop_if_category1 stack in
            let v3 = top stack in
            if is_category1 v3 then begin
              let stack = pop stack in
              let v4 = top stack in
              let stack = pop stack in
              let stack = push v2 stack in
              let stack = push v1 stack in
              let stack = push v4 stack in
              let stack = push v3 stack in
              let stack = push v2 stack in
              push v1 stack
            end else begin
              let stack = pop stack in
              let stack = push v2 stack in
              let stack = push v1 stack in
              let stack = push v3 stack in
              let stack = push v2 stack in
              push v1 stack
            end
          end else begin
            let stack = pop stack in
            let v2 = top stack in
            if is_category1 v2 then begin
              let stack = pop stack in
              let v3, stack = pop_if_category1 stack in
              let stack = push v1 stack in
              let stack = push v3 stack in
              let stack = push v2 stack in
              push v1 stack
            end else begin
              let stack = pop stack in
              let stack = push v1 stack in
              let stack = push v2 stack in
              push v1 stack
            end
          end in
	continue locals stack
      | HI.DUP_X1 ->
	let v1, stack = pop_if_category1 stack in
	let v2, stack = pop_if_category1 stack in
	let stack = push v1 stack in
	let stack = push v2 stack in
	let stack = push v1 stack in
	continue locals stack
      | HI.DUP_X2 ->
	let v1, stack = pop_if_category1 stack in
	let v2 = top stack in
	let stack =
          if is_category1 v2 then
            let v2, stack = pop_if_category1 stack in
            let v3, stack = pop_if_category1 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            let v2, stack = pop_if_category2 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack in
	continue locals stack
      | HI.F2D ->
	let stack = pop_if VI_float stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.F2I ->
	let stack = pop_if VI_float stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.F2L ->
	let stack = pop_if VI_float stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.FADD ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FASTORE ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.FCMPG ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.FCMPL ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.FCONST_0 ->
	let stack = push VI_float stack in
	continue locals stack
      | HI.FCONST_1 ->
	let stack = push VI_float stack in
	continue locals stack
      | HI.FCONST_2 ->
	let stack = push VI_float stack in
	continue locals stack
      | HI.FDIV ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FLOAD parameter ->
	check_load parameter locals VI_float;
	let stack = push VI_float stack in
	continue locals stack
      | HI.FMUL ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FNEG ->
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FREM ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.FRETURN ->
	let stack = pop_if VI_float stack in
	return locals stack
      | HI.FSTORE parameter ->
	let stack = pop_if VI_float stack in
	let locals = store parameter VI_float locals in
	continue locals stack
      | HI.FSUB ->
	let stack = pop_if VI_float stack in
	let stack = pop_if VI_float stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.GETFIELD (_, _, desc) ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push (verification_type_info_of_parameter_descriptor desc) stack in
	continue locals stack
      | HI.GETSTATIC (_, _, desc) ->
	let stack = push (verification_type_info_of_parameter_descriptor desc) stack in
	continue locals stack
      | HI.GOTO lbl ->
	jump1 locals stack [lbl]
      | HI.I2B ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.I2C ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.I2D ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.I2F ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.I2L ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.I2S ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IADD ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IAND ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IASTORE ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.ICONST_0 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_1 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_2 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_3 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_4 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_5 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ICONST_M1 ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IDIV ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IF_ACMPEQ lbl ->
	let stack = pop stack in
	let stack = pop stack in
	jump2 locals stack lbl
      | HI.IF_ACMPNE lbl ->
	let stack = pop stack in
	let stack = pop stack in
	jump2 locals stack lbl
      | HI.IF_ICMPEQ lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IF_ICMPGE lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IF_ICMPGT lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IF_ICMPLE lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IF_ICMPLT lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IF_ICMPNE lbl ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFEQ lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFGE lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFGT lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFLE lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFLT lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFNE lbl ->
	let stack = pop_if VI_integer stack in
	jump2 locals stack lbl
      | HI.IFNONNULL lbl ->
	let stack = pop stack in
	jump2 locals stack lbl
      | HI.IFNULL lbl ->
	let stack = pop stack in
	jump2 locals stack lbl
      | HI.IINC ii ->
	check_load ii.HI.ii_var locals VI_integer;
	let locals = store ii.HI.ii_var VI_integer locals in
	continue locals stack
      | HI.ILOAD parameter ->
	check_load parameter locals VI_integer;
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IMUL ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.INEG ->
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.INSTANCEOF _ ->
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
  (*
    | HI.INVOKEDYNAMIC (_, _, (params, ret)) ->
    let infos = List.rev_map verification_type_info_of_parameter_descriptor params in
    let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
    let topv = top stack in
    check_reference topv;
    let stack = pop stack in
    let stack = push_return_value ret stack in
    { locals; stack }
  *)
      | HI.INVOKEINTERFACE ((_, _, (params, ret)), _) ->
	let infos = List.rev_map verification_type_info_of_parameter_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	continue locals stack
      | HI.INVOKESPECIAL (cn, mn, (params, ret)) ->
	let infos = List.rev_map verification_type_info_of_parameter_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	let locals, stack =
        (* TODO(rgrig): This is wrong. Fix. *)
	(* TODO(rlp) understand what happens here *)
          if U.UTF8.equal Consts.class_constructor (Name.utf8_for_method mn) then
            match topv with
              | VI_uninitialized lbl ->
		let f = function
		  | VI_uninitialized lbl' when lbl = lbl' ->
		    VI_object (`Class_or_interface cn)
		  | x -> x in
		U.IntMap.map f locals, List.map f stack
              | VI_uninitialized_this ->
		let f = function VI_uninitialized_this -> VI_object (`Class_or_interface cn) | x -> x in
		U.IntMap.map f locals, List.map f stack
              | _ -> locals, stack
          else
            locals, stack in
	continue locals stack
      | HI.INVOKESTATIC (_, _, (params, ret)) ->
	let infos = List.rev_map verification_type_info_of_parameter_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let stack = push_return_value ret stack in
	continue locals stack
      | HI.INVOKEVIRTUAL (_, _, (params, ret)) ->
	let infos = List.rev_map verification_type_info_of_parameter_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	continue locals stack
      | HI.IOR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IREM ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IRETURN ->
	let stack = pop_if VI_integer stack in
	return locals stack
      | HI.ISHL ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ISHR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.ISTORE parameter ->
	let stack = pop_if VI_integer stack in
	let locals = store parameter VI_integer locals in
	continue locals stack
      | HI.ISUB ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IUSHR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.IXOR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.JSR lbl ->
        let stack = push (VI_return_address (LS.singleton next_lbl)) stack in
	jump1 locals stack [lbl]
      | HI.L2D ->
	let stack = pop_if VI_long stack in
	let stack = push VI_double stack in
	continue locals stack
      | HI.L2F ->
	let stack = pop_if VI_long stack in
	let stack = push VI_float stack in
	continue locals stack
      | HI.L2I ->
	let stack = pop_if VI_long stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.LADD ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LAND ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LASTORE ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.LCMP ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.LCONST_0 ->
	let stack = push VI_long stack in
	continue locals stack
      | HI.LCONST_1 ->
	let stack = push VI_long stack in
	continue locals stack
      | HI.LDC parameter ->
	let stack = push (verification_type_info_of_constant_descriptor parameter) stack in
	continue locals stack
      | HI.LDIV ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LLOAD parameter ->
	check_load parameter locals VI_long;
	let stack = push VI_long stack in
	continue locals stack
      | HI.LMUL ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LNEG ->
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LOOKUPSWITCH _ ->
	let stack = pop_if VI_integer stack in
	continue locals stack
      | HI.LOR ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LREM ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LRETURN ->
	let stack = pop_if VI_long stack in
	return locals stack
      | HI.LSHL ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LSHR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LSTORE parameter ->
	let stack = pop_if VI_long stack in
	let locals = store parameter VI_long locals in
	continue locals stack
      | HI.LSUB ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LUSHR ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.LXOR ->
	let stack = pop_if VI_long stack in
	let stack = pop_if VI_long stack in
	let stack = push VI_long stack in
	continue locals stack
      | HI.MONITORENTER ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | HI.MONITOREXIT ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | HI.MULTIANEWARRAY (at, dims) ->
	let s = ref stack in
	for i = 1 to (dims :> int) do
          s := pop_if VI_integer !s;
	done;
	let stack = push (VI_object at) !s in
	continue locals stack
      | HI.NEW _ ->
      (* TODO(rlp) understand why the argument to NEW is thrown away *)
	let stack = push (VI_uninitialized lbl) stack in
	continue locals stack
      | HI.NEWARRAY parameter ->
	let stack = pop_if VI_integer stack in
	let stack = push (verification_type_info_of_array_primitive parameter) stack in
	continue locals stack
      | HI.NOP ->
	continue locals stack
      | HI.POP ->
	let _, stack = pop_if_category1 stack in
	continue locals stack
      | HI.POP2 ->
	let v1 = top stack in
	let stack =
          if is_category1 v1 then
            snd (pop_if_category1 (snd (pop_if_category1 stack)))
          else
            pop stack in
	continue locals stack
      | HI.PUTFIELD (_, _, desc) ->
	let stack = pop_if (verification_type_info_of_parameter_descriptor desc) stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | HI.PUTSTATIC (_, _, desc) ->
	let stack = pop_if (verification_type_info_of_parameter_descriptor desc) stack in
	continue locals stack
      | HI.RET index ->
        let lbls = (match load index locals with
          | VI_return_address lbls -> LS.elements lbls
          | v -> fail (report_invalid_local_contents
              (index, v, VI_return_address LS.empty))) in
        jump1 locals stack lbls
      | HI.RETURN ->
	return locals stack
      | HI.SALOAD ->
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	let stack = push VI_integer stack in
	continue locals stack
      | HI.SASTORE ->
	let stack = pop_if VI_integer stack in
	let stack = pop_if VI_integer stack in
	let stack = pop stack in
	continue locals stack
      | HI.SIPUSH _ ->
	let stack = push VI_integer stack in
	continue locals stack
      | HI.SWAP ->
	let v1, stack = pop_if_category1 stack in
	let v2, stack = pop_if_category1 stack in
	let stack = push v1 stack in
	let stack = push v2 stack in
	continue locals stack
      | HI.TABLESWITCH _ ->
	let stack = pop_if VI_integer stack in
	continue locals stack
  (* }}} *)
  (* unification {{{ *)
  type 'a unifier = 'a -> 'a -> 'a

  let make_array_unifier (f : Name.for_class unifier) (x : Descriptor.array_type) (y : Descriptor.array_type) =
    let rec ua (x : Descriptor.java_type) (y : Descriptor.java_type) = match x, y with
      | (`Array x'), (`Array y') -> `Array (ua (x' :> Descriptor.java_type) (y' :> Descriptor.java_type))
      | (`Array _), _ -> `Class java_lang_Object_name
      | _, (`Array _) -> `Class java_lang_Object_name
      | (`Class x'), (`Class y') -> `Class (f x' y')
      | `Boolean, `Boolean -> `Boolean
      | `Byte, `Byte -> `Byte
      | `Char, `Char -> `Char
      | `Double, `Double -> `Double
      | `Float, `Float -> `Float
      | `Int, `Int -> `Int
      | `Long, `Long -> `Long
      | `Short, `Short -> `Short
      | _ -> raise Not_found in
    try
      (match ua (x :> Descriptor.java_type) (y :> Descriptor.java_type) with
        | `Array x -> `Array_type (`Array x)
        | _ -> java_lang_Object)
    with Not_found -> java_lang_Object

  let make_unifier f x y =
    let array_unifier = make_array_unifier f in
    match x, y with
      | (`Array_type at1), (`Array_type at2) -> array_unifier at1 at2
      | (`Class_or_interface cn1), (`Class_or_interface cn2) -> `Class_or_interface (f cn1 cn2)
      | _ -> java_lang_Object

  let unify_to_java_lang_Object =
    let utjlo x y =
      if Name.equal_for_class x y then
        x
      else
        java_lang_Object_name in
    make_unifier utjlo

  let unify_to_closest_common_parent cl l =
    let rec parents cn =
      let hd, prn =
        try
          let c, p = List.find (fun (x, _) -> Name.equal_for_class cn x) l in
          (Name.internal_utf8_for_class c), p
        with Not_found ->
          let cd = ClassLoader.find_class cl (Name.external_utf8_for_class cn) in
          (Name.internal_utf8_for_class cd.ClassDefinition.name),
          cd.ClassDefinition.extends in
      let tl = match prn with
      | Some x -> parents x
      | None -> [] in
      hd :: tl in
    let rec common_parent l = function
    | hd :: tl -> if List.exists (U.UTF8.equal hd) l then hd else common_parent l tl
    | [] -> U.UTF8.of_string "java/lang/Object" in
    let utccp x y =
      let parents_x = parents x in
      let parents_y = parents y in
      Name.make_for_class_from_internal (common_parent parents_x parents_y) in
      make_unifier utccp

  let unify_to_parent_list l =
    let rec parents cn =
      let hd, prn =
        try
          let c, p = List.find (fun (x, _) -> Name.equal_for_class cn x) l in
          (Name.internal_utf8_for_class c), p
        with Not_found ->
          (Name.internal_utf8_for_class cn), None in
      let tl = match prn with
      | Some x -> parents x
      | None -> [] in
      hd :: tl in
    let rec common_parent l = function
    | hd :: tl -> if List.exists (U.UTF8.equal hd) l then hd else common_parent l tl
    | [] -> U.UTF8.of_string "java/lang/Object" in
    let utccp x y =
      let parents_x = parents x in
      let parents_y = parents y in
      Name.make_for_class_from_internal (common_parent parents_x parents_y) in
      make_unifier utccp

  let unify f st1 st2 =
    let unify_elements vti1 vti2 =
      match (vti1, vti2) with
      | VI_top, _
      | _, VI_top -> VI_top
      | (VI_object o1), (VI_object o2) -> VI_object (f o1 o2)
      | VI_null, (VI_object _) -> vti2
      | (VI_object _), VI_null -> vti1
      | (VI_return_address l1), (VI_return_address l2) -> VI_return_address (LS.union l1 l2)
      | _ -> if vti1 = vti2 then vti1 else VI_top in
    let sz1 = List.length st1.stack in
    let sz2 = List.length st2.stack in
    if sz1 = sz2 then begin
      let locals =
        U.IntMap.merge
        (fun _ o1 o2 -> match o1, o2 with
        | Some s1, Some s2 -> Some (unify_elements s1 s2)
        | Some _, None | None, Some _ -> Some VI_top
        | None, None -> None)
        st1.locals st2.locals in
      let stack = List.map2 unify_elements st1.stack st2.stack in
      { locals; stack }
    end else
      fail (SE_different_stack_sizes (sz1, sz2))
  (* }}} *)

  (* TODO(rlp): it appears he pads the stack for big values *)
  (* our symbolic execution should be size agnostic and not need that *)
  let of_list l =
    let l =
      List.map
	(function
          | VI_long ->
            [VI_long; VI_top]
          | VI_double ->
            [VI_double; VI_top]
          | x -> [x])
	l in
    let l = List.concat l in
    let fold (m, i) v = (U.IntMap.add i v m, succ i) in
    let (m, _) = List.fold_left fold (U.IntMap.empty, 0) l in
    m

  let locals_of_method m = of_list (match m with
    | HM.Regular { HM.flags; descriptor; _ } ->
        let l = fst descriptor in
        let l = List.map verification_type_info_of_parameter_descriptor l in
        if List.mem `Static flags then
          l
        else
          (VI_object (`Class_or_interface java_lang_Class)) :: l
    | HM.Constructor { HM.cstr_descriptor = l ; _ } ->
        let l = List.map verification_type_info_of_parameter_descriptor l in
        VI_uninitialized_this :: l
    | HM.Initializer _ ->
        [])

  (* public *) (* {{{ *)
  let compute_max_stack_locals m c =
    let init = { locals = locals_of_method m; stack = [] } in
    let exec_throw s = { s with stack = [VI_object java_lang_Object] } in
    (* TODO(rlp) is this the correct unification? *)
    let unify_states = unify unify_to_java_lang_Object in
    let map_s = execute_method step exec_throw unify_states equal_t init c in
    let f l s (ms, ml) =
      (max ms (stack_size s), max ml (locals_size s)) in
    let max_stack, max_locals = HI.LabelHash.fold f map_s (0, 0) in
    (map_s, max_stack, max_locals)
    (* }}} *)
end (* }}} *)
module SE = SymbExe
module HighAttributeOps = struct (* {{{ *)
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
    IS.read_elements
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
    let module_index = IS.read_u2 st in
    let name_index, version_index =
      match CP.get_entry pool module_index with
      | CP.ModuleId (n, v) -> n, v
      | _ -> fail Invalid_module in
    let name = CP.get_utf8_entry pool name_index in
    let version = CP.get_utf8_entry pool version_index in
    (name, version)

  let read_info st =
    let name = IS.read_u2 st in
    let len = IS.read_u4 st in
    if (len :> int64) > (Int64.of_int max_int) then
      raise (IS.Exception IS.Data_is_too_large)
    else
      let dat = IS.read_bytes st (Int64.to_int (len :> int64)) in
      { A.name_index = name;
	length = len;
	data = dat; }

  let check_code_attribute = function
    | #HA.code_attribute as g -> g
    | b -> fail (Misplaced_attribute (HA.name_of_attribute b, "code"))

  let option_of_u2 f (i : U.u2) =
    if (i :> int) = 0 then None else Some (f i)

  (* actual decoders for attributes *)

  type decoding_arguments =
    { da_element : A.enclosing_element
    ; da_pool : CP.t
    ; da_i : A.info }

  let decode_attr_constant_value _ r st =
        let const_index = IS.read_u2 st in
        match CP.get_entry r.da_pool const_index with
        | CP.Long (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (HA.Long_value v)
        | CP.Float v ->
            `ConstantValue (HA.Float_value (Int32.float_of_bits v))
        | CP.Double (hi, lo) ->
            let v = Int64.logor (Int64.shift_left (Int64.of_int32 hi) 32) (Int64.of_int32 lo) in
            `ConstantValue (HA.Double_value (Int64.float_of_bits v))
        | CP.Integer v ->
            `ConstantValue (HA.Integer_value v)
        | CP.String idx ->
            `ConstantValue (HA.String_value (CP.get_utf8_entry r.da_pool idx))
        | _ -> fail Invalid_constant_value

  let decode_attr_code decode r st =
    (* read these anyway to get into the stream *)
    let (*mx_stack*) _ = IS.read_u2 st in
    let (*mx_locals*) _ = IS.read_u2 st in
    let code_len' = IS.read_u4 st in
    let code_len =
      let x = (code_len' : Utils.u4 :> int64) in
      if 0L < x && x < 65536L then
        Int64.to_int x
      else
        fail Invalid_code_length in
    let code_content = IS.read_bytes st code_len in
    let code_stream = IS.make_of_string code_content in
    let instr_codes = BC.read code_stream 0 in
    let ofs_to_label, ofs_to_prev_label =
      let rec f o ps = function (* note the sentinel added at the end *)
        | [] -> Array.of_list (List.rev ((o, HI.invalid_label) :: ps))
        | s :: ss -> f (o + BC.size_of o s) ((o, HI.fresh_label ()) :: ps) ss in
      let a = f 0 [] instr_codes in
      let n = Array.length a in
      assert (n > 1); (* see Invalid_code_length check above *)
      let rec bs o l h =
        if l + 1 = h then
          (if fst a.(l) = o then l else fail Invalid_offset)
        else begin
          let m = l + (h - l) / 2 in
          if fst a.(m) <= o then bs o m h else bs o l m
        end in
      ( (fun o -> snd a.(bs o 0 (pred n))),
        (fun o -> snd a.(pred (bs o 1 n))) ) in
    let instrs =
      let f (o, ss) s =
        (o + BC.size_of o s,
          (ofs_to_label o, HI.decode r.da_pool ofs_to_label o s) :: ss) in
      let _, r = List.fold_left f (0, []) instr_codes in
      List.rev r in
    let exceptions =
      IS.read_elements
        st
        (fun st ->
          let start_pc = IS.read_u2 st in
          let end_pc = IS.read_u2 st in
          let handler_pc = IS.read_u2 st in
          let catch_index = IS.read_u2 st in
          let catch_type =
            if (catch_index :> int) <> 0 then
              Some (CP.get_class_name r.da_pool catch_index)
            else
              None in
	  let u2_ofs_to_label ofs = ofs_to_label (ofs : Utils.u2 :> int) in
          { HA.try_start = u2_ofs_to_label start_pc;
            HA.try_end = ofs_to_prev_label (end_pc : Utils.u2 :> int);
            HA.catch = u2_ofs_to_label handler_pc;
            HA.caught = catch_type; }) in
    let attrs = IS.read_elements
        st
        (fun st -> decode { r with da_i = read_info st }) in
    let update_lnt = function
      | `LineNumberTable h ->
          let r = HI.LabelHash.create 13 in
          let f ofs ln =
            let label = ofs_to_label (Int64.to_int ofs) in
            assert (not (HI.LabelHash.mem r label));
            HI.LabelHash.add r label ln in
          HI.LabelHash.iter f h;
          `LineNumberTable r
      | x -> x in
    let attrs = List.map check_code_attribute attrs in
    let attrs = List.map update_lnt attrs in
    `Code { HA.code = instrs;
            HA.exception_table = exceptions;
            HA.attributes = attrs }

  let decode_attr_exceptions _ r st : HA.t =
    let f st = CP.get_class_name r.da_pool (IS.read_u2 st) in
    `Exceptions (IS.read_elements st f)

  let decode_attr_inner_classes _ r st : HA.t =
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
      { HA.inner_class; HA.outer_class; HA.inner_name; HA.inner_flags } in
    `InnerClasses (IS.read_elements st one)

  let decode_attr_enclosing_method _ r st : HA.t =
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
    `EnclosingMethod { HA.innermost_class; HA.enclosing_method }

  let decode_attr_synthetic _ _ _ : HA.t =
    `Synthetic

  let decode_attr_signature _ r st : HA.t =
    let signature_index = IS.read_u2 st in
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
  (* TODO(rgrig): The casts between in int and int64 are ugly and must go.*)
  let decode_attr_line_number_table _ _ st =
    let h = IS.read_elements
      st
      (fun st ->
        let start_pc = IS.read_u2 st in
        let line_number = IS.read_u2 st in
        (Int64.of_int (start_pc :> int), (line_number :> int))) in
    let h = hash_of_list HI.LabelHash.create HI.LabelHash.replace h in
    `LineNumberTable h

  let decode_attr_local_variable_table _ _ _ =
    (* See TODO note on type code_attribute. *)
    `LocalVariableTable ()

  let decode_attr_local_variable_type_table _ _ _ =
    (* See TODO note on type code_attribute. *)
    `LocalVariableTypeTable ()

  let decode_attr_deprecated _ _ _ : HA.t = `Deprecated

  let decode_attr_runtime_visible_annotations _ r st : HA.t =
    `RuntimeVisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_invisible_annotations _ r st : HA.t =
    `RuntimeInvisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_visible_parameter_annotations _ r st : HA.t =
    `RuntimeVisibleParameterAnnotations (read_annotations_list r.da_pool st)
  let decode_attr_runtime_invisible_parameter_annotations _ r st : HA.t =
    `RuntimeInvisibleParameterAnnotations (read_annotations_list r.da_pool st)

  let decode_attr_runtime_visible_type_annotations _ = failwith "todo:decode_attr_runtime_visible_type_annotations"
  let decode_attr_runtime_invisible_type_annotations _ = failwith "todo:decode_attr_runtime_invisible_type_annotations"

  let decode_attr_annotation_default _ r st : HA.t =
    let eiv = Annotation.read_info_element_value st in
    `AnnotationDefault (Annotation.decode_element_value r.da_pool eiv)

  let decode_attr_bootstrap_methods _ = failwith "todo:decode_attr_bootstrap_methods"
  let decode_attr_module _ = failwith "todo:decode_attr_module"
  let decode_attr_module_requires _ = failwith "todo:decode_attr_module_requires"
  let decode_attr_module_permits _ = failwith "todo:decode_attr_module_permits"
  let decode_attr_module_provides _ = failwith "todo:decode_attr_module_provides"

  module UTF8Hashtbl = Hashtbl.Make (Utils.UTF8)

  let decoders :
      ((decoding_arguments -> HA.t) ->
        decoding_arguments -> IS.t -> HA.t)
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
      | #HA.for_class as g -> g
      | b -> fail (Misplaced_attribute (HA.name_of_attribute b, "class"))

  let decode_field da_pool da_i =
    match decode { da_element = A.Field; da_pool; da_i } with
      | #HA.for_field as g -> g
      | b -> fail (Misplaced_attribute (HA.name_of_attribute b, "field"))

  let decode_method da_pool da_i =
    match decode { da_element = A.Method; da_pool; da_i } with
      | #HA.for_method as g -> g
      | b -> fail (Misplaced_attribute (HA.name_of_attribute b, "method"))

  let rec version_bounds : HA.t -> Version.bounds = function
    | `AnnotationDefault _ ->
        Version.make_bounds "'AnnotationDefault' attribute" Version.Java_1_5 None
    | `BootstrapMethods _ ->
        Version.make_bounds "'BootstrapMethods' attribute" Version.Java_1_7 None
    | `ClassSignature _ ->
        Version.make_bounds "'ClassSignature' attribute" Version.Java_1_5 None
    | `Code cv ->
        let instrs_bounds = List.map HI.version_bounds cv.HA.code in
        let attrs_bounds = List.map version_bounds (cv.HA.attributes :> HA.t list) in
        Version.intersect_list (instrs_bounds @ attrs_bounds)
    | `ConstantValue _ ->
        Version.make_bounds "'ConstantValue' attribute" Version.Java_1_0 None
    | `Deprecated ->
        Version.make_bounds "'Deprecated' attribute" Version.Java_1_1 None
    | `EnclosingMethod _ ->
        Version.make_bounds "'EnclosingMethod' attribute" Version.Java_1_5 None
    | `Exceptions _ ->
        Version.make_bounds "'Exceptions' attribute" Version.Java_1_0 None
    | `FieldSignature _ ->
        Version.make_bounds "'Signature' attribute" Version.Java_1_5 None
    | `InnerClasses _ ->
        Version.make_bounds "'InnerClasses' attribute" Version.Java_1_1 None
    | `LineNumberTable _ ->
        Version.make_bounds "'LineNumberTable' attribute" Version.Java_1_0 None
    | `LocalVariableTable _ ->
        Version.make_bounds "'LocalVariableTable' attribute" Version.Java_1_0 None
    | `LocalVariableTypeTable _ ->
        Version.make_bounds "'LocalVariableTypeTable' attribute" Version.Java_1_5 None
    | `MethodSignature _ ->
        Version.make_bounds "'MethodSignature' attribute" Version.Java_1_5 None
    | `Module _ ->
        Version.make_bounds "'Module' attribute" Version.Java_1_8 None
    | `RuntimeInvisibleAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeInvisibleParameterAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleParameterAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeInvisibleTypeAnnotations _ ->
        Version.make_bounds "'RuntimeInvisibleTypeAnnotations' attribute" Version.Java_1_7 None
    | `RuntimeVisibleAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeVisibleParameterAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleParameterAnnotations' attribute" Version.Java_1_5 None
    | `RuntimeVisibleTypeAnnotations _ ->
        Version.make_bounds "'RuntimeVisibleTypeAnnotations' attribute" Version.Java_1_7 None
    | `SourceDebugExtension _ ->
        Version.make_bounds "'SourceDebugExtension' attribute" Version.Java_1_5 None
    | `SourceFile _ ->
        Version.make_bounds "'SourceFile' attribute" Version.Java_1_0 None
    | `Synthetic ->
        Version.make_bounds "'Synthetic' attribute" Version.Java_1_1 None
    | `Unknown _ ->
        Version.make_bounds "'Unknown' attribute" Version.Java_1_0 None

  type encoder = { en_pool : CP.extendable
		 ; en_buffer : Buffer.t
		 ; en_st : OS.t }

  let make_encoder pool n =
    let buffer = Buffer.create n in
    let st = OS.make_of_buffer buffer in
    { en_pool = pool; en_buffer = buffer; en_st = st }

  let enc_return enc n =
    let name_idx = CP.add_utf8 enc.en_pool n in
    let content = Buffer.contents enc.en_buffer in
    { A.name_index = name_idx;
      length = U.u4 (Int64.of_int (String.length content));
      data = content; }

  let write_info enc i =
    OS.write_u2 enc.en_st i.Attribute.name_index;
    OS.write_u4 enc.en_st i.Attribute.length;
    OS.write_bytes enc.en_st i.Attribute.data

  let write_annotations enc l =
    OS.write_elements
      (checked_length "annotations")
      enc.en_st
      (fun st a ->
        let a' = Annotation.encode enc.en_pool a in
        Annotation.write_info st a')
      l

  let write_annotations_list enc l =
    let len = checked_length_u1 "annotation lists" l in
    OS.write_u1 enc.en_st len;
    List.iter
      (fun l' ->
        OS.write_elements
          (checked_length "annotations")
          enc.en_st
          (fun st a ->
            let a' = Annotation.encode enc.en_pool a in
            Annotation.write_info st a')
          l')
      l

  let write_extended_annotations enc l =
    OS.write_elements
      (checked_length "extended annotations")
      enc.en_st
      (fun st a ->
        let a' = Annotation.encode_extended enc.en_pool a in
        Annotation.write_extended_info st a')
      l

  let encode_attr_annotation_default enc ev =
      let eiv = Annotation.encode_element_value enc.en_pool ev in
      Annotation.write_info_element_value enc.en_st eiv;
      enc_return enc attr_annotation_default

  let encode_attr_bootstrap_methods _ = failwith "todo   let encode_attr_bootstrap_methods _ = " (* not decoded yet *)
  let encode_attr_class_signature enc s =
      let idx = CP.add_utf8 enc.en_pool (Signature.utf8_of_class_signature s) in
      OS.write_u2 enc.en_st idx;
      enc_return enc attr_signature

  let i_list_to_labeled_bc_list m pool l =
    let fold (bcl, ofs) (lbl, i) =
      let bc = HI.encode m ofs pool i in
      ((lbl, bc)::bcl, ofs + (BC.size_of ofs bc)) in
    let bcl, _ = List.fold_left fold ([], 0) l in
    List.rev bcl

  let compute_ofs_map bcl =
    let m = HI.LabelHash.create 131 in
    let fold ofs (lbl, bc) =
      (* could check for clashes here *)
      HI.LabelHash.add m lbl ofs;
      ofs + (BC.size_of ofs bc) in
    ignore (List.fold_left fold 0 bcl);
    m

  let encode_instr_list pool l =
    let dummy_map _ = 0 in
    let rec fix_map m bcl =
      let m' = compute_ofs_map bcl in
      let same =
        let f k v same = same && m k = v in
        HI.LabelHash.fold f m' true in
      if same then (m, List.map snd bcl)
      else
        let m' = HI.LabelHash.find m' in
	let bcl' = i_list_to_labeled_bc_list m' pool l in
	fix_map m' bcl' in
    let bcl = i_list_to_labeled_bc_list dummy_map pool l in
    fix_map dummy_map bcl

  let rec encode_attr_code m enc encode c =
    let m = match m with
      | Some m -> m
      | None -> failwith "INTERNAL: encode_attr_code" in
    let label_to_ofs, code_content = encode_instr_list enc.en_pool c.HA.code in
    let code_enc = make_encoder enc.en_pool 16 in
    BC.write code_enc.en_st 0 code_content;
    OS.close code_enc.en_st;
    let actual_code = Buffer.contents code_enc.en_buffer in
    (* TODO: write the stackmap at some point *)
    let stackmap, max_stack, max_locals = SE.compute_max_stack_locals m c in
    OS.write_u2 enc.en_st (U.u2 max_stack);
    OS.write_u2 enc.en_st (U.u2 max_locals);
    let code_length = String.length actual_code in
    if code_length > U.max_u2 then fail Invalid_code_length;
    OS.write_u4 enc.en_st (U.u4 (Int64.of_int code_length));
    OS.write_bytes enc.en_st actual_code;
    OS.write_elements
      (checked_length "Exceptions")
      enc.en_st
      (fun st elem ->
        let catch_idx = match elem.HA.caught with
        | Some exn_name -> CP.add_class enc.en_pool exn_name
        | None -> U.u2 0 in
        OS.write_u2 st (U.u2 (label_to_ofs elem.HA.try_start));
        OS.write_u2 st (U.u2 (label_to_ofs elem.HA.try_end));
        OS.write_u2 st (U.u2 (label_to_ofs elem.HA.catch));
        OS.write_u2 st catch_idx)
      c.HA.exception_table;
    let len' = checked_length "Attributes" c.HA.attributes in
    OS.write_u2 enc.en_st len';
    let sub_enc = make_encoder enc.en_pool 16 in
    List.iter
      (fun a ->
        let res = encode (Some m) sub_enc.en_pool (a :> HA.t) in
        write_info sub_enc res)
      c.HA.attributes;
    OS.close sub_enc.en_st;
    OS.write_bytes enc.en_st (Buffer.contents sub_enc.en_buffer);
    enc_return enc attr_code

  let encode_attr_constant_value enc = function
      | HA.Boolean_value b ->
          let idx = CP.add_integer enc.en_pool (if b then 1l else 0l) in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.Byte_value v | HA.Character_value v | HA.Short_value v ->
          let idx = CP.add_integer enc.en_pool (Int32.of_int v) in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.Double_value d ->
          let idx = CP.add_double enc.en_pool d in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.Float_value f ->
          let idx = CP.add_float enc.en_pool f in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.Integer_value i ->
          let idx = CP.add_integer enc.en_pool i in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.Long_value l ->
          let idx = CP.add_long enc.en_pool l in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value
      | HA.String_value s ->
          let idx = CP.add_string enc.en_pool s in
          OS.write_u2 enc.en_st idx;
          enc_return enc attr_constant_value

  let encode_attr_deprecated enc = enc_return enc attr_deprecated

  let encode_attr_enclosing_method enc { HA.innermost_class; HA.enclosing_method } =
      let class_idx = CP.add_class enc.en_pool innermost_class in
      let meth_idx = match enclosing_method with
      | Some (n, d) ->
          CP.add_name_and_type enc.en_pool
            (Name.utf8_for_method n)
            (Descriptor.utf8_of_method d)
      | None -> U.u2 0 in
      OS.write_u2 enc.en_st class_idx;
      OS.write_u2 enc.en_st meth_idx;
      enc_return enc attr_enclosing_method

  let encode_attr_exceptions enc l =
      OS.write_elements
        (checked_length "exceptions")
        enc.en_st
        (fun st s ->
          let idx = CP.add_class enc.en_pool s in
          OS.write_u2 st idx)
        l;
      enc_return enc attr_exceptions

  let encode_attr_field_signature enc s =
      let idx = CP.add_utf8 enc.en_pool (Signature.utf8_of_field_type_signature s) in
      OS.write_u2 enc.en_st idx;
      enc_return enc attr_signature

  let encode_attr_inner_classes enc l =
      OutputStream.write_elements
        (checked_length "inner classes")
        enc.en_st
        (fun st { HA.inner_class; HA.outer_class; HA.inner_name; HA.inner_flags } ->
          let inner_idx = match inner_class with
          | None -> U.u2 0
          | Some c -> CP.add_class enc.en_pool c in
          let outer_idx = match outer_class with
          | None -> U.u2 0
          | Some c -> CP.add_class enc.en_pool c in
          let name_idx = match inner_name with
          | None -> U.u2 0
          | Some c -> CP.add_utf8 enc.en_pool c in
          let fl = AccessFlag.list_to_u2 (inner_flags :> AccessFlag.t list) in
          OutputStream.write_u2 enc.en_st inner_idx;
          OutputStream.write_u2 enc.en_st outer_idx;
          OutputStream.write_u2 enc.en_st name_idx;
          OutputStream.write_u2 enc.en_st fl)
        l;
      enc_return enc attr_inner_classes

  let write_empty_list msg attr enc =
    OS.write_elements
      (checked_length msg)
      enc.en_st
      (fun _ _ -> ())
      [];
    enc_return enc attr

  (* TODO(rgrig): Implement. *)
  let encode_attr_line_number_table enc _ =
    write_empty_list "line numbers" attr_line_number_table enc

  let encode_attr_local_variable_table =
    write_empty_list "local variables" attr_local_variable_table

  let encode_attr_local_variable_type_table =
    write_empty_list "local variable types" attr_local_variable_type_table

  let encode_attr_method_signature enc s =
      let idx = CP.add_utf8 enc.en_pool (Signature.utf8_of_method_signature s) in
      OS.write_u2 enc.en_st idx;
      enc_return enc attr_signature

  let encode_attr_module _ = failwith "todo   let encode_attr_module _ = " (* not decoded yet *)
  let encode_attr_runtime_invisible_annotations enc l =
      write_annotations enc l;
      enc_return enc attr_runtime_invisible_annotations

  let encode_attr_runtime_invisible_parameter_annotations enc l =
      write_annotations_list enc l;
      enc_return enc attr_runtime_invisible_parameter_annotations

  let encode_attr_runtime_invisible_type_annotations enc l = (* not decoded yet *)
      write_extended_annotations enc l;
      enc_return enc attr_runtime_invisible_type_annotations

  let encode_attr_runtime_visible_annotations enc l =
      write_annotations enc l;
      enc_return enc attr_runtime_visible_type_annotations

  let encode_attr_runtime_visible_parameter_annotations enc l =
      write_annotations_list enc l;
      enc_return enc attr_runtime_visible_parameter_annotations

  let encode_attr_runtime_visible_type_annotations enc l = (* not decoded yet *)
      write_extended_annotations enc l;
      enc_return enc attr_runtime_invisible_type_annotations

  let encode_attr_source_debug_extension enc sde = (* not decoded yet *)
      let bytes = U.UTF8.bytes_of_modified (U.UTF8.to_modified sde) in
      Buffer.add_string enc.en_buffer bytes;
      enc_return enc attr_source_debug_extension

  let encode_attr_source_file enc sf =
      let idx = CP.add_utf8 enc.en_pool sf in
      OS.write_u2 enc.en_st idx;
      enc_return enc attr_source_file

  let encode_attr_synthetic enc = enc_return enc attr_synthetic
  let encode_attr_unknown enc (n, v) =
      Buffer.add_string enc.en_buffer v;
      enc_return enc n

  let rec encode m pool : HA.t -> A.info =
    let enc = make_encoder pool 64 in
  function
    | `AnnotationDefault ev -> encode_attr_annotation_default enc ev
    | `BootstrapMethods _ -> encode_attr_bootstrap_methods ()
    | `ClassSignature s -> encode_attr_class_signature enc s
    | `Code c -> encode_attr_code m enc encode c
    | `ConstantValue v -> encode_attr_constant_value enc v
    | `Deprecated -> encode_attr_deprecated enc
    | `EnclosingMethod em -> encode_attr_enclosing_method enc em
    | `Exceptions l -> encode_attr_exceptions enc l
    | `FieldSignature s -> encode_attr_field_signature enc s
    | `InnerClasses l -> encode_attr_inner_classes enc l
    | `LineNumberTable h -> encode_attr_line_number_table enc h
    | `LocalVariableTable _ -> encode_attr_local_variable_table enc
    | `LocalVariableTypeTable _ -> encode_attr_local_variable_type_table enc
    | `MethodSignature s -> encode_attr_method_signature enc s
    | `Module _ -> encode_attr_module ()
    | `RuntimeInvisibleAnnotations l -> encode_attr_runtime_invisible_annotations enc l
    | `RuntimeInvisibleParameterAnnotations l -> encode_attr_runtime_invisible_parameter_annotations enc l
    | `RuntimeInvisibleTypeAnnotations l -> encode_attr_runtime_invisible_type_annotations enc l
    | `RuntimeVisibleAnnotations l -> encode_attr_runtime_visible_annotations enc l
    | `RuntimeVisibleParameterAnnotations l -> encode_attr_runtime_visible_parameter_annotations enc l
    | `RuntimeVisibleTypeAnnotations l -> encode_attr_runtime_visible_type_annotations enc l
    | `SourceDebugExtension sde -> encode_attr_source_debug_extension enc sde
    | `SourceFile sf -> encode_attr_source_file enc sf
    | `Synthetic -> encode_attr_synthetic enc
    | `Unknown u -> encode_attr_unknown enc u

  let encode_class pool a = encode None pool (a : HA.for_class :> HA.t)
  let encode_field pool a = encode None pool (a : HA.for_field :> HA.t)
  let encode_method m pool a =
    if log_se then printf "@\n@[<2>digraph %s {" (HM.to_string m);
    let r = encode (Some m) pool (a : HA.for_method :> HA.t) in
    if log_se then printf "@]@\n}";
    r
end
(* }}} *)
module HAO = HighAttributeOps
module HighMethodOps = struct (* {{{ *)
  let decode_initializer init_attributes flags _ =
    let init_flags = AccessFlag.check_initializer_flags flags in
    HM.Initializer { HM.init_flags; HM.init_attributes }

  let decode_constructor (cstr_descriptor,_) cstr_attributes flags _ =
    let cstr_flags = AccessFlag.check_constructor_flags flags in
    HM.Constructor { HM.cstr_flags; HM.cstr_descriptor; HM.cstr_attributes }

  let decode_regular i name descriptor attributes flags _ =
    let flags = AccessFlag.check_method_flags i flags in
    HM.Regular { HM.flags; HM.name; HM.descriptor; HM.attributes }

  let decode is_interface pool m =
    let utf8 = CP.get_utf8_entry pool in (* (local) short name *)
    let utf8_name = utf8 m.M.name_index in
    let name = Name.make_for_method utf8_name in
    let descriptor =
      Descriptor.method_of_utf8 (utf8 m.M.descriptor_index) in
    let attributes =
      U.map_array_to_list (HAO.decode_method pool) m.M.attributes_array in
    let flags = AccessFlag.from_u2 true m.M.access_flags in
    U.switch U.UTF8.equal
      [ class_initializer, decode_initializer attributes flags
      ; class_constructor, decode_constructor descriptor attributes flags ]
      (decode_regular is_interface name descriptor attributes flags)
      utf8_name

  let encode pool m =
    let flags, name, desc, attrs = match m with
      | HM.Regular r ->
	r.HM.flags,
	r.HM.name,
	r.HM.descriptor,
	r.HM.attributes
      | HM.Constructor c ->
	(c.HM.cstr_flags :> AF.for_method list),
	(Name.make_for_method class_constructor),
	(c.HM.cstr_descriptor, `Void),
	c.HM.cstr_attributes
      | HM.Initializer i ->
	(i.HM.init_flags :> AF.for_method list),
	(Name.make_for_method class_initializer),
	([], `Void),
	i.HM.init_attributes in
    let acc_flags = AF.list_to_u2 (flags :> AF.t list) in
    let name_idx = CP.add_utf8 pool (Name.utf8_for_method name) in
    let desc_utf8 = Descriptor.utf8_of_method desc in
    let desc_idx = CP.add_utf8 pool desc_utf8 in
    { M.access_flags = acc_flags;
      name_index = name_idx;
      descriptor_index = desc_idx;
      attributes_count = U.u2 (List.length attrs);
      attributes_array = U.map_list_to_array (HAO.encode_method m pool) (attrs :> HA.for_method list); }
end (* }}} *)
module HMO = HighMethodOps
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
      U.map_array_to_list (HAO.decode_field pool) i.F.attributes_array in
    { flags; name; descriptor; attributes }

  let encode pool f =
    let access_flags = AF.list_to_u2 (f.flags :> AccessFlag.t list) in
    let name_index = CP.add_utf8 pool (Name.utf8_for_field f.name) in
    let desc_utf8 = Descriptor.utf8_of_field f.descriptor in
    let descriptor_index = CP.add_utf8 pool desc_utf8 in
    let attributes_count = U.u2 (List.length f.attributes) in
    let attributes_array =
      U.map_list_to_array (HAO.encode None pool) (f.attributes :> HA.t list) in
    { F.access_flags; name_index; descriptor_index; attributes_count
    ; attributes_array }

end (* }}} *)
module HF = HighField

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
  let check_attribute x = Version.check (HAO.version_bounds x) version in
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

let decode cf =
  let pool = cf.CF.constant_pool in
  let version = cf.CF.major_version, cf.CF.minor_version in
  let version = Version.version_of_major_minor version in
  CP.check_version version pool;
  let flags = AF.check_class_flags (AF.from_u2 false cf.CF.access_flags) in
  let class_name = CP.get_class_name pool in
  let extends =
    if cf.CF.super_class = U.u2 0
    then None
    else Some (class_name cf.ClassFile.super_class) in
  let is_interface = List.mem `Interface flags in
  let field_decode = HF.decode is_interface pool in
  let method_decode = HMO.decode is_interface pool in
  let attribute_decode = HAO.decode_class pool in
  let hc = check_version_high ~version { access_flags = flags;
    name = class_name cf.CF.this_class;
    extends = extends;
    implements = List.map class_name (Array.to_list cf.CF.interfaces);
    fields = List.map field_decode (Array.to_list cf.CF.fields);
    methods = List.map method_decode (Array.to_list cf.CF.methods);
    attributes = List.map attribute_decode (Array.to_list cf.CF.attributes); }
  in (hc, version)

(* TODO should this be in consts? *)
let no_super_class = U.u2 0

let encode (cd, version) =
  ignore (check_version_high ~version:version cd);
  let major, minor = Version.major_minor_of_version version in
  let pool = CP.make_extendable () in
  let this_index = CP.add_class pool cd.name in
  let super_index = match cd.extends with
    | Some n -> CP.add_class pool n
    | None -> no_super_class in
  let itfs = U.map_list_to_array (fun s -> CP.add_class pool s) cd.implements in
  let flds = U.map_list_to_array (HF.encode pool) cd.fields in
  let mths = U.map_list_to_array (HMO.encode pool) cd.methods in
  let atts = U.map_list_to_array (HAO.encode_class pool) cd.attributes in
  let cpool = CP.to_pool pool in
  let checked_number s sz =
    if sz <= U.max_u2 then
      U.u2 sz
    else
      fail (Too_many s) in
  let checked_length_array s arr =
    let res = Array.length arr in
    checked_number s res in
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
