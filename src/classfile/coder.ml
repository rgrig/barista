(* open modules *) (* {{{ *)
open Consts
open Debug
open Format
(* }}} *)
(* module shorthands *) (* {{{ *)
module A = Attribute
module M = Method
module AF = AccessFlag
module BC = ByteCode
module CF = ClassFile
module CP = ConstantPool
module IS = InputStream
module OS = OutputStream
module U = Utils
module T = HighTypes

(* }}} *)
(* helpers *) (* {{{ *)
type enclosing_element =
  | EE_class
  | EE_method of (Name.for_class * T.method_)
  | EE_field

let fail e = raise (T.Exception e)

let string_of_error = function
  | T.Invalid_TABLESWITCH -> "TABLESWITCH with empty range [low..high]"
  | T.Invalid_offset -> "offset does not point at start of instruction"
  | T.Invalid_pool_element -> "unexpected element type in the constant pool"
  | T.Invalid_stack_map_table -> "invalid stack map table"
  | T.Misplaced_attribute (a, e) -> "attribute " ^ a ^ " appears on " ^ e
  | T.SE_array_expected s -> "SE: found " ^ s ^ " where an array was expected"
  | T.SE_unexpected_size (s, x) -> "SE: found " ^ x ^ " where a value of size " ^  (string_of_int s) ^ " was expected"
  | T.SE_different_stack_sizes (s, s') -> "SE: saw a stack with size " ^ (string_of_int s) ^ " but expected one with size " ^ (string_of_int s')
  | T.SE_double_new -> "SE: NEW instruction executed again without an interposing <init>"
  | T.SE_empty_stack -> "SE: pop from empty stack during symbolic execution"
  | T.SE_invalid_label -> "SE: jump to inexistent label"
  | T.SE_invalid_local_contents (i, s, s') -> "SE: index " ^ (string_of_int i) ^ " contains " ^ s' ^ " but " ^ s ^ " was expected"
  | T.SE_uninitialized_register (i, len) -> "SE: requesting uninitialized register " ^ (string_of_int i) ^ " in a pool of size " ^ (string_of_int len)
  | T.SE_invalid_stack_top (s, s') -> "SE: top of stack is " ^ s' ^ " but " ^ s ^ " was expected"
  | T.SE_missing_return -> "SE: no return at end of method"
  | T.SE_reference_expected s -> "SE: found " ^ s ^ " where a reference was expected"
  | T.Too_many s -> "number of " ^ s ^ " exceeds " ^ (string_of_int (U.max_u2 :> int))
  | T.Unsupported s -> "unsupported " ^ s
  | _ -> "undescribed error (todo)"

let checked_length s l =
  let res = List.length l in
  if res <= U.max_u2 then
    U.u2 res
  else
    fail (T.Too_many s)

let checked_length_u1 s l =
  let res = List.length l in
  if res <= U.max_u1 then
    U.u1 res
  else
    fail (T.Too_many s)

let fresh_label = U.fresh ()

let size_of_bt = function T.Double | T.Long -> 2 | _ -> 1

let pp_label_set pe f = T.LabelSet.iter (fun e -> fprintf f "@ %a" pe e)

let pp_bt f = function
  | T.Top -> fprintf f "top"
  | T.Integer -> fprintf f "integer"
  | T.Float -> fprintf f "float"
  | T.Long -> fprintf f "long"
  | T.Double -> fprintf f "double"
  | T.Null -> fprintf f "null"
  | T.Uninitialized_this -> fprintf f "uninitialized_this"
  | T.Object _ -> fprintf f "object"
  | T.Uninitialized l -> fprintf f "uninitialized(%Ld)" l
  | T.Return_address l -> fprintf f "return_address [%a]" (pp_label_set (fun f a -> fprintf f "%Ld" a)) l
  | T.Reference -> fprintf f "reference"

let string_of_bt =
  U.string_of_pp pp_bt

let bt_of_descriptor = function
  | `Boolean -> T.Integer
  | `Byte -> T.Integer
  | `Char -> T.Integer
  | `Double -> T.Double
  | `Float -> T.Float
  | `Int -> T.Integer
  | `Long -> T.Long
  | `Short -> T.Integer
  | `Class cn -> T.Object (`Class_or_interface cn)
  | `Array e -> T.Object (`Array_type (`Array e))

(* make types less precise, by mapping to the subset of types that correspond
to instruction types *)
let fuzzy_bt = function
  | T.Integer -> T.Integer
  | T.Float -> T.Float
  | T.Long -> T.Long
  | T.Double -> T.Double
  | T.Null
  | T.Uninitialized_this
  | T.Reference
  | T.Object _
  | T.Uninitialized _
  | T.Return_address _ -> T.Reference
  | _ -> failwith "INTERNAL: Already *too* fuzzy."

let java_lang_Class = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.Class")
let java_lang_Object_name = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.Object")
let java_lang_Object = `Class_or_interface java_lang_Object_name


let locals_of_method c_name m = match m with
  | T.RegularMethod { T.rm_flags; rm_descriptor; _ } ->
      let l = fst rm_descriptor in
      let l = List.map bt_of_descriptor l in
      if List.mem `Static rm_flags then
        l
      else
        (T.Object (`Class_or_interface c_name)) :: l
  | T.InitMethod { T.im_descriptor = l ; _ } ->
      let l = List.map bt_of_descriptor l in
      T.Uninitialized_this :: l
  | T.ClinitMethod _ ->
      []

(* }}} *)
(* HighInstruction, SymbExe, HighAttribute, HighField and HighMethod *) (* {{{ *)
module HighConstant = struct (* {{{ *)
  let rec decode pool i : T.constant =
    let utf8 = CP.get_utf8_entry pool in
    let d_class n =
      let s = utf8 n in
      if U.UChar.equal opening_square_bracket (U.UTF8.get s 0) then
        let t = Descriptor.java_type_of_internal_utf8 s in
        `Array_type (Descriptor.filter_non_array Descriptor.Invalid_array_element_type t)
      else
        `Class_or_interface (Name.make_for_class_from_internal s) in
    let d_nt nt = match CP.get_entry pool nt with
      | CP.NameAndType (n, t) -> (decode pool n, decode pool t)
      | _ -> fail T.Invalid_pool_element in
    let d_fieldref c nt = match decode pool c, d_nt nt with
      | `Class_or_interface c, (`String f, `String t) -> `Fieldref
          (c, Name.make_for_field f, Descriptor.field_of_utf8 t)
      | _ -> fail T.Invalid_pool_element in
    let d_methodref c nt =
      let c = decode_typeref pool c in
      match d_nt nt with
        | `String m, `String t -> `Methodref
            (c, Name.make_for_method m, Descriptor.method_of_utf8 t )
        | _ -> fail T.Invalid_pool_element in
    match CP.get_entry pool i with
      | CP.Class n -> d_class n
      | CP.Fieldref (c, nt) -> d_fieldref c nt
      | CP.Methodref (c, nt)
      | CP.InterfaceMethodref (c, nt) -> d_methodref c nt
      | CP.String s -> decode pool s
      | CP.Integer x -> `Int x
      | CP.Float f -> `Float (Int32.float_of_bits f)
      | CP.Long (hi, lo) -> `Long (U.i64_of_2i32 hi lo)
      | CP.Double (hi, lo) -> `Double (Int64.float_of_bits (U.i64_of_2i32 hi lo))
      | CP.NameAndType _ -> fail T.Invalid_pool_element
      | CP.UTF8 s -> `String s
      | CP.MethodHandle _ -> fail (T.Unsupported "dynamic binding of methods")
      | CP.MethodType _ -> fail (T.Unsupported "constant pool tag MethodType")
      | CP.InvokeDynamic _ -> fail (T.Unsupported "instruction INVOKEDYNAMIC")
      | CP.ModuleId _ -> fail (T.Unsupported "module system (jigsaw)")
  (* These don't do much work, but are convenient. *)
  and decode_classref pool i = match decode pool i with
    | #T.constant_classref as g -> g
    | _ -> fail T.Invalid_pool_element
  and decode_field pool i = match decode pool i with
    | #T.constant_field as g -> g
    | _ -> fail T.Invalid_pool_element
  and decode_fieldref pool i = match decode pool i with
    | #T.constant_fieldref as g -> g
    | _ -> fail T.Invalid_pool_element
  and decode_methodref pool i = match decode pool i with
    | #T.constant_methodref as g -> g
    | _ -> fail T.Invalid_pool_element
  and decode_stack pool i = match decode pool i with
    | #T.constant_stack as g -> g
    | _ -> fail T.Invalid_pool_element
  and decode_typeref pool i = match decode pool i with
    | #T.constant_typeref as g -> g
    | _ -> fail T.Invalid_pool_element

  let rec encode pool (e : T.constant) = match e with
    | #T.constant_methodref -> failwith "INTERNAL: You must use encode_methodref"
    | `Array_type a -> CP.add_array_class pool a
    | `Class_or_interface c -> CP.add_class pool c
    | `Double d -> CP.add_double pool d
    | `Fieldref (c, f, t) -> CP.add_field pool c f t
    | `Float f -> CP.add_float pool f
    | `Int i -> CP.add_integer pool i
    | `Long l -> CP.add_long pool l
    | `String s -> CP.add_string pool s
  let encode_methodref is_interface pool (c : T.constant_methodref) = match c with
    | `Methodref (`Class_or_interface c, m, t) ->
        (if is_interface then CP.add_interface_method else CP.add_method)
        pool c m t
    | `Methodref (`Array_type a, m, t) -> CP.add_array_method pool a m t

  let encode_field pool (c : T.constant_field) =
    encode pool (c :> T.constant)

  let encode_fieldref pool (c : T.constant_fieldref) =
    encode pool (c :> T.constant)

  let encode_stack pool (c : T.constant_stack) =
    encode pool (c :> T.constant)

  let encode_typeref pool (c : T.constant_typeref) =
    encode pool (c :> T.constant)

  let size : T.constant -> int = function `Double _ | `Long _ -> 2 | _ -> 1
end (* }}} *)
module HC = HighConstant
module HighInstruction = struct (* {{{ *)

  let invalid_label = -1L

  let s1_to_int (s : U.s1) = (s :> int)
  let s1_to_int32 (s : U.s1) = Int32.of_int (s :> int)
  let s2_to_int (s : U.s2) = (s :> int)
  let s4_to_int (s : U.s4) =
    let int_32 = (s :> int32) in
    let int_nat = Int32.to_int int_32 in
    if Int32.of_int int_nat <> int_32 then
      assert false; (* We should be using int32 if this fails. *)
    int_nat
  let s4_to_int32 (s : U.s4) = (s :> int32)

  let u1_to_int (u : U.u1) = (u :> int)
  let u2_to_int (u : U.u2) = (u :> int)

  let decode pool ofs_to_lbl ofs =
    let rel_s_ofs_to_lbl (s : U.s2) = ofs_to_lbl (ofs + (s :> int)) in
    let rel_l_ofs_to_lbl (s : U.s4) = ofs_to_lbl (ofs + s4_to_int s) in
    let primitive_array_type_of_int = function
      | 4 -> `Boolean
      | 5 -> `Char
      | 6 -> `Float
      | 7 -> `Double
      | 8 -> `Byte
      | 9 -> `Short
      | 10 -> `Int
      | 11 -> `Long
      | _ -> fail T.Invalid_primitive_array_type in
    let hi_NEW c = match HC.decode_classref pool c with
      | `Class_or_interface n -> T.NEW n in
    function
      | BC.AALOAD -> T.AALOAD
      | BC.AASTORE -> T.AASTORE
      | BC.ACONST_NULL -> T.ACONST_NULL
      | BC.ALOAD p1 -> T.ALOAD (u1_to_int p1)
      | BC.ALOAD_0 -> T.ALOAD 0
      | BC.ALOAD_1 -> T.ALOAD 1
      | BC.ALOAD_2 -> T.ALOAD 2
      | BC.ALOAD_3 -> T.ALOAD 3
      | BC.ANEWARRAY p1 -> T.ANEWARRAY (HC.decode_typeref pool p1)
      | BC.ARETURN -> T.ARETURN
      | BC.ARRAYLENGTH -> T.ARRAYLENGTH
      | BC.ASTORE p1 -> T.ASTORE (u1_to_int p1)
      | BC.ASTORE_0 -> T.ASTORE 0
      | BC.ASTORE_1 -> T.ASTORE 1
      | BC.ASTORE_2 -> T.ASTORE 2
      | BC.ASTORE_3 -> T.ASTORE 3
      | BC.ATHROW -> T.ATHROW
      | BC.BALOAD -> T.BALOAD
      | BC.BASTORE -> T.BASTORE
      | BC.BIPUSH p1 -> T.LDC (`Int (s1_to_int32 p1))
      | BC.CALOAD -> T.CALOAD
      | BC.CASTORE -> T.CASTORE
      | BC.CHECKCAST p1 -> T.CHECKCAST (HC.decode_typeref pool p1)
      | BC.D2F -> T.D2F
      | BC.D2I -> T.D2I
      | BC.D2L -> T.D2L
      | BC.DADD -> T.DADD
      | BC.DALOAD -> T.DALOAD
      | BC.DASTORE -> T.DASTORE
      | BC.DCMPG -> T.DCMPG
      | BC.DCMPL -> T.DCMPL
      | BC.DCONST_0 -> T.DCONST_0
      | BC.DCONST_1 -> T.DCONST_1
      | BC.DDIV -> T.DDIV
      | BC.DLOAD p1 -> T.DLOAD (u1_to_int p1)
      | BC.DLOAD_0 -> T.DLOAD 0
      | BC.DLOAD_1 -> T.DLOAD 1
      | BC.DLOAD_2 -> T.DLOAD 2
      | BC.DLOAD_3 -> T.DLOAD 3
      | BC.DMUL -> T.DMUL
      | BC.DNEG -> T.DNEG
      | BC.DREM -> T.DREM
      | BC.DRETURN -> T.DRETURN
      | BC.DSTORE p1 -> T.DSTORE (u1_to_int p1)
      | BC.DSTORE_0 -> T.DSTORE 0
      | BC.DSTORE_1 -> T.DSTORE 1
      | BC.DSTORE_2 -> T.DSTORE 2
      | BC.DSTORE_3 -> T.DSTORE 3
      | BC.DSUB -> T.DSUB
      | BC.DUP -> T.DUP
      | BC.DUP2 -> T.DUP2
      | BC.DUP2_X1 -> T.DUP2_X1
      | BC.DUP2_X2 -> T.DUP2_X2
      | BC.DUP_X1 -> T.DUP_X1
      | BC.DUP_X2 -> T.DUP_X2
      | BC.F2D -> T.F2D
      | BC.F2I -> T.F2I
      | BC.F2L -> T.F2L
      | BC.FADD -> T.FADD
      | BC.FALOAD -> T.FALOAD
      | BC.FASTORE -> T.FASTORE
      | BC.FCMPG -> T.FCMPG
      | BC.FCMPL -> T.FCMPL
      | BC.FCONST_0 -> T.FCONST_0
      | BC.FCONST_1 -> T.FCONST_1
      | BC.FCONST_2 -> T.FCONST_2
      | BC.FDIV -> T.FDIV
      | BC.FLOAD p1 -> T.FLOAD (u1_to_int p1)
      | BC.FLOAD_0 -> T.FLOAD 0
      | BC.FLOAD_1 -> T.FLOAD 1
      | BC.FLOAD_2 -> T.FLOAD 2
      | BC.FLOAD_3 -> T.FLOAD 3
      | BC.FMUL -> T.FMUL
      | BC.FNEG -> T.FNEG
      | BC.FREM -> T.FREM
      | BC.FRETURN -> T.FRETURN
      | BC.FSTORE p1 -> T.FSTORE (u1_to_int p1)
      | BC.FSTORE_0 -> T.FSTORE 0
      | BC.FSTORE_1 -> T.FSTORE 1
      | BC.FSTORE_2 -> T.FSTORE 2
      | BC.FSTORE_3 -> T.FSTORE 3
      | BC.FSUB -> T.FSUB
      | BC.GETFIELD p1 -> T.GETFIELD (HC.decode_fieldref pool p1)
      | BC.GETSTATIC p1 -> T.GETSTATIC (HC.decode_fieldref pool p1)
      | BC.GOTO p1 -> T.GOTO (rel_s_ofs_to_lbl p1)
      | BC.GOTO_W p1 -> T.GOTO (rel_l_ofs_to_lbl p1)
      | BC.I2B -> T.I2B
      | BC.I2C -> T.I2C
      | BC.I2D -> T.I2D
      | BC.I2F -> T.I2F
      | BC.I2L -> T.I2L
      | BC.I2S -> T.I2S
      | BC.IADD -> T.IADD
      | BC.IALOAD -> T.IALOAD
      | BC.IAND -> T.IAND
      | BC.IASTORE -> T.IASTORE
      | BC.ICONST_0 -> T.LDC (`Int 0l)
      | BC.ICONST_1 -> T.LDC (`Int 1l)
      | BC.ICONST_2 -> T.LDC (`Int 2l)
      | BC.ICONST_3 -> T.LDC (`Int 3l)
      | BC.ICONST_4 -> T.LDC (`Int 4l)
      | BC.ICONST_5 -> T.LDC (`Int 5l)
      | BC.ICONST_M1 -> T.LDC (`Int (-1l))
      | BC.IDIV -> T.IDIV
      | BC.IF_ACMPEQ p1 -> T.IF_ACMPEQ (rel_s_ofs_to_lbl p1)
      | BC.IF_ACMPNE p1 -> T.IF_ACMPNE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPEQ p1 -> T.IF_ICMPEQ (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPGE p1 -> T.IF_ICMPGE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPGT p1 -> T.IF_ICMPGT (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPLE p1 -> T.IF_ICMPLE (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPLT p1 -> T.IF_ICMPLT (rel_s_ofs_to_lbl p1)
      | BC.IF_ICMPNE p1 -> T.IF_ICMPNE (rel_s_ofs_to_lbl p1)
      | BC.IFEQ p1 -> T.IFEQ (rel_s_ofs_to_lbl p1)
      | BC.IFGE p1 -> T.IFGE (rel_s_ofs_to_lbl p1)
      | BC.IFGT p1 -> T.IFGT (rel_s_ofs_to_lbl p1)
      | BC.IFLE p1 -> T.IFLE (rel_s_ofs_to_lbl p1)
      | BC.IFLT p1 -> T.IFLT (rel_s_ofs_to_lbl p1)
      | BC.IFNE p1 -> T.IFNE (rel_s_ofs_to_lbl p1)
      | BC.IFNONNULL p1 -> T.IFNONNULL (rel_s_ofs_to_lbl p1)
      | BC.IFNULL p1 -> T.IFNULL (rel_s_ofs_to_lbl p1)
      | BC.IINC (p1, p2) -> T.IINC { T.ii_var = u1_to_int p1; ii_inc = s1_to_int p2 }
      | BC.ILOAD p1 -> T.ILOAD (u1_to_int p1)
      | BC.ILOAD_0 -> T.ILOAD 0
      | BC.ILOAD_1 -> T.ILOAD 1
      | BC.ILOAD_2 -> T.ILOAD 2
      | BC.ILOAD_3 -> T.ILOAD 3
      | BC.IMUL -> T.IMUL
      | BC.INEG -> T.INEG
      | BC.INSTANCEOF p1 -> T.INSTANCEOF (HC.decode_typeref pool p1)
      | BC.INVOKEDYNAMIC p1 -> fail (T.Unsupported "Instruction INVOKEDYNAMIC")
      | BC.INVOKEINTERFACE (p1, _) -> T.INVOKEINTERFACE (HC.decode_methodref pool p1)
      | BC.INVOKESPECIAL p1 -> T.INVOKESPECIAL (HC.decode_methodref pool p1)
      | BC.INVOKESTATIC p1 -> T.INVOKESTATIC (HC.decode_methodref pool p1)
      | BC.INVOKEVIRTUAL p1 -> T.INVOKEVIRTUAL (HC.decode_methodref pool p1)
      | BC.IOR -> T.IOR
      | BC.IREM -> T.IREM
      | BC.IRETURN -> T.IRETURN
      | BC.ISHL -> T.ISHL
      | BC.ISHR -> T.ISHR
      | BC.ISTORE p1 -> T.ISTORE (u1_to_int p1)
      | BC.ISTORE_0 -> T.ISTORE 0
      | BC.ISTORE_1 -> T.ISTORE 1
      | BC.ISTORE_2 -> T.ISTORE 2
      | BC.ISTORE_3 -> T.ISTORE 3
      | BC.ISUB -> T.ISUB
      | BC.IUSHR -> T.IUSHR
      | BC.IXOR -> T.IXOR
      | BC.JSR p1 -> T.JSR (rel_s_ofs_to_lbl p1)
      | BC.JSR_W p1 -> T.JSR (rel_l_ofs_to_lbl p1)
      | BC.L2D -> T.L2D
      | BC.L2F -> T.L2F
      | BC.L2I -> T.L2I
      | BC.LADD -> T.LADD
      | BC.LALOAD -> T.LALOAD
      | BC.LAND -> T.LAND
      | BC.LASTORE -> T.LASTORE
      | BC.LCMP -> T.LCMP
      | BC.LCONST_0 -> T.LCONST_0
      | BC.LCONST_1 -> T.LCONST_1
      | BC.LDC c -> T.LDC (HC.decode_stack pool (U.u2_of_u1 c))
      | BC.LDC2_W c -> T.LDC (HC.decode_stack pool c)
      | BC.LDC_W c -> T.LDC (HC.decode_stack pool c)
      | BC.LDIV -> T.LDIV
      | BC.LLOAD p1 -> T.LLOAD (u1_to_int p1)
      | BC.LLOAD_0 -> T.LLOAD 0
      | BC.LLOAD_1 -> T.LLOAD 1
      | BC.LLOAD_2 -> T.LLOAD 2
      | BC.LLOAD_3 -> T.LLOAD 3
      | BC.LMUL -> T.LMUL
      | BC.LNEG -> T.LNEG
      | BC.LOOKUPSWITCH (p1, p2, p3) ->
        let keys, offsets = List.split p3 in
        let labels = List.map rel_l_ofs_to_lbl offsets in
        let items = List.map s4_to_int32 keys in
        (* TODO(rgrig): [p2] should not be in BC.LOOKUPSWITCH. *)
      (* bytecode parser should ensure this *)
        assert (s4_to_int p2 = List.length p3);
        T.LOOKUPSWITCH { T.ls_def = rel_l_ofs_to_lbl p1
                     ; ls_branches = List.combine items labels }
      | BC.LOR -> T.LOR
      | BC.LREM -> T.LREM
      | BC.LRETURN -> T.LRETURN
      | BC.LSHL -> T.LSHL
      | BC.LSHR -> T.LSHR
      | BC.LSTORE p1 -> T.LSTORE (u1_to_int p1)
      | BC.LSTORE_0 -> T.LSTORE 0
      | BC.LSTORE_1 -> T.LSTORE 1
      | BC.LSTORE_2 -> T.LSTORE 2
      | BC.LSTORE_3 -> T.LSTORE 3
      | BC.LSUB -> T.LSUB
      | BC.LUSHR -> T.LUSHR
      | BC.LXOR -> T.LXOR
      | BC.MONITORENTER -> T.MONITORENTER
      | BC.MONITOREXIT -> T.MONITOREXIT
      | BC.MULTIANEWARRAY (p1, p2) -> T.MULTIANEWARRAY (HC.decode_typeref pool p1, u1_to_int p2)
      | BC.NEW p1 -> hi_NEW p1
      | BC.NEWARRAY p1 -> T.NEWARRAY (primitive_array_type_of_int (p1 :> int))
      | BC.NOP -> T.NOP
      | BC.POP -> T.POP
      | BC.POP2 -> T.POP2
      | BC.PUTFIELD p1 -> T.PUTFIELD (HC.decode_fieldref pool p1)
      | BC.PUTSTATIC p1 -> T.PUTSTATIC (HC.decode_fieldref pool p1)
      | BC.RET p1 -> T.RET (u1_to_int p1)
      | BC.RETURN -> T.RETURN
      | BC.SALOAD -> T.SALOAD
      | BC.SASTORE -> T.SASTORE
      | BC.SIPUSH p1 -> T.SIPUSH (s2_to_int p1)
      | BC.SWAP -> T.SWAP
      | BC.TABLESWITCH (def, low, high, ofss) ->
        T.TABLESWITCH {
          T.ts_def = rel_l_ofs_to_lbl def;
          ts_low = s4_to_int32 low;
          ts_high = s4_to_int32 high;
          ts_ofss = List.map rel_l_ofs_to_lbl ofss }
      | BC.WIDE_ALOAD p1 -> T.ALOAD (u2_to_int p1)
      | BC.WIDE_ASTORE p1 -> T.ASTORE (u2_to_int p1)
      | BC.WIDE_DLOAD p1 -> T.DLOAD (u2_to_int p1)
      | BC.WIDE_DSTORE p1 -> T.DSTORE (u2_to_int p1)
      | BC.WIDE_FLOAD p1 -> T.FLOAD (u2_to_int p1)
      | BC.WIDE_FSTORE p1 -> T.FSTORE (u2_to_int p1)
      | BC.WIDE_IINC (p1, p2) -> T.IINC { T.ii_var = u2_to_int p1
                                            ; ii_inc = s2_to_int p2 }
      | BC.WIDE_ILOAD p1 -> T.ILOAD (u2_to_int p1)
      | BC.WIDE_ISTORE p1 -> T.ISTORE (u2_to_int p1)
      | BC.WIDE_LLOAD p1 -> T.LLOAD (u2_to_int p1)
      | BC.WIDE_LSTORE p1 -> T.LSTORE (u2_to_int p1)
      | BC.WIDE_RET p1 -> T.RET (u2_to_int p1)

  (* NOTE: The current implementation generates suboptimal bytecode, but it
  ensures that the chosen opcodes do not depend on [ool] or on [here]. We do
  intend to generate better bytecode, but that requires calling this encode
  (from [encode_attr_code] in a sort-of fixed-point computation. *)
  let encode (ool : T.label -> int) here pool instruction =
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
      | _ -> fail T.Invalid_primitive_array_type in
    let bc_INVOKEINTERFACE m =
      (* TODO(rgrig): Move the following two helpers in [Descriptor]? *)
      let sizeof_jt : Descriptor.for_parameter -> int = function
        | `Long | `Double -> 2 | _ -> 1 in
      let sizeof_m ((args, _) : Descriptor.for_method) : int =
        List.fold_left (fun s a -> s + sizeof_jt a) 1 args in
      let `Methodref (_, _, d) = m in
      BC.INVOKEINTERFACE (HC.encode_methodref true pool m, U.u1 (sizeof_m d)) in
    let bc_LOOKUPSWITCH { T.ls_def; ls_branches } =
      let def = s4_offset ls_def in
      let cnt = s4 (List.length ls_branches) in
      let eb (v, l) = (U.s4 v, s4_offset l) in
      let jmp = List.map eb (List.sort compare ls_branches) in
      BC.LOOKUPSWITCH (def, cnt, jmp) in
    let bc_TABLESWITCH { T.ts_def; ts_low; ts_high; ts_ofss } =
      if ts_low > ts_high then fail T.Invalid_TABLESWITCH;
      let def = s4_offset ts_def in
      let low, high = U.s4 ts_low, U.s4 ts_high in
      let ofss = List.map s4_offset ts_ofss in
      BC.TABLESWITCH (def, low, high, ofss) in

    (* Helpers that pick between wide and narrow versions of instructions. *)
    let bc_LDC = function
      | `Int (-1l) -> BC.ICONST_M1
      | `Int 0l -> BC.ICONST_0
      | `Int 1l -> BC.ICONST_1
      | `Int 2l -> BC.ICONST_2
      | `Int 3l -> BC.ICONST_3
      | `Int 4l -> BC.ICONST_4
      | `Int 5l -> BC.ICONST_5
      | `Int x when -128l <= x && x < 128l -> BC.BIPUSH (U.s1 (Int32.to_int x))
      | c ->
          let j = HC.encode_stack pool c in
          if HC.size (c :> T.constant) = 2 then
            BC.LDC2_W j
          else
            BC.LDC_W j in
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
    let bc_IINC { T.ii_var; ii_inc } = BC.WIDE_IINC (U.u2 ii_var, U.s2 ii_inc) in
    let bc_GOTO l = BC.GOTO_W (s4_offset l) in
    let bc_JSR l = BC.JSR_W (s4_offset l) in

    match instruction with
    | T.AALOAD -> BC.AALOAD
    | T.AASTORE -> BC.AASTORE
    | T.ACONST_NULL -> BC.ACONST_NULL
    | T.ALOAD p1 -> bc_ALOAD p1
    | T.ANEWARRAY p1 -> BC.ANEWARRAY (HC.encode_typeref pool p1)
    | T.ARETURN -> BC.ARETURN
    | T.ARRAYLENGTH -> BC.ARRAYLENGTH
    | T.ASTORE p1 -> bc_ASTORE p1
    | T.ATHROW -> BC.ATHROW
    | T.BALOAD -> BC.BALOAD
    | T.BASTORE -> BC.BASTORE
    | T.CALOAD -> BC.CALOAD
    | T.CASTORE -> BC.CASTORE
    | T.CHECKCAST p1 -> BC.CHECKCAST (HC.encode_typeref pool p1)
    | T.D2F -> BC.D2F
    | T.D2I -> BC.D2I
    | T.D2L -> BC.D2L
    | T.DADD -> BC.DADD
    | T.DALOAD -> BC.DALOAD
    | T.DASTORE -> BC.DASTORE
    | T.DCMPG -> BC.DCMPG
    | T.DCMPL -> BC.DCMPL
    | T.DCONST_0 -> BC.DCONST_0
    | T.DCONST_1 -> BC.DCONST_1
    | T.DDIV -> BC.DDIV
    | T.DLOAD p1 -> bc_DLOAD p1
    | T.DMUL -> BC.DMUL
    | T.DNEG -> BC.DNEG
    | T.DREM -> BC.DREM
    | T.DRETURN -> BC.DRETURN
    | T.DSTORE p1 -> bc_DSTORE p1
    | T.DSUB -> BC.DSUB
    | T.DUP -> BC.DUP
    | T.DUP2 -> BC.DUP2
    | T.DUP2_X1 -> BC.DUP2_X1
    | T.DUP2_X2 -> BC.DUP2_X2
    | T.DUP_X1 -> BC.DUP_X1
    | T.DUP_X2 -> BC.DUP_X2
    | T.F2D -> BC.F2D
    | T.F2I -> BC.F2I
    | T.F2L -> BC.F2L
    | T.FADD -> BC.FADD
    | T.FALOAD -> BC.FALOAD
    | T.FASTORE -> BC.FASTORE
    | T.FCMPG -> BC.FCMPG
    | T.FCMPL -> BC.FCMPL
    | T.FCONST_0 -> BC.FCONST_0
    | T.FCONST_1 -> BC.FCONST_1
    | T.FCONST_2 -> BC.FCONST_2
    | T.FDIV -> BC.FDIV
    | T.FLOAD p1 -> bc_FLOAD p1
    | T.FMUL -> BC.FMUL
    | T.FNEG -> BC.FNEG
    | T.FREM -> BC.FREM
    | T.FRETURN -> BC.FRETURN
    | T.FSTORE p1 -> bc_FSTORE p1
    | T.FSUB -> BC.FSUB
    | T.GETFIELD f -> BC.GETFIELD (HC.encode_fieldref pool f)
    | T.GETSTATIC f -> BC.GETSTATIC (HC.encode_fieldref pool f)
    | T.GOTO p1 -> bc_GOTO p1
    | T.I2B -> BC.I2B
    | T.I2C -> BC.I2C
    | T.I2D -> BC.I2D
    | T.I2F -> BC.I2F
    | T.I2L -> BC.I2L
    | T.I2S -> BC.I2S
    | T.IADD -> BC.IADD
    | T.IALOAD -> BC.IALOAD
    | T.IAND -> BC.IAND
    | T.IASTORE -> BC.IASTORE
    | T.IDIV -> BC.IDIV
    | T.IF_ACMPEQ p1 -> BC.IF_ACMPEQ (s2_offset p1)
    | T.IF_ACMPNE p1 -> BC.IF_ACMPNE (s2_offset p1)
    | T.IF_ICMPEQ p1 -> BC.IF_ICMPEQ (s2_offset p1)
    | T.IF_ICMPGE p1 -> BC.IF_ICMPGE (s2_offset p1)
    | T.IF_ICMPGT p1 -> BC.IF_ICMPGT (s2_offset p1)
    | T.IF_ICMPLE p1 -> BC.IF_ICMPLE (s2_offset p1)
    | T.IF_ICMPLT p1 -> BC.IF_ICMPLT (s2_offset p1)
    | T.IF_ICMPNE p1 -> BC.IF_ICMPNE (s2_offset p1)
    | T.IFEQ p1 -> BC.IFEQ (s2_offset p1)
    | T.IFGE p1 -> BC.IFGE (s2_offset p1)
    | T.IFGT p1 -> BC.IFGT (s2_offset p1)
    | T.IFLE p1 -> BC.IFLE (s2_offset p1)
    | T.IFLT p1 -> BC.IFLT (s2_offset p1)
    | T.IFNE p1 -> BC.IFNE (s2_offset p1)
    | T.IFNONNULL p1 -> BC.IFNONNULL (s2_offset p1)
    | T.IFNULL p1 -> BC.IFNULL (s2_offset p1)
    | T.IINC x -> bc_IINC x
    | T.ILOAD p1 -> bc_ILOAD p1
    | T.IMUL -> BC.IMUL
    | T.INEG -> BC.INEG
    | T.INSTANCEOF p1 -> BC.INSTANCEOF (HC.encode_typeref pool p1)
    | T.INVOKEINTERFACE m -> bc_INVOKEINTERFACE m
    | T.INVOKESPECIAL m -> BC.INVOKESPECIAL (HC.encode_methodref false pool m)
    | T.INVOKESTATIC m -> BC.INVOKESTATIC (HC.encode_methodref false pool m)
    | T.INVOKEVIRTUAL m -> BC.INVOKEVIRTUAL (HC.encode_methodref false pool m)
    | T.IOR -> BC.IOR
    | T.IREM -> BC.IREM
    | T.IRETURN -> BC.IRETURN
    | T.ISHL -> BC.ISHL
    | T.ISHR -> BC.ISHR
    | T.ISTORE p1 -> bc_ISTORE p1
    | T.ISUB -> BC.ISUB
    | T.IUSHR -> BC.IUSHR
    | T.IXOR -> BC.IXOR
    | T.JSR p1 -> bc_JSR p1
    | T.L2D -> BC.L2D
    | T.L2F -> BC.L2F
    | T.L2I -> BC.L2I
    | T.LADD -> BC.LADD
    | T.LALOAD -> BC.LALOAD
    | T.LAND -> BC.LAND
    | T.LASTORE -> BC.LASTORE
    | T.LCMP -> BC.LCMP
    | T.LCONST_0 -> BC.LCONST_0
    | T.LCONST_1 -> BC.LCONST_1
    | T.LDC p1 -> bc_LDC p1
    | T.LDIV -> BC.LDIV
    | T.LLOAD p1 -> bc_LLOAD p1
    | T.LMUL -> BC.LMUL
    | T.LNEG -> BC.LNEG
    | T.LOOKUPSWITCH x -> bc_LOOKUPSWITCH x
    | T.LOR -> BC.LOR
    | T.LREM -> BC.LREM
    | T.LRETURN -> BC.LRETURN
    | T.LSHL -> BC.LSHL
    | T.LSHR -> BC.LSHR
    | T.LSTORE p1 -> bc_LSTORE p1
    | T.LSUB -> BC.LSUB
    | T.LUSHR -> BC.LUSHR
    | T.LXOR -> BC.LXOR
    | T.MONITORENTER -> BC.MONITORENTER
    | T.MONITOREXIT -> BC.MONITOREXIT
    | T.MULTIANEWARRAY (c, d) -> BC.MULTIANEWARRAY (HC.encode_typeref pool c, U.u1 d)
    | T.NEW c -> BC.NEW (CP.add_class pool c)
    | T.NEWARRAY p1 -> BC.NEWARRAY (U.u1 (int_of_primitive_array_type p1))
    | T.NOP -> BC.NOP
    | T.POP -> BC.POP
    | T.POP2 -> BC.POP2
    | T.PUTFIELD f -> BC.PUTFIELD (HC.encode_fieldref pool f)
    | T.PUTSTATIC f -> BC.PUTSTATIC (HC.encode_fieldref pool f)
    | T.RET p1 -> bc_RET p1
    | T.RETURN -> BC.RETURN
    | T.SALOAD -> BC.SALOAD
    | T.SASTORE -> BC.SASTORE
    | T.SIPUSH p1 -> BC.SIPUSH (U.s2 p1)
    | T.SWAP -> BC.SWAP
    | T.TABLESWITCH x -> bc_TABLESWITCH x

  let version_bounds (_, inst) = match inst with
    | T.AALOAD -> Version.make_bounds "'AALOAD' instruction" Version.Java_1_0 None
    | T.AASTORE -> Version.make_bounds "'AASTORE' instruction" Version.Java_1_0 None
    | T.ACONST_NULL -> Version.make_bounds "'ACONST_NULL' instruction" Version.Java_1_0 None
    | T.ALOAD _ -> Version.make_bounds "'ALOAD' instruction" Version.Java_1_0 None
    | T.ANEWARRAY _ -> Version.make_bounds "'ANEWARRAY' instruction" Version.Java_1_0 None
    | T.ARETURN -> Version.make_bounds "'ARETURN' instruction" Version.Java_1_0 None
    | T.ARRAYLENGTH -> Version.make_bounds "'ARRAYLENGTH' instruction" Version.Java_1_0 None
    | T.ASTORE _ -> Version.make_bounds "'ASTORE' instruction" Version.Java_1_0 None
    | T.ATHROW -> Version.make_bounds "'ATHROW' instruction" Version.Java_1_0 None
    | T.BALOAD -> Version.make_bounds "'BALOAD' instruction" Version.Java_1_0 None
    | T.BASTORE -> Version.make_bounds "'BASTORE' instruction" Version.Java_1_0 None
    | T.CALOAD -> Version.make_bounds "'CALOAD' instruction" Version.Java_1_0 None
    | T.CASTORE -> Version.make_bounds "'CASTORE' instruction" Version.Java_1_0 None
    | T.CHECKCAST _ -> Version.make_bounds "'CHECKCAST' instruction" Version.Java_1_0 None
    | T.D2F -> Version.make_bounds "'D2F' instruction" Version.Java_1_0 None
    | T.D2I -> Version.make_bounds "'D2I' instruction" Version.Java_1_0 None
    | T.D2L -> Version.make_bounds "'D2L' instruction" Version.Java_1_0 None
    | T.DADD -> Version.make_bounds "'DADD' instruction" Version.Java_1_0 None
    | T.DALOAD -> Version.make_bounds "'DALOAD' instruction" Version.Java_1_0 None
    | T.DASTORE -> Version.make_bounds "'DASTORE' instruction" Version.Java_1_0 None
    | T.DCMPG -> Version.make_bounds "'DCMPG' instruction" Version.Java_1_0 None
    | T.DCMPL -> Version.make_bounds "'DCMPL' instruction" Version.Java_1_0 None
    | T.DCONST_0 -> Version.make_bounds "'DCONST_0' instruction" Version.Java_1_0 None
    | T.DCONST_1 -> Version.make_bounds "'DCONST_1' instruction" Version.Java_1_0 None
    | T.DDIV -> Version.make_bounds "'DDIV' instruction" Version.Java_1_0 None
    | T.DLOAD _ -> Version.make_bounds "'DLOAD' instruction" Version.Java_1_0 None
    | T.DMUL -> Version.make_bounds "'DMUL' instruction" Version.Java_1_0 None
    | T.DNEG -> Version.make_bounds "'DNEG' instruction" Version.Java_1_0 None
    | T.DREM -> Version.make_bounds "'DREM' instruction" Version.Java_1_0 None
    | T.DRETURN -> Version.make_bounds "'DRETURN' instruction" Version.Java_1_0 None
    | T.DSTORE _ -> Version.make_bounds "'DSTORE' instruction" Version.Java_1_0 None
    | T.DSUB -> Version.make_bounds "'DSUB' instruction" Version.Java_1_0 None
    | T.DUP -> Version.make_bounds "'DUP' instruction" Version.Java_1_0 None
    | T.DUP2 -> Version.make_bounds "'DUP2' instruction" Version.Java_1_0 None
    | T.DUP2_X1 -> Version.make_bounds "'DUP2_X1' instruction" Version.Java_1_0 None
    | T.DUP2_X2 -> Version.make_bounds "'DUP2_X2' instruction" Version.Java_1_0 None
    | T.DUP_X1 -> Version.make_bounds "'DUP_X1' instruction" Version.Java_1_0 None
    | T.DUP_X2 -> Version.make_bounds "'DUP_X2' instruction" Version.Java_1_0 None
    | T.F2D -> Version.make_bounds "'F2D' instruction" Version.Java_1_0 None
    | T.F2I -> Version.make_bounds "'F2I' instruction" Version.Java_1_0 None
    | T.F2L -> Version.make_bounds "'F2L' instruction" Version.Java_1_0 None
    | T.FADD -> Version.make_bounds "'FADD' instruction" Version.Java_1_0 None
    | T.FALOAD -> Version.make_bounds "'FALOAD' instruction" Version.Java_1_0 None
    | T.FASTORE -> Version.make_bounds "'FASTORE' instruction" Version.Java_1_0 None
    | T.FCMPG -> Version.make_bounds "'FCMPG' instruction" Version.Java_1_0 None
    | T.FCMPL -> Version.make_bounds "'FCMPL' instruction" Version.Java_1_0 None
    | T.FCONST_0 -> Version.make_bounds "'FCONST_0' instruction" Version.Java_1_0 None
    | T.FCONST_1 -> Version.make_bounds "'FCONST_1' instruction" Version.Java_1_0 None
    | T.FCONST_2 -> Version.make_bounds "'FCONST_2' instruction" Version.Java_1_0 None
    | T.FDIV -> Version.make_bounds "'FDIV' instruction" Version.Java_1_0 None
    | T.FLOAD _ -> Version.make_bounds "'FLOAD' instruction" Version.Java_1_0 None
    | T.FMUL -> Version.make_bounds "'FMUL' instruction" Version.Java_1_0 None
    | T.FNEG -> Version.make_bounds "'FNEG' instruction" Version.Java_1_0 None
    | T.FREM -> Version.make_bounds "'FREM' instruction" Version.Java_1_0 None
    | T.FRETURN -> Version.make_bounds "'FRETURN' instruction" Version.Java_1_0 None
    | T.FSTORE _ -> Version.make_bounds "'FSTORE' instruction" Version.Java_1_0 None
    | T.FSUB -> Version.make_bounds "'FSUB' instruction" Version.Java_1_0 None
    | T.GETFIELD _ -> Version.make_bounds "'GETFIELD' instruction" Version.Java_1_0 None
    | T.GETSTATIC _ -> Version.make_bounds "'GETSTATIC' instruction" Version.Java_1_0 None
    | T.GOTO _ -> Version.make_bounds "'GOTO' instruction" Version.Java_1_0 None
    | T.I2B -> Version.make_bounds "'I2B' instruction" Version.Java_1_0 None
    | T.I2C -> Version.make_bounds "'I2C' instruction" Version.Java_1_0 None
    | T.I2D -> Version.make_bounds "'I2D' instruction" Version.Java_1_0 None
    | T.I2F -> Version.make_bounds "'I2F' instruction" Version.Java_1_0 None
    | T.I2L -> Version.make_bounds "'I2L' instruction" Version.Java_1_0 None
    | T.I2S -> Version.make_bounds "'I2S' instruction" Version.Java_1_0 None
    | T.IADD -> Version.make_bounds "'IADD' instruction" Version.Java_1_0 None
    | T.IALOAD -> Version.make_bounds "'IALOAD' instruction" Version.Java_1_0 None
    | T.IAND -> Version.make_bounds "'IAND' instruction" Version.Java_1_0 None
    | T.IASTORE -> Version.make_bounds "'IASTORE' instruction" Version.Java_1_0 None
    | T.IDIV -> Version.make_bounds "'IDIV' instruction" Version.Java_1_0 None
    | T.IF_ACMPEQ _ -> Version.make_bounds "'IF_ACMPEQ' instruction" Version.Java_1_0 None
    | T.IF_ACMPNE _ -> Version.make_bounds "'IF_ACMPNE' instruction" Version.Java_1_0 None
    | T.IF_ICMPEQ _ -> Version.make_bounds "'IF_ICMPEQ' instruction" Version.Java_1_0 None
    | T.IF_ICMPGE _ -> Version.make_bounds "'IF_ICMPGE' instruction" Version.Java_1_0 None
    | T.IF_ICMPGT _ -> Version.make_bounds "'IF_ICMPGT' instruction" Version.Java_1_0 None
    | T.IF_ICMPLE _ -> Version.make_bounds "'IF_ICMPLE' instruction" Version.Java_1_0 None
    | T.IF_ICMPLT _ -> Version.make_bounds "'IF_ICMPLT' instruction" Version.Java_1_0 None
    | T.IF_ICMPNE _ -> Version.make_bounds "'IF_ICMPNE' instruction" Version.Java_1_0 None
    | T.IFEQ _ -> Version.make_bounds "'IFEQ' instruction" Version.Java_1_0 None
    | T.IFGE _ -> Version.make_bounds "'IFGE' instruction" Version.Java_1_0 None
    | T.IFGT _ -> Version.make_bounds "'IFGT' instruction" Version.Java_1_0 None
    | T.IFLE _ -> Version.make_bounds "'IFLE' instruction" Version.Java_1_0 None
    | T.IFLT _ -> Version.make_bounds "'IFLT' instruction" Version.Java_1_0 None
    | T.IFNE _ -> Version.make_bounds "'IFNE' instruction" Version.Java_1_0 None
    | T.IFNONNULL _ -> Version.make_bounds "'IFNONNULL' instruction" Version.Java_1_0 None
    | T.IFNULL _ -> Version.make_bounds "'IFNULL' instruction" Version.Java_1_0 None
    | T.IINC _ -> Version.make_bounds "'IINC' instruction" Version.Java_1_0 None
    | T.ILOAD _ -> Version.make_bounds "'ILOAD' instruction" Version.Java_1_0 None
    | T.IMUL -> Version.make_bounds "'IMUL' instruction" Version.Java_1_0 None
    | T.INEG -> Version.make_bounds "'INEG' instruction" Version.Java_1_0 None
    | T.INSTANCEOF _ -> Version.make_bounds "'INSTANCEOF' instruction" Version.Java_1_0 None
    | T.INVOKEINTERFACE _ -> Version.make_bounds "'INVOKEINTERFACE' instruction" Version.Java_1_0 None
    | T.INVOKESPECIAL _ -> Version.make_bounds "'INVOKESPECIAL' instruction" Version.Java_1_0 None
    | T.INVOKESTATIC _ -> Version.make_bounds "'INVOKESTATIC' instruction" Version.Java_1_0 None
    | T.INVOKEVIRTUAL _ -> Version.make_bounds "'INVOKEVIRTUAL' instruction" Version.Java_1_0 None
    | T.IOR -> Version.make_bounds "'IOR' instruction" Version.Java_1_0 None
    | T.IREM -> Version.make_bounds "'IREM' instruction" Version.Java_1_0 None
    | T.IRETURN -> Version.make_bounds "'IRETURN' instruction" Version.Java_1_0 None
    | T.ISHL -> Version.make_bounds "'ISHL' instruction" Version.Java_1_0 None
    | T.ISHR -> Version.make_bounds "'ISHR' instruction" Version.Java_1_0 None
    | T.ISTORE _ -> Version.make_bounds "'ISTORE' instruction" Version.Java_1_0 None
    | T.ISUB -> Version.make_bounds "'ISUB' instruction" Version.Java_1_0 None
    | T.IUSHR -> Version.make_bounds "'IUSHR' instruction" Version.Java_1_0 None
    | T.IXOR -> Version.make_bounds "'IXOR' instruction" Version.Java_1_0 None
    | T.JSR _ -> Version.make_bounds "'JSR' instruction" Version.Java_1_0 (Some Version.Java_1_6)
    | T.L2D -> Version.make_bounds "'L2D' instruction" Version.Java_1_0 None
    | T.L2F -> Version.make_bounds "'L2F' instruction" Version.Java_1_0 None
    | T.L2I -> Version.make_bounds "'L2I' instruction" Version.Java_1_0 None
    | T.LADD -> Version.make_bounds "'LADD' instruction" Version.Java_1_0 None
    | T.LALOAD -> Version.make_bounds "'LALOAD' instruction" Version.Java_1_0 None
    | T.LAND -> Version.make_bounds "'LAND' instruction" Version.Java_1_0 None
    | T.LASTORE -> Version.make_bounds "'LASTORE' instruction" Version.Java_1_0 None
    | T.LCMP -> Version.make_bounds "'LCMP' instruction" Version.Java_1_0 None
    | T.LCONST_0 -> Version.make_bounds "'LCONST_0' instruction" Version.Java_1_0 None
    | T.LCONST_1 -> Version.make_bounds "'LCONST_1' instruction" Version.Java_1_0 None
    | T.LDC x -> Version.make_bounds "'LDC' instruction" (match x with `Class_or_interface _ -> Version.Java_1_5 | _ -> Version.Java_1_0) None
    | T.LDIV -> Version.make_bounds "'LDIV' instruction" Version.Java_1_0 None
    | T.LLOAD _ -> Version.make_bounds "'LLOAD' instruction" Version.Java_1_0 None
    | T.LMUL -> Version.make_bounds "'LMUL' instruction" Version.Java_1_0 None
    | T.LNEG -> Version.make_bounds "'LNEG' instruction" Version.Java_1_0 None
    | T.LOOKUPSWITCH _ -> Version.make_bounds "'LOOKUPSWITCH' instruction" Version.Java_1_0 None
    | T.LOR -> Version.make_bounds "'LOR' instruction" Version.Java_1_0 None
    | T.LREM -> Version.make_bounds "'LREM' instruction" Version.Java_1_0 None
    | T.LRETURN -> Version.make_bounds "'LRETURN' instruction" Version.Java_1_0 None
    | T.LSHL -> Version.make_bounds "'LSHL' instruction" Version.Java_1_0 None
    | T.LSHR -> Version.make_bounds "'LSHR' instruction" Version.Java_1_0 None
    | T.LSTORE _ -> Version.make_bounds "'LSTORE' instruction" Version.Java_1_0 None
    | T.LSUB -> Version.make_bounds "'LSUB' instruction" Version.Java_1_0 None
    | T.LUSHR -> Version.make_bounds "'LUSHR' instruction" Version.Java_1_0 None
    | T.LXOR -> Version.make_bounds "'LXOR' instruction" Version.Java_1_0 None
    | T.MONITORENTER -> Version.make_bounds "'MONITORENTER' instruction" Version.Java_1_0 None
    | T.MONITOREXIT -> Version.make_bounds "'MONITOREXIT' instruction" Version.Java_1_0 None
    | T.MULTIANEWARRAY _ -> Version.make_bounds "'MULTIANEWARRAY' instruction" Version.Java_1_0 None
    | T.NEW _ -> Version.make_bounds "'NEW' instruction" Version.Java_1_0 None
    | T.NEWARRAY _ -> Version.make_bounds "'NEWARRAY' instruction" Version.Java_1_0 None
    | T.NOP -> Version.make_bounds "'NOP' instruction" Version.Java_1_0 None
    | T.POP -> Version.make_bounds "'POP' instruction" Version.Java_1_0 None
    | T.POP2 -> Version.make_bounds "'POP2' instruction" Version.Java_1_0 None
    | T.PUTFIELD _ -> Version.make_bounds "'PUTFIELD' instruction" Version.Java_1_0 None
    | T.PUTSTATIC _ -> Version.make_bounds "'PUTSTATIC' instruction" Version.Java_1_0 None
    | T.RET _ -> Version.make_bounds "'RET' instruction" Version.Java_1_0 (Some Version.Java_1_6)
    | T.RETURN -> Version.make_bounds "'RETURN' instruction" Version.Java_1_0 None
    | T.SALOAD -> Version.make_bounds "'SALOAD' instruction" Version.Java_1_0 None
    | T.SASTORE -> Version.make_bounds "'SASTORE' instruction" Version.Java_1_0 None
    | T.SIPUSH _ -> Version.make_bounds "'SIPUSH' instruction" Version.Java_1_0 None
    | T.SWAP -> Version.make_bounds "'SWAP' instruction" Version.Java_1_0 None
    | T.TABLESWITCH _ -> Version.make_bounds "'TABLESWITCH' instruction" Version.Java_1_0 None

 let instruction_to_string = function
    | T.AALOAD -> "AALOAD"
    | T.AASTORE -> "AASTORE"
    | T.ACONST_NULL -> "ACONST"
    | T.ALOAD _ -> "ALOAD"
    | T.ANEWARRAY _ -> "ANEWARRAY"
    | T.ARETURN -> "ARETURN"
    | T.ARRAYLENGTH -> "ARRAYLENGTH"
    | T.ASTORE _ -> "ASTORE"
    | T.ATHROW -> "ATHROW"
    | T.BALOAD -> "BALOAD"
    | T.BASTORE -> "BASTORE"
    | T.CALOAD -> "CALOAD"
    | T.CASTORE -> "CASTORE"
    | T.CHECKCAST _ -> "CHECKCAST"
    | T.D2F -> "D2F"
    | T.D2I -> "D2I"
    | T.D2L -> "D2L"
    | T.DADD -> "DADD"
    | T.DALOAD -> "DALOAD"
    | T.DASTORE -> "DASTORE"
    | T.DCMPG -> "DCMPG"
    | T.DCMPL -> "DCMPL"
    | T.DCONST_0 -> "DCONST"
    | T.DCONST_1 -> "DCONST"
    | T.DDIV -> "DDIV"
    | T.DLOAD _ -> "DLOAD"
    | T.DMUL -> "DMUL"
    | T.DNEG -> "DNEG"
    | T.DREM -> "DREM"
    | T.DRETURN -> "DRETURN"
    | T.DSTORE _ -> "DSTORE"
    | T.DSUB -> "DSUB"
    | T.DUP -> "DUP"
    | T.DUP2 -> "DUP2"
    | T.DUP2_X1 -> "DUP2"
    | T.DUP2_X2 -> "DUP2"
    | T.DUP_X1 -> "DUP"
    | T.DUP_X2 -> "DUP"
    | T.F2D -> "F2D"
    | T.F2I -> "F2I"
    | T.F2L -> "F2L"
    | T.FADD -> "FADD"
    | T.FALOAD -> "FALOAD"
    | T.FASTORE -> "FASTORE"
    | T.FCMPG -> "FCMPG"
    | T.FCMPL -> "FCMPL"
    | T.FCONST_0 -> "FCONST"
    | T.FCONST_1 -> "FCONST"
    | T.FCONST_2 -> "FCONST"
    | T.FDIV -> "FDIV"
    | T.FLOAD _ -> "FLOAD"
    | T.FMUL -> "FMUL"
    | T.FNEG -> "FNEG"
    | T.FREM -> "FREM"
    | T.FRETURN -> "FRETURN"
    | T.FSTORE _ -> "FSTORE"
    | T.FSUB -> "FSUB"
    | T.GETFIELD _ -> "GETFIELD"
    | T.GETSTATIC _ -> "GETSTATIC"
    | T.GOTO _ -> "GOTO"
    | T.I2B -> "I2B"
    | T.I2C -> "I2C"
    | T.I2D -> "I2D"
    | T.I2F -> "I2F"
    | T.I2L -> "I2L"
    | T.I2S -> "I2S"
    | T.IADD -> "IADD"
    | T.IALOAD -> "IALOAD"
    | T.IAND -> "IAND"
    | T.IASTORE -> "IASTORE"
    | T.IDIV -> "IDIV"
    | T.IF_ACMPEQ _ -> "IF"
    | T.IF_ACMPNE _ -> "IF"
    | T.IF_ICMPEQ _ -> "IF"
    | T.IF_ICMPGE _ -> "IF"
    | T.IF_ICMPGT _ -> "IF"
    | T.IF_ICMPLE _ -> "IF"
    | T.IF_ICMPLT _ -> "IF"
    | T.IF_ICMPNE _ -> "IF"
    | T.IFEQ _ -> "IFEQ"
    | T.IFGE _ -> "IFGE"
    | T.IFGT _ -> "IFGT"
    | T.IFLE _ -> "IFLE"
    | T.IFLT _ -> "IFLT"
    | T.IFNE _ -> "IFNE"
    | T.IFNONNULL _ -> "IFNONNULL"
    | T.IFNULL _ -> "IFNULL"
    | T.IINC _ -> "IINC"
    | T.ILOAD _ -> "ILOAD"
    | T.IMUL -> "IMUL"
    | T.INEG -> "INEG"
    | T.INSTANCEOF _ -> "INSTANCEOF"
    | T.INVOKEINTERFACE _ -> "INVOKEINTERFACE"
    | T.INVOKESPECIAL _ -> "INVOKESPECIAL"
    | T.INVOKESTATIC _ -> "INVOKESTATIC"
    | T.INVOKEVIRTUAL _ -> "INVOKEVIRTUAL"
    | T.IOR -> "IOR"
    | T.IREM -> "IREM"
    | T.IRETURN -> "IRETURN"
    | T.ISHL -> "ISHL"
    | T.ISHR -> "ISHR"
    | T.ISTORE _ -> "ISTORE"
    | T.ISUB -> "ISUB"
    | T.IUSHR -> "IUSHR"
    | T.IXOR -> "IXOR"
    | T.JSR _ -> "JSR"
    | T.L2D -> "L2D"
    | T.L2F -> "L2F"
    | T.L2I -> "L2I"
    | T.LADD -> "LADD"
    | T.LALOAD -> "LALOAD"
    | T.LAND -> "LAND"
    | T.LASTORE -> "LASTORE"
    | T.LCMP -> "LCMP"
    | T.LCONST_0 -> "LCONST"
    | T.LCONST_1 -> "LCONST"
    | T.LDC _ -> "LDC"
    | T.LDIV -> "LDIV"
    | T.LLOAD _ -> "LLOAD"
    | T.LMUL -> "LMUL"
    | T.LNEG -> "LNEG"
    | T.LOOKUPSWITCH _ -> "LOOKUPSWITCH"
    | T.LOR -> "LOR"
    | T.LREM -> "LREM"
    | T.LRETURN -> "LRETURN"
    | T.LSHL -> "LSHL"
    | T.LSHR -> "LSHR"
    | T.LSTORE _ -> "LSTORE"
    | T.LSUB -> "LSUB"
    | T.LUSHR -> "LUSHR"
    | T.LXOR -> "LXOR"
    | T.MONITORENTER -> "MONITORENTER"
    | T.MONITOREXIT -> "MONITOREXIT"
    | T.MULTIANEWARRAY _ -> "MULTIANEWARRAY"
    | T.NEW _ -> "NEW"
    | T.NEWARRAY _ -> "NEWARRAY"
    | T.NOP -> "NOP"
    | T.POP -> "POP"
    | T.POP2 -> "POP2"
    | T.PUTFIELD _ -> "PUTFIELD"
    | T.PUTSTATIC _ -> "PUTSTATIC"
    | T.RET _ -> "RET"
    | T.RETURN -> "RETURN"
    | T.SALOAD -> "SALOAD"
    | T.SASTORE -> "SASTORE"
    | T.SIPUSH _ -> "SIPUSH"
    | T.SWAP -> "SWAP"
    | T.TABLESWITCH _ -> "TABLESWITCH"
end (* }}} *)
module HI = HighInstruction
module HighMethod = struct (* {{{ *)
  let to_string = function
    | T.RegularMethod r -> U.UTF8.to_string (Name.utf8_for_method r.T.rm_name)
    | T.InitMethod _ -> "<init>"
    | T.ClinitMethod _ -> "<clinit>"
end
(* }}} *)
module HM = HighMethod
module SymbExe = struct  (* {{{ *)
  module LS = HighTypes.LabelSet
  module H = HighTypes.LabelHash

  (* equal shape, used for verification *)
  let equal_verification_type_info x y = match (x, y) with
    | (T.Object (`Class_or_interface cn1)),
      (T.Object (`Class_or_interface cn2)) -> Name.equal_for_class cn1 cn2
    | (T.Object (`Array_type at1)),
      (T.Object (`Array_type at2)) ->
	Descriptor.equal_java_type
          (at1 :> Descriptor.java_type)
          (at2 :> Descriptor.java_type)
    | (T.Object _), (T.Object _) -> false
    | T.Return_address _, T.Return_address _ -> true
    | _ -> x = y

  (* equal shape and value, used for symbolic execution *)
  let equal_bt_se x y = match (x, y) with
    | T.Return_address sx, T.Return_address sy -> LS.equal sx sy
    | _ -> equal_verification_type_info x y

  let java_lang_String = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.String")
  let java_lang_invoke_MethodType = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.invoke.MethodType")
  let java_lang_invoke_MethodHandle = Name.make_for_class_from_external (U.UTF8.of_string "java.lang.invoke.MethodHandle")
  let verification_type_info_of_constant_descriptor = function
    | `Int _ -> T.Integer
    | `Float _ -> T.Float
(* TODO(rgrig): Does `Class_or_interface need distinct parameters here? *)
    | `String _ -> T.Object (`Class_or_interface java_lang_String)
    | `Class_or_interface _ -> T.Object (`Class_or_interface java_lang_Class)
    | `Array_type _ -> T.Object (`Class_or_interface java_lang_Class)
    | `Long _ -> T.Long
    | `Double _ -> T.Double
    | `Interface_method _ -> T.Object (`Class_or_interface java_lang_invoke_MethodHandle)
    | `Method_type _ -> T.Object (`Class_or_interface java_lang_invoke_MethodType)
    | `Method_handle _ -> T.Object (`Class_or_interface java_lang_invoke_MethodHandle)

  let enclose (x : Descriptor.array_type) =
    `Array (x :> Descriptor.for_field)

  let verification_type_info_of_array_element = function
    | `Array_type at -> T.Object (`Array_type (enclose at))
    | `Class_or_interface cn -> T.Object (`Array_type (`Array (`Class cn)))

  let verification_type_info_of_array_primitive = function
    | `Boolean -> T.Object (`Array_type (`Array `Boolean))
    | `Char -> T.Object (`Array_type (`Array `Char))
    | `Float -> T.Object (`Array_type (`Array `Float))
    | `Double -> T.Object (`Array_type (`Array `Double))
    | `Byte -> T.Object (`Array_type (`Array `Byte))
    | `Short -> T.Object (`Array_type (`Array `Short))
    | `Int -> T.Object (`Array_type (`Array `Int))
    | `Long -> T.Object (`Array_type (`Array `Long))
    | _ -> fail T.Invalid_primitive_array_type

  (* error reporting {{{ *)
  let report_invalid_stack_top (v, v') =
    T.SE_invalid_stack_top (string_of_bt v, string_of_bt v')

  let report_reference_expected x = T.SE_reference_expected (string_of_bt x)

  let report_array_expected x = T.SE_array_expected (string_of_bt x)

  let report_invalid_local_contents (i, v, v') =
    T.SE_invalid_local_contents (i, string_of_bt v, string_of_bt v')

  let report_unexpected_size s x =
    T.SE_unexpected_size (s, string_of_bt x)
  (* }}} *)
  (* symbolic stack {{{ *)
  type stack = T.bytecode_type list (* stack top is list head *)

  let pp_stack f x = fprintf f "@[[%a ]@]" (U.pp_list pp_bt) x

  let equal_stack a b = List.fold_left2 (fun r a b -> r && equal_bt_se a b) true a b

  let empty () = []

  let push v s =
    if log_se then printf "+";
    v :: s

  let push_return_value x s = match x with
    | `Void -> s
    | #Descriptor.for_parameter as y ->
      push (bt_of_descriptor y) s

  let top = function
    | hd :: _ -> hd
    | [] -> fail T.SE_empty_stack

  let pop =
  function
    | _ :: tl -> if log_se then printf "-"; tl
    | [] -> fail T.SE_empty_stack

  let pop_if v s =
    let v' = top s in
    let popable = match v with
      | T.Object _ -> true
      | _ -> equal_verification_type_info v v' in
    if popable then
      pop s
    else
      fail (report_invalid_stack_top (v, v'))

  let pop_if_size s =
    assert (s = 1 || s = 2);
    function
    | hd :: tl ->
        if size_of_bt hd = 1
        then (if log_se then printf "-"; (hd, tl))
        else fail (report_unexpected_size s hd)
    | [] -> fail T.SE_empty_stack

  (* }}} *)
  (* symbolic locals {{{ *)
  type locals = T.bytecode_type U.IntMap.t

  let pp_locals f ls =
    let pb r v = fprintf f "@ (%d->%a)" r pp_bt v in
    fprintf f "@[["; U.IntMap.iter pb ls; fprintf f " ]@]"

  let equal_locals a b = U.IntMap.equal equal_bt_se a b

  let intmap_width m =
    try
      let a, _ = U.IntMap.min_binding m in
      let b, _ = U.IntMap.max_binding m in
      b - a + 1
    with Not_found -> 0

  let load i l =
    try U.IntMap.find i l
    with Not_found ->
      fail (T.SE_uninitialized_register (i, intmap_width l))

  let check_load i l v =
    let v' = load i l in
    if not (equal_verification_type_info v v') then
      fail (report_invalid_local_contents (i, v, v'))

  let store i v l =
    let l = U.IntMap.add i v l in
    let l = if size_of_bt v = 2 then U.IntMap.add (succ i) T.Top l else l in
    l

  let encode_locals m =
    try
      let max, _ = U.IntMap.max_binding m in
      let r = ref [] in
      for i = 0 to max do
        r := (try U.IntMap.find i m with Not_found -> T.Top) :: !r
      done;
      let rec f acc = function
        | T.Top :: l :: ls when size_of_bt l = 2 -> f (l :: acc) ls
        | l :: ls -> f (l :: acc) ls
        | [] -> acc in
      List.rev (f !r [])
    with Not_found -> []
  (* }}} *)
  (* checks {{{ *)
  let check_reference x =
    match x with
      | T.Integer
      | T.Float
      | T.Long
      | T.Double
      | T.Return_address _
      | T.Top -> fail (report_reference_expected x)
      | T.Null
      | T.Uninitialized_this
      | T.Object _
      | T.Reference
      | T.Uninitialized _ -> ()

  let check_reference_or_return x =
    if not (equal_verification_type_info x (T.Return_address LS.empty))
    then check_reference x
  (* }}} *)
  (* symbolic execution {{{ *)

  type 'a stepper = 'a -> T.instruction -> T.label -> 'a * T.label list
  type 'a executor =
    'a stepper -> ('a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> bool) -> 'a
    -> T.code_value -> 'a H.t

  type t = {
    locals : locals;
    stack : stack;
  }

  let pp_t f { locals; stack } =
    fprintf f "@[<1>(%a,@ %a)@]" pp_locals locals pp_stack stack

  let pp_map_t f m =
    let l = ref [] in
    H.iter (fun k v -> l := (k, v) :: !l) m;
    let l = List.sort compare !l in
    let pb (l, t) = fprintf f "@\n@[%Ld -> %a@]" l pp_t t in
    fprintf f "@\n@[<2>(symbolic execution state %d" (H.length m);
    List.iter pb l;
    fprintf f "@\n)@]@\n"

  let equal_t a b =
    equal_locals a.locals b.locals && equal_stack a.stack b.stack

  let make_empty () =
    { locals = U.IntMap.empty; stack = []; }

  let stack_size st =
    List.fold_left
      (fun acc x -> acc + (match x with T.Double | T.Long -> 2 | _ -> 1))
      0
      st.stack

  let locals_size st = intmap_width st.locals

  let prev_next_label lcs =
    let hp, hn = H.create 13, H.create 13 in
    let rec f = function
      | (l, _) :: (((m, _) :: _) as lcs) ->
          H.add hp m l; H.add hn l m; f lcs
      | _ -> () in
    f lcs;
    let get h l = try H.find h l with Not_found -> HI.invalid_label in
    (get hp, get hn)

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
    match code.T.cv_code with
      | [] -> (H.create 2, H.create 2)
      | ((l, _) :: _) as cs ->
          let instruction_at =
            let h = H.create 13 in
            let f ((l, _) as c) = H.add h l c in
            List.iter f cs;
            fun l -> try H.find h l with Not_found -> fail T.SE_invalid_label in
          let _, next_label = prev_next_label cs in
          let handlers_at =
            let h = H.create 13 in
            let get l = try H.find h l with Not_found -> [] in
            let add l e = H.replace h l (e :: (get l)) in
            let rec f e l m = add m e; if l <> m then f e l (next_label m) in
            let g e = f e.T.catch e.T.try_end e.T.try_start in
            List.iter g code.T.cv_exception_table; get in
          let state = H.create 13 in
          let graph = H.create 13 in
          let record_state s l =
            try
              let t = H.find state l in
              let u = unify s t in
	      (if log_se && not (eq s u) then
		  let sl = Hashtbl.hash (s,l) in
		  let tl = Hashtbl.hash (t,l) in
                  let ul = Hashtbl.hash (u,l) in
		  printf "@\n@[%d [shape=box,label=\"%Ld\"];@]" sl l;
		  printf "@\n@[%d -> %d [color=blue];@]" sl ul;
		  printf "@\n@[%d [shape=box,label=\"%Ld\"];@]" tl l;
		  printf "@\n@[%d -> %d [color=blue];@]" tl ul);
              H.replace state l u;
              (u, not (eq u t))
            with Not_found ->
              (H.add state l s; (s, true)) in
          let exec exec' (_, step', s, l) = (* this is the main part *)
            if l = HI.invalid_label then fail T.SE_missing_return;
            let s, progress = record_state s l in
            if progress then begin
	      if log_se_full then pp_map_t std_formatter state;
              let t, ls = step' s (instruction_at l) (next_label l) in
              List.iter (fun l -> exec' ("black", step, t, l)) ls;
              let t = exec_throw t in
              List.iter (fun l -> exec' ("red", step, t, l)) (handlers_at l)
            end in
          let log_collisions =
            let eq (s, l) (t, m) = l = m && eq s t in
            let hash_codes = Hashtbl.create 13 in
            let rs (_, _, s, l) =
              let sl = (s, l) in
              let hc = Hashtbl.hash sl in
              try
                let tm = Hashtbl.find hash_codes hc in
                if not (eq sl tm) then
                  printf "@\n@[%d [style=fill,fillcolor=yellow];@]" hc
              with Not_found -> Hashtbl.add hash_codes hc sl in
            U.k_log rs in
          let log_state =
            let map_step step s (l, c) m =
              printf "@\n@[%d [shape=box,label=\"%Ld:%s:"
                (Hashtbl.hash (s, l)) l (HI.instruction_to_string c);
              let ls = step s (l, c) m in printf "\"];@]"; ls in
            U.k_map (fun (color, step, s, l) -> (color, map_step step, s, l)) in
          let log_edge =
            let pe ((_, _, s, l), (color, _, t, m)) =
              let sl = Hashtbl.hash (s, l) in
              let tm = Hashtbl.hash (t, m) in
              printf "@\n@[%d -> %d [color=%s];@]" sl tm color in
            U.k_log pe in
          let update_cfg =
            let re ((_, _, _, l), (_, _, _, m)) =
              let prev = try H.find graph m with Not_found -> LS.empty in
              H.replace graph m (LS.add l prev) in
            U.k_log re in
          let exec =
            if log_se
            then log_edge (U.k_successive (log_state (log_collisions exec)))
            else U.k_successive exec in
          let exec = update_cfg exec in
          let exec = U.k_y exec in
          exec (("", (fun _ -> failwith "what?"), init, HI.invalid_label),
                ("green", step, init, l));
          (graph, state)

  (* was StackState.update *)
  let step c_name st (lbl, i) next_lbl =
    let continue locals stack = ({stack; locals}, [next_lbl]) in
    let jump locals stack jump_lbls = ({stack; locals}, jump_lbls) in
    let jump2 locals stack jump_lbl = ({stack; locals}, [jump_lbl; next_lbl]) in
    let return locals stack = ({stack; locals}, []) in
    let locals = st.locals in
    let stack = st.stack in
    let state_contains bt =
      List.mem bt stack || U.IntMap.exists (fun _ v -> v = bt) locals in
    match i with
      | T.AALOAD ->
	let stack = pop_if T.Integer stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack =
          (match topv with
            | T.Null -> push T.Null stack
            | T.Object (`Array_type (`Array t)) -> push (bt_of_descriptor t) stack
            | _ -> fail (report_array_expected topv)) in
	continue locals stack
      | T.AASTORE ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = pop_if T.Integer stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | T.ACONST_NULL ->
	let stack = push T.Null stack in
	continue locals stack
      | T.ALOAD parameter ->
	let loc = load parameter locals in
	check_reference loc;
	let stack = push loc stack in
	continue locals stack
      | T.ANEWARRAY parameter ->
	let stack = pop_if T.Integer stack in
	let stack = push (verification_type_info_of_array_element parameter) stack in
	continue locals stack
      | T.ARETURN ->
	let stack = pop stack in
	return locals stack
      | T.ARRAYLENGTH ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.ASTORE parameter ->
	let loc = top stack in
	let stack = pop stack in
	check_reference_or_return loc;
	let locals = store parameter loc locals in
	continue locals stack
      | T.ATHROW ->
	let exc = top stack in
        check_reference exc;
	return locals stack
      | T.BALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.BASTORE ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.CALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.CASTORE ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.CHECKCAST parameter -> (* no check needed? *)
	let stack = pop stack in
	let stack = push (bt_of_descriptor (match parameter with `Array_type at -> (at :> Descriptor.for_parameter) | `Class_or_interface cn -> `Class cn)) stack in
	continue locals stack
      | T.D2F ->
	let stack = pop_if T.Double stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.D2I ->
	let stack = pop_if T.Double stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.D2L ->
	let stack = pop_if T.Double stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.DADD ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DASTORE ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.DCMPG ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.DCMPL ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.DCONST_0 ->
	let stack = push T.Double stack in
	continue locals stack
      | T.DCONST_1 ->
	let stack = push T.Double stack in
	continue locals stack
      | T.DDIV ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DLOAD parameter ->
	check_load parameter locals T.Double;
	let stack = push T.Double stack in
	continue locals stack
      | T.DMUL ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DNEG ->
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DREM ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DRETURN ->
	let stack = pop_if T.Double stack in
	return locals stack
      | T.DSTORE parameter ->
	let stack = pop_if T.Double stack in
	let locals = store parameter T.Double locals in
	continue locals stack
      | T.DSUB ->
	let stack = pop_if T.Double stack in
	let stack = pop_if T.Double stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.DUP ->
	let v, stack = pop_if_size 1 stack in
	let stack = push v stack in
	let stack = push v stack in
	continue locals stack
      | T.DUP2 ->
	let v1 = top stack in
	let stack = pop stack in
	let stack =
          if size_of_bt v1 = 1 then
            let v2, stack = pop_if_size 1 stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            push v1 (push v1 stack) in
	continue locals stack
      | T.DUP2_X1 ->
	let v1 = top stack in
	let stack =
          if size_of_bt v1 = 1 then
            let stack = pop stack in
            let v2, stack = pop_if_size 1 stack in
            let v3, stack = pop_if_size 1 stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            let stack = pop stack in
            let v2, stack = pop_if_size 1 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack in
	continue locals stack
      | T.DUP2_X2 ->
	let v1 = top stack in
	let stack =
          if size_of_bt v1 = 1 then begin
            let stack = pop stack in
            let v2, stack = pop_if_size 1 stack in
            let v3 = top stack in
            if size_of_bt v3 = 1 then begin
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
            if size_of_bt v2 = 1 then begin
              let stack = pop stack in
              let v3, stack = pop_if_size 1 stack in
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
      | T.DUP_X1 ->
	let v1, stack = pop_if_size 1 stack in
	let v2, stack = pop_if_size 1 stack in
	let stack = push v1 stack in
	let stack = push v2 stack in
	let stack = push v1 stack in
	continue locals stack
      | T.DUP_X2 ->
	let v1, stack = pop_if_size 1 stack in
	let v2 = top stack in
	let stack =
          if size_of_bt v2 = 1 then
            let v2, stack = pop_if_size 1 stack in
            let v3, stack = pop_if_size 1 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          else
            let v2, stack = pop_if_size 2 stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack in
	continue locals stack
      | T.F2D ->
	let stack = pop_if T.Float stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.F2I ->
	let stack = pop_if T.Float stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.F2L ->
	let stack = pop_if T.Float stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.FADD ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FASTORE ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.FCMPG ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.FCMPL ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.FCONST_0 ->
	let stack = push T.Float stack in
	continue locals stack
      | T.FCONST_1 ->
	let stack = push T.Float stack in
	continue locals stack
      | T.FCONST_2 ->
	let stack = push T.Float stack in
	continue locals stack
      | T.FDIV ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FLOAD parameter ->
	check_load parameter locals T.Float;
	let stack = push T.Float stack in
	continue locals stack
      | T.FMUL ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FNEG ->
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FREM ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.FRETURN ->
	let stack = pop_if T.Float stack in
	return locals stack
      | T.FSTORE parameter ->
	let stack = pop_if T.Float stack in
	let locals = store parameter T.Float locals in
	continue locals stack
      | T.FSUB ->
	let stack = pop_if T.Float stack in
	let stack = pop_if T.Float stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.GETFIELD (`Fieldref (_, _, desc)) ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push (bt_of_descriptor desc) stack in
	continue locals stack
      | T.GETSTATIC (`Fieldref (_, _, desc)) ->
	let stack = push (bt_of_descriptor desc) stack in
	continue locals stack
      | T.GOTO lbl ->
	jump locals stack [lbl]
      | T.I2B ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.I2C ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.I2D ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.I2F ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.I2L ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.I2S ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IADD ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IAND ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IASTORE ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.IDIV ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IF_ACMPEQ lbl ->
	let stack = pop stack in
	let stack = pop stack in
	jump2 locals stack lbl
      | T.IF_ACMPNE lbl ->
	let stack = pop stack in
	let stack = pop stack in
	jump2 locals stack lbl
      | T.IF_ICMPEQ lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IF_ICMPGE lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IF_ICMPGT lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IF_ICMPLE lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IF_ICMPLT lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IF_ICMPNE lbl ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFEQ lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFGE lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFGT lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFLE lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFLT lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFNE lbl ->
	let stack = pop_if T.Integer stack in
	jump2 locals stack lbl
      | T.IFNONNULL lbl ->
	let stack = pop stack in
	jump2 locals stack lbl
      | T.IFNULL lbl ->
	let stack = pop stack in
	jump2 locals stack lbl
      | T.IINC ii ->
	check_load ii.T.ii_var locals T.Integer;
	let locals = store ii.T.ii_var T.Integer locals in
	continue locals stack
      | T.ILOAD parameter ->
	check_load parameter locals T.Integer;
	let stack = push T.Integer stack in
	continue locals stack
      | T.IMUL ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.INEG ->
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.INSTANCEOF _ ->
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
  (*
    | T.INVOKEDYNAMIC (_, _, (params, ret)) ->
    let infos = List.rev_map bt_of_descriptor params in
    let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
    let topv = top stack in
    check_reference topv;
    let stack = pop stack in
    let stack = push_return_value ret stack in
    { locals; stack }
  *)
      | T.INVOKEINTERFACE (`Methodref (_, _, (params, ret))) ->
	let infos = List.rev_map bt_of_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	continue locals stack
      | T.INVOKESPECIAL (`Methodref (cn, mn, (params, ret))) ->
	let infos = List.rev_map bt_of_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	let locals, stack =
	(* TODO(rlp) understand what happens here *)
          if U.UTF8.equal Consts.class_constructor (Name.utf8_for_method mn) then
            match topv with
              | T.Uninitialized lbl ->
		let f = function
		  | T.Uninitialized lbl' when lbl = lbl' -> T.Object cn
		  | x -> x in
		U.IntMap.map f locals, List.map f stack
              | T.Uninitialized_this ->
		let f = function
                  | T.Uninitialized_this -> T.Object (`Class_or_interface c_name)
                  | x -> x in
		U.IntMap.map f locals, List.map f stack
              | _ -> locals, stack
          else
            locals, stack in
	continue locals stack
      | T.INVOKESTATIC (`Methodref (_, _, (params, ret))) ->
	let infos = List.rev_map bt_of_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let stack = push_return_value ret stack in
	continue locals stack
      | T.INVOKEVIRTUAL (`Methodref (_, _, (params, ret))) ->
	let infos = List.rev_map bt_of_descriptor params in
	let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	let stack = push_return_value ret stack in
	continue locals stack
      | T.IOR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IREM ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IRETURN ->
	let stack = pop_if T.Integer stack in
	return locals stack
      | T.ISHL ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.ISHR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.ISTORE parameter ->
	let stack = pop_if T.Integer stack in
	let locals = store parameter T.Integer locals in
	continue locals stack
      | T.ISUB ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IUSHR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.IXOR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.JSR lbl ->
        let stack = push (T.Return_address (LS.singleton next_lbl)) stack in
	jump locals stack [lbl]
      | T.L2D ->
	let stack = pop_if T.Long stack in
	let stack = push T.Double stack in
	continue locals stack
      | T.L2F ->
	let stack = pop_if T.Long stack in
	let stack = push T.Float stack in
	continue locals stack
      | T.L2I ->
	let stack = pop_if T.Long stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.LADD ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LAND ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LASTORE ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.LCMP ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.LCONST_0 ->
	let stack = push T.Long stack in
	continue locals stack
      | T.LCONST_1 ->
	let stack = push T.Long stack in
	continue locals stack
      | T.LDC parameter ->
	let stack = push (verification_type_info_of_constant_descriptor parameter) stack in
	continue locals stack
      | T.LDIV ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LLOAD parameter ->
	check_load parameter locals T.Long;
	let stack = push T.Long stack in
	continue locals stack
      | T.LMUL ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LNEG ->
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LOOKUPSWITCH {T.ls_def; ls_branches} ->
	let stack = pop_if T.Integer stack in
        let targets = ls_def :: (List.map snd ls_branches) in
	jump locals stack targets
      | T.LOR ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LREM ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LRETURN ->
	let stack = pop_if T.Long stack in
	return locals stack
      | T.LSHL ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LSHR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LSTORE parameter ->
	let stack = pop_if T.Long stack in
	let locals = store parameter T.Long locals in
	continue locals stack
      | T.LSUB ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LUSHR ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.LXOR ->
	let stack = pop_if T.Long stack in
	let stack = pop_if T.Long stack in
	let stack = push T.Long stack in
	continue locals stack
      | T.MONITORENTER ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | T.MONITOREXIT ->
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | T.MULTIANEWARRAY (at, dims) ->
	let s = ref stack in
	for i = 1 to (dims :> int) do
          s := pop_if T.Integer !s;
	done;
	let stack = push (T.Object at) !s in
	continue locals stack
      | T.NEW _ ->
      (* TODO(rlp) understand why the argument to NEW is thrown away *)
        let bt = T.Uninitialized lbl in
        if state_contains bt then fail T.SE_double_new;
	let stack = push bt stack in
	continue locals stack
      | T.NEWARRAY parameter ->
	let stack = pop_if T.Integer stack in
	let stack = push (verification_type_info_of_array_primitive parameter) stack in
	continue locals stack
      | T.NOP ->
	continue locals stack
      | T.POP ->
	let _, stack = pop_if_size 1 stack in
	continue locals stack
      | T.POP2 ->
	let v1 = top stack in
	let stack =
          if size_of_bt v1 = 1 then
            snd (pop_if_size 1 (snd (pop_if_size 1 stack)))
          else
            pop stack in
	continue locals stack
      | T.PUTFIELD (`Fieldref (_, _, desc)) ->
	let stack = pop_if (bt_of_descriptor desc) stack in
	let topv = top stack in
	check_reference topv;
	let stack = pop stack in
	continue locals stack
      | T.PUTSTATIC (`Fieldref (_, _, desc)) ->
	let stack = pop_if (bt_of_descriptor desc) stack in
	continue locals stack
      | T.RET index ->
        let lbls = (match load index locals with
          | T.Return_address lbls -> LS.elements lbls
          | v -> fail (report_invalid_local_contents
              (index, v, T.Return_address LS.empty))) in
        jump locals stack lbls
      | T.RETURN ->
	return locals stack
      | T.SALOAD ->
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	let stack = push T.Integer stack in
	continue locals stack
      | T.SASTORE ->
	let stack = pop_if T.Integer stack in
	let stack = pop_if T.Integer stack in
	let stack = pop stack in
	continue locals stack
      | T.SIPUSH _ ->
	let stack = push T.Integer stack in
	continue locals stack
      | T.SWAP ->
	let v1, stack = pop_if_size 1 stack in
	let v2, stack = pop_if_size 1 stack in
	let stack = push v1 stack in
	let stack = push v2 stack in
	continue locals stack
      | T.TABLESWITCH {T.ts_def; ts_ofss; _} ->
	let stack = pop_if T.Integer stack in
        let targets = ts_def :: ts_ofss in
	jump locals stack targets
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
      | T.Top, _
      | _, T.Top -> T.Top
      | (T.Object o1), (T.Object o2) -> T.Object (f o1 o2)
      | T.Null, (T.Object _) -> vti2
      | (T.Object _), T.Null -> vti1
      | (T.Return_address l1), (T.Return_address l2) -> T.Return_address (LS.union l1 l2)
      | _ -> if vti1 = vti2 then vti1 else T.Top in
    let sz1 = List.length st1.stack in
    let sz2 = List.length st2.stack in
    if sz1 = sz2 then begin
      let locals =
        U.IntMap.merge
        (fun _ o1 o2 -> match o1, o2 with
        | Some s1, Some s2 -> Some (unify_elements s1 s2)
        | Some _, None | None, Some _ -> Some T.Top
        | None, None -> None)
        st1.locals st2.locals in
      let stack = List.map2 unify_elements st1.stack st2.stack in
      { locals; stack }
    end else
      fail (T.SE_different_stack_sizes (sz1, sz2))
  (* }}} *)

  (* TODO(rlp): it appears he pads the stack for big values *)
  (* our symbolic execution should be size agnostic and not need that *)
  let of_list l =
    let l =
      List.map
	(function
          | T.Long ->
            [T.Long; T.Top]
          | T.Double ->
            [T.Double; T.Top]
          | x -> [x])
	l in
    let l = List.concat l in
    let fold (m, i) v = (U.IntMap.add i v m, succ i) in
    let (m, _) = List.fold_left fold (U.IntMap.empty, 0) l in
    m

  (* Makes sure exception handlers refer only to live labels. *)
  let adjust_exception_table is_live c =
    let prev, next = prev_next_label c.T.cv_code in
    let rec advance until next l =
      if is_live l || l = until then l else advance until next (next l) in
    let adjust_handler h =
      let l = advance h.T.try_end next h.T.try_start in
      let r = advance l prev h.T.try_end in
      if not (is_live h.T.catch) || (l = r && not (is_live l))
      then None
      else Some { h with T.try_start = l; try_end = r } in
    U.map_partial adjust_handler c.T.cv_exception_table

  (* public *) (* {{{ *)
  let compute_max_stack_locals c_name m c =
    let step = step c_name in
    let init = { locals = of_list (locals_of_method c_name m); stack = [] } in
    let exec_throw s = { s with stack = [T.Object java_lang_Object] } in
    (* TODO(rlp) is this the correct unification? *)
    let unify_states = unify unify_to_java_lang_Object in
    let cfg, smfs =
      execute_method step exec_throw unify_states equal_t init c in
    let count_incoming l =
      try LS.cardinal (H.find cfg l) with Not_found -> 0 in
    let first_label =
      match c.T.cv_code with [] -> assert false | (l, _) :: _ -> l in
    let stripped_smfs = H.create 13 in
    let live_labels = ref LS.empty in
    let f l s (ms, ml) =
      let count = count_incoming l in
      if l = first_label || true(*count > 1*) then H.add stripped_smfs l s;
      if l = first_label || count > 0 then live_labels := LS.add l !live_labels;
      (max ms (stack_size s), max ml (locals_size s)) in
    let max_stack, max_locals = H.fold f smfs (0, 0) in
    let cv_code = List.filter (fun (l,_) -> LS.mem l !live_labels) c.T.cv_code in
    let cv_exception_table =
      adjust_exception_table (fun l -> LS.mem l !live_labels) c in
    let c = { c with T.cv_code; cv_exception_table } in
    (c, stripped_smfs, max_stack, max_locals)
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
      | _ -> fail T.Invalid_module in
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
      { Attribute.name_index = name;
	length = len;
	data = dat; }

  let name_of_attribute (a : T.attribute) = U.UTF8.to_string (match a with
    | `AnnotationDefault _ -> Consts.attr_annotation_default
    | `BootstrapMethods _ -> attr_bootstrap_methods
    | `ClassSignature _ -> attr_signature
    | `Code _ -> attr_code
    | `ConstantValue _ -> attr_constant_value
    | `Deprecated -> attr_deprecated
    | `EnclosingMethod _ -> attr_enclosing_method
    | `Exceptions _ -> attr_exceptions
    | `FieldSignature _ -> attr_signature
    | `IgnoredAttribute -> attr_ignored
    | `InnerClasses _ -> attr_inner_classes
    | `LineNumberTable _ -> attr_line_number_table
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

  let check_code_attribute = function
    | #T.code_attribute as g -> g
    | b -> fail (T.Misplaced_attribute (name_of_attribute b, "code"))

  let option_of_u2 f (i : U.u2) =
    if (i :> int) = 0 then None else Some (f i)

  (* actual decoders for attributes *)

  type decoding_arguments =
    { da_element : enclosing_element
    ; da_pool : CP.t
    ; da_i : Attribute.info }

  let decode_attr_constant_value _ r st =
    let i = IS.read_u2 st in
    `ConstantValue (HC.decode_field r.da_pool i)

  let enclosing_arguments = function
    | EE_method (c_name, m) -> List.map fuzzy_bt (locals_of_method c_name m)
    | _ -> failwith "TODO: Does this happen?"

  let local_of_instruction = function
    | T.ALOAD x | T.ASTORE x | T.DLOAD x | T.DSTORE x | T.FLOAD x
    | T.FSTORE x | T.IINC { T.ii_var = x; _ } | T.ILOAD x | T.ISTORE x
    | T.LLOAD x | T.LSTORE x | T.RET x
        -> Some x
    | _ -> None

  let set_local i = function
    | T.ALOAD _ -> T.ALOAD i
    | T.ASTORE _ -> T.ASTORE i
    | T.DLOAD _ -> T.DLOAD i
    | T.DSTORE _ -> T.DSTORE i
    | T.FLOAD _ -> T.FLOAD i
    | T.FSTORE _ -> T.FSTORE i
    | T.IINC a -> T.IINC {a with T.ii_var = i}
    | T.ILOAD _ -> T.ILOAD i
    | T.ISTORE _ -> T.ISTORE i
    | T.LLOAD _ -> T.LLOAD i
    | T.LSTORE _ -> T.LSTORE i
    | T.RET _ -> T.RET i
    | _ -> failwith "INTERNAL: Trying to set local on an opcode with no local."

  let bt_of_instruction = function
    | T.ALOAD _ | T.ASTORE _ | T.RET _ -> Some T.Reference
    | T.DLOAD _ | T.DSTORE _ -> Some T.Double
    | T.FLOAD _ | T.FSTORE _ -> Some T.Float
    | T.IINC _ | T.ILOAD _ | T.ISTORE _ -> Some T.Integer
    | T.LLOAD _ | T.LSTORE _ -> Some T.Long
    | _ -> None

  let map_local_access f g x =
    match local_of_instruction x, bt_of_instruction x with
      | Some k, Some t -> f x k t
      | None, None -> g x
      | _ -> failwith "INTERNAL"

  let update_locals f =
    let update x k t = set_local (f (k, t)) x in
    List.map (map_local_access update (fun x -> x))

  let decode_locals arguments code =
    try
      let m = Hashtbl.create 0 in (* old (index, type) -> new index *)
      let sz = ref 0 in
      let fresh t = let r = !sz in sz := succ r; r in
      let record_access k t =
        if not (Hashtbl.mem m (k, t)) then
          Hashtbl.add m (k, t) (fresh t) in
      let record_argument k t = record_access k t; k + size_of_bt t in
      let labels, code = List.split code in
      ignore (List.fold_left record_argument 0 arguments);
      List.iter (map_local_access (fun _ -> record_access) (fun _ -> ())) code;
      let code = update_locals (Hashtbl.find m) code in
      let code = List.combine labels code in
      let add_type (_, t) i = U.IntMap.add i t in
      let types = Hashtbl.fold add_type m U.IntMap.empty in
      (code, types)
    with _ ->
      failwith "INTERNAL: decode_locals"

  let decode_attr_code decode r st =
    let (*mx_stack*) _ = IS.read_u2 st in
    let (*max_locals*) _ = IS.read_u2 st in
    let code_len' = IS.read_u4 st in
    let code_len =
      let x = (code_len' : Utils.u4 :> int64) in
      if 0L < x && x < 65536L then
        Int64.to_int x
      else
        fail T.Invalid_code_length in
    let code_content = IS.read_bytes st code_len in
    let code_stream = IS.make_of_string code_content in
    let instr_codes = BC.read code_stream 0 in
    let ofs_to_label, ofs_to_prev_label =
      let rec f o ps = function (* note the sentinel added at the end *)
        | [] -> Array.of_list (List.rev ((o, HI.invalid_label) :: ps))
        | s :: ss -> f (o + BC.size_of o s) ((o, fresh_label ()) :: ps) ss in
      let a = f 0 [] instr_codes in
      let n = Array.length a in
      assert (n > 1); (* see Invalid_code_length check above *)
      let rec bs o l h =
        if l + 1 = h then
          (if fst a.(l) = o then l else fail T.Invalid_offset)
        else begin
          let m = l + (h - l) / 2 in
          if fst a.(m) <= o then bs o m h else bs o l m
        end in
      ( (fun o -> snd a.(bs o 0 (pred n))),
        (fun o -> snd a.(pred (bs o 1 n))) ) in
    let cv_code =
      let f (o, ss) s =
        (o + BC.size_of o s,
          (ofs_to_label o, HI.decode r.da_pool ofs_to_label o s) :: ss) in
      let _, r = List.fold_left f (0, []) instr_codes in
      List.rev r in
    let cv_code, cv_type_of_local =
      decode_locals (enclosing_arguments r.da_element) cv_code in
    let cv_exception_table =
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
          { T.try_start = u2_ofs_to_label start_pc
          ; try_end = ofs_to_prev_label (end_pc : Utils.u2 :> int)
          ; catch = u2_ofs_to_label handler_pc
          ; caught = catch_type; }) in
    let cv_attributes = IS.read_elements
        st
        (fun st -> decode { r with da_i = read_info st }) in
    let update_lnt = function
      | `LineNumberTable h ->
          let r = T.LabelHash.create 13 in
          let f ofs ln =
            let label = ofs_to_label (Int64.to_int ofs) in
            assert (not (T.LabelHash.mem r label));
            T.LabelHash.add r label ln in
          T.LabelHash.iter f h;
          `LineNumberTable r
      | x -> x in
    let cv_attributes = List.map check_code_attribute cv_attributes in
    let cv_attributes = List.map update_lnt cv_attributes in
    `Code { T.cv_code; cv_exception_table; cv_attributes; cv_type_of_local }

  let decode_attr_exceptions _ r st : T.attribute =
    let f st = CP.get_class_name r.da_pool (IS.read_u2 st) in
    `Exceptions (IS.read_elements st f)

  let decode_attr_inner_classes _ r st : T.attribute =
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
      { T.inner_class; outer_class; inner_name; inner_flags } in
    `InnerClasses (IS.read_elements st one)

  let decode_attr_enclosing_method _ r st : T.attribute =
    let f i = match CP.get_entry r.da_pool i with
      | CP.NameAndType (name, desc) ->
          let utf8 = CP.get_utf8_entry r.da_pool in
          (Name.make_for_method (utf8 name),
            Descriptor.method_of_utf8 (utf8 desc))
      | _ -> fail T.Invalid_enclosing_method in
    let innermost_class = IS.read_u2 st in
    let enclosing_method = IS.read_u2 st in
    let innermost_class = CP.get_class_name r.da_pool innermost_class in
    let enclosing_method = option_of_u2 f enclosing_method in
    `EnclosingMethod { T.innermost_class; enclosing_method }

  let decode_attr_synthetic _ _ _ : T.attribute =
    `Synthetic

  let decode_attr_signature _ r st : T.attribute =
    let signature_index = IS.read_u2 st in
    let s = CP.get_utf8_entry r.da_pool signature_index in
    match r.da_element with
      | EE_class -> `ClassSignature (Signature.class_signature_of_utf8 s)
      | EE_method _ -> `MethodSignature (Signature.method_signature_of_utf8 s)
      | EE_field -> `FieldSignature (Signature.field_type_signature_of_utf8 s)

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
    let h = hash_of_list T.LabelHash.create T.LabelHash.replace h in
    `LineNumberTable h

  let decode_attr_deprecated _ _ _ : T.attribute = `Deprecated

  let decode_attr_runtime_visible_annotations _ r st : T.attribute =
    `RuntimeVisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_invisible_annotations _ r st : T.attribute =
    `RuntimeInvisibleAnnotations (read_annotations r.da_pool st)
  let decode_attr_runtime_visible_parameter_annotations _ r st : T.attribute =
    `RuntimeVisibleParameterAnnotations (read_annotations_list r.da_pool st)
  let decode_attr_runtime_invisible_parameter_annotations _ r st : T.attribute =
    `RuntimeInvisibleParameterAnnotations (read_annotations_list r.da_pool st)

  let decode_attr_runtime_visible_type_annotations _ = failwith "todo:decode_attr_runtime_visible_type_annotations"
  let decode_attr_runtime_invisible_type_annotations _ = failwith "todo:decode_attr_runtime_invisible_type_annotations"

  let decode_attr_annotation_default _ r st : T.attribute =
    let eiv = Annotation.read_info_element_value st in
    `AnnotationDefault (Annotation.decode_element_value r.da_pool eiv)

  (* Not needed. *)
  let decode_attr_stack_map_table _ _ _ : T.attribute = `IgnoredAttribute

  (* See issue #14. *)
  let decode_attr_local_variable_table _ _ _ : T.attribute =
    `IgnoredAttribute
  let decode_attr_local_variable_type_table _ _ _ : T.attribute =
    `IgnoredAttribute

  let decode_attr_bootstrap_methods _ = failwith "todo:decode_attr_bootstrap_methods"
  let decode_attr_module _ = failwith "todo:decode_attr_module"
  let decode_attr_module_requires _ = failwith "todo:decode_attr_module_requires"
  let decode_attr_module_permits _ = failwith "todo:decode_attr_module_permits"
  let decode_attr_module_provides _ = failwith "todo:decode_attr_module_provides"

  module UTF8Hashtbl = Hashtbl.Make (Utils.UTF8)

  let decoders :
      ((decoding_arguments -> T.attribute) ->
        decoding_arguments -> IS.t -> T.attribute)
      UTF8Hashtbl.t =
    let ds =
      [ attr_annotation_default, decode_attr_annotation_default
      ; attr_bootstrap_methods, decode_attr_bootstrap_methods
      ; attr_code, decode_attr_code
      ; attr_constant_value, decode_attr_constant_value
      ; attr_deprecated, decode_attr_deprecated
      ; attr_enclosing_method, decode_attr_enclosing_method
      ; attr_exceptions, decode_attr_exceptions
      ; attr_inner_classes, decode_attr_inner_classes
      ; attr_line_number_table, decode_attr_line_number_table
      ; attr_local_variable_table, decode_attr_local_variable_table
      ; attr_local_variable_type_table, decode_attr_local_variable_type_table
      ; attr_module, decode_attr_module
      ; attr_module_permits, decode_attr_module_permits
      ; attr_module_provides, decode_attr_module_provides
      ; attr_module_requires, decode_attr_module_requires
      ; attr_runtime_invisible_annotations, decode_attr_runtime_invisible_annotations
      ; attr_runtime_invisible_parameter_annotations, decode_attr_runtime_invisible_parameter_annotations
      ; attr_runtime_invisible_type_annotations, decode_attr_runtime_invisible_type_annotations
      ; attr_runtime_visible_annotations, decode_attr_runtime_visible_annotations
      ; attr_runtime_visible_parameter_annotations, decode_attr_runtime_visible_parameter_annotations
      ; attr_runtime_visible_type_annotations, decode_attr_runtime_visible_type_annotations
      ; attr_signature, decode_attr_signature
      ; attr_source_debug_extension, decode_attr_source_debug_extension
      ; attr_source_file, decode_attr_source_file
      ; attr_stack_map_table, decode_attr_stack_map_table
      ; attr_synthetic, decode_attr_synthetic ] in
    hash_of_list UTF8Hashtbl.create UTF8Hashtbl.add ds

  (* knot tying and visible functions *)

  let rec decode r =
    let st = IS.make_of_string r.da_i.Attribute.data in
    let attr_name =
      CP.get_utf8_entry r.da_pool r.da_i.Attribute.name_index in
    try UTF8Hashtbl.find decoders attr_name decode r st
    with Not_found -> `Unknown (attr_name, r.da_i.Attribute.data)

  let decode_class da_pool da_i =
    match decode { da_element = EE_class; da_pool; da_i } with
      | #T.class_attribute as g -> g
      | b -> fail (T.Misplaced_attribute (name_of_attribute b, "class"))

  let decode_field da_pool da_i =
    match decode { da_element = EE_field; da_pool; da_i } with
      | #T.field_attribute as g -> g
      | b -> fail (T.Misplaced_attribute (name_of_attribute b, "field"))

  let decode_method c_name m da_pool da_i =
    match decode { da_element = EE_method (c_name, m); da_pool; da_i } with
      | #T.method_attribute as g -> g
      | b -> fail (T.Misplaced_attribute (name_of_attribute b, "method"))

  let rec version_bounds : T.attribute -> Version.bounds = function
    | `AnnotationDefault _ ->
        Version.make_bounds "'AnnotationDefault' attribute" Version.Java_1_5 None
    | `BootstrapMethods _ ->
        Version.make_bounds "'BootstrapMethods' attribute" Version.Java_1_7 None
    | `ClassSignature _ ->
        Version.make_bounds "'ClassSignature' attribute" Version.Java_1_5 None
    | `Code cv ->
        let instrs_bounds = List.map HI.version_bounds cv.T.cv_code in
        let attrs_bounds = List.map version_bounds (cv.T.cv_attributes :> T.attribute list) in
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
    | `IgnoredAttribute ->
        Version.make_bounds "ignored attribute" Version.Java_1_0 None
    | `InnerClasses _ ->
        Version.make_bounds "'InnerClasses' attribute" Version.Java_1_1 None
    | `LineNumberTable _ ->
        Version.make_bounds "'LineNumberTable' attribute" Version.Java_1_0 None
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

  (* PRE: ool is injective *)
  let encode_attr_stackmaptable (ool : T.label -> int) pool m : A.info =
    try
      let enc = make_encoder pool 16 in
      let full_frame l s acc =
        ( ool l
        , SE.encode_locals s.SE.locals
        , List.rev s.SE.stack )
        :: acc in
      let smt = T.LabelHash.fold full_frame m [] in
      let smt = List.sort compare smt in
      let write_smf previous (o, locals, stack) =
        let write_bt = function
          | T.Reference -> failwith "INTERNAL(unego): type should be more specific"
          | T.Top -> OS.write_u1 enc.en_st (U.u1 0)
          | T.Return_address _ (* TODO(rgrig): check whether this is OK. *)
          | T.Integer -> OS.write_u1 enc.en_st (U.u1 1)
          | T.Float -> OS.write_u1 enc.en_st (U.u1 2)
          | T.Double -> OS.write_u1 enc.en_st (U.u1 3)
          | T.Long -> OS.write_u1 enc.en_st (U.u1 4)
          | T.Null -> OS.write_u1 enc.en_st (U.u1 5)
          | T.Uninitialized_this -> OS.write_u1 enc.en_st (U.u1 6)
          | T.Object d ->
              OS.write_u1 enc.en_st (U.u1 7);
              let idx = match d with
                | `Class_or_interface u -> CP.add_class enc.en_pool u
                | `Array_type t -> CP.add_array_class enc.en_pool t in
              OS.write_u2 enc.en_st idx
          | T.Uninitialized l ->
              OS.write_u1 enc.en_st (U.u1 8);
              OS.write_u2 enc.en_st (U.u2 (ool l)) in
        let write_list l =
          OS.write_u2 enc.en_st (U.u2 (List.length l));
          List.iter write_bt l in
        let o = match previous with None -> 0 | Some (o',_,_) -> o-o'-1 in
        let same bt =
          assert (0 <= o);
          let e = 64 <= o in    (* extended *)
          let s = bt <> None in (* with stack *)
          let frame_type = match e, s with
            | false, false -> o
            | false, true -> o + 64
            | true, false -> 251
            | true, true -> 247 in
          OS.write_u1 enc.en_st (U.u1 frame_type);
          if e then OS.write_u2 enc.en_st (U.u2 o);
          if s then write_bt (U.from_some bt) in
        let diff (k, vis) =
          assert (-4 < k && k < 4);
          assert (k <> 0);
          assert (k < 0 || k = List.length vis);
          OS.write_u1 enc.en_st (U.u1 (k + 251));
          OS.write_u2 enc.en_st (U.u2 o);
          List.iter write_bt vis in
        let full () =
          OS.write_u1 enc.en_st (U.u1 255);
          OS.write_u2 enc.en_st (U.u2 o);
          write_list locals;
          write_list stack in
        let rec ldiff xs ys = match xs, ys with
          | x :: xs, y :: ys when x = y -> ldiff xs ys
          | xs, ys -> (xs, ys) in
        let dist xs ys = match ldiff xs ys with
          | ([], zs) | (zs, []) -> List.length zs
          | _ -> max_int in
        let grow_info xs ys = match ldiff xs ys with
          | [], zs -> (List.length zs, zs)
          | zs, [] -> (-(List.length zs), [])
          | _ -> assert false in
        (match previous, stack with
          | Some (_,l,_), [] when dist l locals = 0 -> same None
          | Some (_,l,_), [x] when dist l locals = 0 -> same (Some x)
          | Some (_,l,_), []  when dist l locals < 4 ->
              diff (grow_info l locals)
          | _ -> full ()) in
      let write_smf p n = write_smf p n; Some n in
      OS.write_u2 enc.en_st (U.u2 (List.length smt));
      ignore (List.fold_left write_smf None smt);
      enc_return enc attr_stack_map_table
    with _ -> fail T.Invalid_stack_map_table

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
    let m = T.LabelHash.create 131 in
    let fold ofs (lbl, bc) =
      (* could check for clashes here *)
      let next_ofs = ofs + (BC.size_of ofs bc) in
      T.LabelHash.add m lbl (ofs, next_ofs);
      next_ofs in
    ignore (List.fold_left fold 0 bcl);
    m

  let encode_instr_list pool l =
    let get f h x = f (T.LabelHash.find h x) in
    let rec fix_map m bcl =
      let m' = compute_ofs_map bcl in
      let same =
        let f k v same = same && m k = v in
        T.LabelHash.fold f m' true in
      if same then (get fst m', get snd m', List.map snd bcl)
      else
	let bcl' = i_list_to_labeled_bc_list (get fst m') pool l in
	fix_map (T.LabelHash.find m') bcl' in
    let bcl = i_list_to_labeled_bc_list (fun _ -> 0) pool l in
    fix_map (fun _ -> (0,0)) bcl

  let encode_locals arguments code =
    let record_use m k t =
      let ov = try U.IntMap.find k m with Not_found -> 0 in
      U.IntMap.add k (max ov (size_of_bt t)) m in
    let record_arg (m, k) t = (record_use m k t, succ k) in
    let record_instruction m =
      map_local_access (fun _ -> record_use m) (fun _ -> m) in
    let labels, code = List.split code in
    let m, _ = List.fold_left record_arg (U.IntMap.empty, 0) arguments in
    let m = List.fold_left record_instruction m code in
    let new_index =
      let h = Hashtbl.create 0 in
      let f o sz n =
        assert (sz > 0);
        Hashtbl.add h o n;
        n + sz in
      ignore (U.IntMap.fold f m 0);
      fun (k, _) -> Hashtbl.find h k in
    let code = update_locals new_index code in
    List.combine labels code

  let rec encode_attr_code env enc encode c =
    let c_name, m = U.from_some env in
    let arguments = List.map fuzzy_bt (locals_of_method c_name m) in
    let c = { c with T.cv_code = encode_locals arguments c.T.cv_code } in
    let c, stackmap, max_stack, max_locals =
      SE.compute_max_stack_locals c_name m c in
    let label_to_ofs, label_to_next_ofs, code_content =
      encode_instr_list enc.en_pool c.T.cv_code in
    let code_enc = make_encoder enc.en_pool 16 in
    BC.write code_enc.en_st 0 code_content;
    OS.close code_enc.en_st;
    let actual_code = Buffer.contents code_enc.en_buffer in
    OS.write_u2 enc.en_st (U.u2 max_stack);
    OS.write_u2 enc.en_st (U.u2 max_locals);
    let code_length = String.length actual_code in
    if code_length > U.max_u2 then fail T.Invalid_code_length;
    OS.write_u4 enc.en_st (U.u4 (Int64.of_int code_length));
    OS.write_bytes enc.en_st actual_code;
    OS.write_elements
      (checked_length "Exceptions")
      enc.en_st
      (fun st elem ->
        let catch_idx = match elem.T.caught with
        | Some exn_name -> CP.add_class enc.en_pool exn_name
        | None -> U.u2 0 in
        OS.write_u2 st (U.u2 (label_to_ofs elem.T.try_start));
        OS.write_u2 st (U.u2 (label_to_next_ofs elem.T.try_end));
        OS.write_u2 st (U.u2 (label_to_ofs elem.T.catch));
        OS.write_u2 st catch_idx)
      c.T.cv_exception_table;
    let len = checked_length "Attributes" c.T.cv_attributes in
    let len = U.u2 (succ (len : U.u2 :> int)) in
    OS.write_u2 enc.en_st len;
    let sub_enc = make_encoder enc.en_pool 16 in
    List.iter
      (fun a ->
        let res = encode env sub_enc.en_pool (a :> T.attribute) in
        A.write_info sub_enc.en_st res)
      c.T.cv_attributes;
    let res = encode_attr_stackmaptable label_to_ofs enc.en_pool stackmap in
    A.write_info sub_enc.en_st res;
    OS.close sub_enc.en_st;
    OS.write_bytes enc.en_st (Buffer.contents sub_enc.en_buffer);
    enc_return enc attr_code

  let encode_attr_constant_value enc (c : T.constant_field) =
    let c = HC.encode_field enc.en_pool c in
    OS.write_u2 enc.en_st c;
    assert (Buffer.length enc.en_buffer = 2);
    enc_return enc attr_constant_value

  let encode_attr_deprecated enc = enc_return enc attr_deprecated

  let encode_attr_enclosing_method enc { T.innermost_class; enclosing_method } =
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
        (fun st { T.inner_class; outer_class; inner_name; inner_flags } ->
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
  let encode_attr_line_number_table =
    write_empty_list "line numbers" attr_line_number_table

  let encode_attr_local_variable_table =
    write_empty_list "local variables" attr_local_variable_table

  let encode_attr_local_variable_type_table =
    write_empty_list "local types" attr_local_variable_type_table

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
  let encode_attr_ignored enc =
      enc_return enc attr_ignored

  type encode_env = (Name.for_class * T.method_) option

  let rec encode (env : encode_env) pool : T.attribute -> A.info = fun x ->
    let encoder = make_encoder pool 64 in
  match x with
    | `AnnotationDefault ev -> encode_attr_annotation_default encoder ev
    | `BootstrapMethods _ -> encode_attr_bootstrap_methods ()
    | `ClassSignature s -> encode_attr_class_signature encoder s
    | `Code c -> encode_attr_code env encoder encode c
    | `ConstantValue v -> encode_attr_constant_value encoder v
    | `Deprecated -> encode_attr_deprecated encoder
    | `EnclosingMethod em -> encode_attr_enclosing_method encoder em
    | `Exceptions l -> encode_attr_exceptions encoder l
    | `FieldSignature s -> encode_attr_field_signature encoder s
    | `IgnoredAttribute -> encode_attr_ignored encoder
    | `InnerClasses l -> encode_attr_inner_classes encoder l
    | `LineNumberTable _ -> encode_attr_line_number_table encoder
    | `MethodSignature s -> encode_attr_method_signature encoder s
    | `Module _ -> encode_attr_module ()
    | `RuntimeInvisibleAnnotations l -> encode_attr_runtime_invisible_annotations encoder l
    | `RuntimeInvisibleParameterAnnotations l -> encode_attr_runtime_invisible_parameter_annotations encoder l
    | `RuntimeInvisibleTypeAnnotations l -> encode_attr_runtime_invisible_type_annotations encoder l
    | `RuntimeVisibleAnnotations l -> encode_attr_runtime_visible_annotations encoder l
    | `RuntimeVisibleParameterAnnotations l -> encode_attr_runtime_visible_parameter_annotations encoder l
    | `RuntimeVisibleTypeAnnotations l -> encode_attr_runtime_visible_type_annotations encoder l
    | `SourceDebugExtension sde -> encode_attr_source_debug_extension encoder sde
    | `SourceFile sf -> encode_attr_source_file encoder sf
    | `Synthetic -> encode_attr_synthetic encoder
    | `Unknown u -> encode_attr_unknown encoder u

  let encode_class pool a =
    encode None pool (a : T.class_attribute :> T.attribute)
  let encode_field pool a =
    encode None pool (a : T.field_attribute :> T.attribute)
  let encode_method c_name m pool a =
    encode (Some (c_name, m)) pool (a : T.method_attribute :> T.attribute)
end
(* }}} *)
module HAO = HighAttributeOps
module HighMethodOps = struct (* {{{ *)
  let make_ClinitMethod flags _ =
    let cm_flags = AccessFlag.check_initializer_flags flags in
    T.ClinitMethod { T.cm_flags; cm_attributes = [] }

  let make_InitMethod (im_descriptor,_) flags _ =
    let im_flags = AccessFlag.check_constructor_flags flags in
    T.InitMethod { T.im_flags; im_descriptor; im_attributes = [] }

  let make_RegularMethod i rm_name rm_descriptor flags _ =
    let rm_flags = AccessFlag.check_method_flags i flags in
    T.RegularMethod { T.rm_flags; rm_name; rm_descriptor; rm_attributes = [] }

  let set_method_attributes xs = function
    | T.RegularMethod m -> T.RegularMethod { m with T.rm_attributes = xs }
    | T.InitMethod m -> T.InitMethod { m with T.im_attributes = xs }
    | T.ClinitMethod m -> T.ClinitMethod { m with T.cm_attributes = xs }

  let decode c_name is_interface pool m =
    let utf8 = CP.get_utf8_entry pool in (* (local) short name *)
    let utf8_name = utf8 m.M.name_index in
    let n = Name.make_for_method utf8_name in
    let d = Descriptor.method_of_utf8 (utf8 m.M.descriptor_index) in
    let fs = AccessFlag.from_u2 true m.M.access_flags in
    let hm = U.switch U.UTF8.equal
      [ class_initializer, make_ClinitMethod fs
      ; class_constructor, make_InitMethod d fs ]
      (make_RegularMethod is_interface n d fs)
      utf8_name in
    let decode_attribute = HAO.decode_method c_name hm pool in
    let attributes =
      U.map_array_to_list decode_attribute m.M.attributes_array in
    set_method_attributes attributes hm

  let encode c_name pool m =
    let flags, name, desc, attrs = match m with
      | T.RegularMethod m ->
	m.T.rm_flags,
	m.T.rm_name,
	m.T.rm_descriptor,
	m.T.rm_attributes
      | T.InitMethod m ->
	(m.T.im_flags :> AF.for_method list),
	(Name.make_for_method class_constructor),
	(m.T.im_descriptor, `Void),
	m.T.im_attributes
      | T.ClinitMethod m ->
	(m.T.cm_flags :> AF.for_method list),
	(Name.make_for_method class_initializer),
	([], `Void),
	m.T.cm_attributes in
    let acc_flags = AF.list_to_u2 (flags :> AF.t list) in
    let name_idx = CP.add_utf8 pool (Name.utf8_for_method name) in
    let desc_utf8 = Descriptor.utf8_of_method desc in
    let desc_idx = CP.add_utf8 pool desc_utf8 in
    if log_se_full then printf "@\n@[encoding method %s@\n@." (HM.to_string m);
    if log_se then printf "@\n@[<2>digraph %s {" (HM.to_string m);
    let r =
      { M.access_flags = acc_flags;
        name_index = name_idx;
        descriptor_index = desc_idx;
        attributes_count = U.u2 (List.length attrs);
        attributes_array = U.map_list_to_array (HAO.encode_method c_name m pool)
          (attrs :> T.method_attribute list); }
    in
    if log_se then printf "@]@\n}";
    r
end (* }}} *)
module HMO = HighMethodOps
module F = Field
module HighField = struct (* {{{ *)
  let decode is_interface pool i =
    let flags = AF.from_u2 false i.F.access_flags in
    let f_flags = AF.check_field_flags is_interface flags in
    let name = CP.get_utf8_entry pool i.F.name_index in
    if not (Name.is_valid_unqualified name) then fail T.Invalid_name;
    let f_name = Name.make_for_field name in
    let descriptor = CP.get_utf8_entry pool i.F.descriptor_index in
    let f_descriptor = Descriptor.field_of_utf8 descriptor in
    let f_attributes =
      U.map_array_to_list (HAO.decode_field pool) i.F.attributes_array in
    { T.f_flags; f_name; f_descriptor; f_attributes }

  let encode pool f =
    let access_flags = AF.list_to_u2 (f.T.f_flags :> AccessFlag.t list) in
    let name_index = CP.add_utf8 pool (Name.utf8_for_field f.T.f_name) in
    let desc_utf8 = Descriptor.utf8_of_field f.T.f_descriptor in
    let descriptor_index = CP.add_utf8 pool desc_utf8 in
    let attributes_count = U.u2 (List.length f.T.f_attributes) in
    let attributes_array =
      U.map_list_to_array (HAO.encode None pool) (f.T.f_attributes :> T.attribute list) in
    { F.access_flags; name_index; descriptor_index; attributes_count
    ; attributes_array }

end (* }}} *)
module HF = HighField
(* }}} *)
(* rest/most of Coder *) (* {{{ *)
let check_version_high version c =
  let check_flag x = Version.check (AF.version_bounds x) version in
  let check_attribute x = Version.check (HAO.version_bounds x) version in
  let check_field x =
    List.iter check_flag (x.T.f_flags :> AF.t list);
    List.iter check_attribute (x.T.f_attributes :> T.attribute list) in
  let check_method =
    let cfa f a =
      List.iter check_flag (f :> AF.t list);
      List.iter check_attribute (a :> T.attribute list) in
    function
      | T.RegularMethod m -> cfa m.T.rm_flags m.T.rm_attributes
      | T.InitMethod m -> cfa m.T.im_flags m.T.im_attributes
      | T.ClinitMethod m -> cfa m.T.cm_flags m.T.cm_attributes
  in
  List.iter check_field c.T.c_fields;
  List.iter check_method c.T.c_methods;
  List.iter check_attribute (c.T.c_attributes :> T.attribute list);
  c

let decode cf =
  let pool = cf.CF.constant_pool in
  let version = cf.CF.major_version, cf.CF.minor_version in
  let version = Version.version_of_major_minor version in
  CP.check_version version pool;
  let flags = AF.check_class_flags (AF.from_u2 false cf.CF.access_flags) in
  let class_name = CP.get_class_name pool in
  let this_class_name = class_name cf.CF.this_class in
  let extends =
    if cf.CF.super_class = U.u2 0
    then None
    else Some (class_name cf.ClassFile.super_class) in
  let is_interface = List.mem `Interface flags in
  let field_decode = HF.decode is_interface pool in
  let method_decode = HMO.decode this_class_name is_interface pool in
  let attribute_decode = HAO.decode_class pool in
  let hc = check_version_high version
    { T.c_flags = flags
    ; c_name = this_class_name
    ; c_extends = extends
    ; c_implements = List.map class_name (Array.to_list cf.CF.interfaces)
    ; c_fields = List.map field_decode (Array.to_list cf.CF.fields)
    ; c_methods = List.map method_decode (Array.to_list cf.CF.methods)
    ; c_attributes =
      List.map attribute_decode (Array.to_list cf.CF.attributes) } in
  (hc, version)

(* TODO should this be in consts? *)
let no_super_class = U.u2 0

let encode (cd, version) =
  ignore (check_version_high version cd);
  let major, minor = Version.major_minor_of_version version in
  let pool = CP.make_extendable () in
  let this_index = CP.add_class pool cd.T.c_name in
  let super_index = match cd.T.c_extends with
    | Some n -> CP.add_class pool n
    | None -> no_super_class in
  let itfs = U.map_list_to_array (fun s -> CP.add_class pool s) cd.T.c_implements in
  let flds = U.map_list_to_array (HF.encode pool) cd.T.c_fields in
  let mths = U.map_list_to_array (HMO.encode cd.T.c_name pool) cd.T.c_methods in
  let atts = U.map_list_to_array (HAO.encode_class pool) cd.T.c_attributes in
  let cpool = CP.to_pool pool in
  let checked_number s sz =
    if sz <= U.max_u2 then
      U.u2 sz
    else
      fail (T.Too_many s) in
  let checked_length_array s arr =
    let res = Array.length arr in
    checked_number s res in
  CP.check_version version cpool;
  { CF.magic = U.u4 magic_number;
    CF.minor_version = minor;
    CF.major_version = major;
    CF.constant_pool_count = CP.size cpool;
    CF.constant_pool = cpool;
    CF.access_flags = AF.list_to_u2 (cd.T.c_flags :> AF.t list);
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
