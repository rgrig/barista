(**
  A truly high-level representation of Java bytecode.

    - agnostic of any byte counts (such as offsets or instruction sizes)
    - no instruction variants whose goal is to save speed/memory
      (such as ICONST/ICONST_0, LDC/LDC_W)
    - no constant pool
 *)

(* NOTE: This is supposed to be what [ClassDefinition] should have been in
the first place. *)

module HighConstant : sig (* {{{ *)
  type primitive =
    [ `Double of float
    | `Float of float
    | `Int of int32
    | `Long of int64
    | `String of Utils.UTF8.t ]
  type arrayref =
    [ `Array_type of Descriptor.array_type ]
  type classref =
    [ `Class_or_interface of Name.for_class ]
  type typeref =
    [ arrayref | classref ]
  type fieldref = (* used by instructions like GETFIELD *)
    [ `Fieldref of Name.for_class * Name.for_field * Descriptor.for_field ]
  type methodref =
    [ `Methodref of typeref * Name.for_method * Descriptor.for_method ]
  type stack = (* what LDC may put on the stack *)
    [ primitive | typeref ]
  type field = (* what may initialize a field *)
    primitive

  (* Anything that may come from (the low-level) [ConstantPool.element]. *)
  type t =
    [ arrayref
    | classref
    | field
    | fieldref
    | methodref
    | primitive
    | stack ]

  val decode : ConstantPool.t -> Utils.u2 -> t
  val encode : ConstantPool.extendable -> t -> Utils.u2

  (* TODO(rgrig): Add here the [decode_*] functions. *)
end (* }}} *)
module HighInstruction : sig (* {{{ *)
  type label
  val fresh_label : unit -> label
  val invalid_label : label
  type iinc = { ii_var: int; ii_inc: int }
  type lookupswitch =
    { ls_def: label
    ; ls_branches: (int32 * label) list }
  type tableswitch =
    { ts_def: label
    ; ts_low: int32
    ; ts_high: int32
    ; ts_ofss: label list }
  type instruction =
    | AALOAD
    | AASTORE
    | ACONST_NULL
    | ALOAD of int
    | ANEWARRAY of HighConstant.typeref
    | ARETURN
    | ARRAYLENGTH
    | ASTORE of int
    | ATHROW
    | BALOAD
    | BASTORE
    | BIPUSH of int
    | CALOAD
    | CASTORE
    | CHECKCAST of HighConstant.typeref
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
    | GETFIELD of HighConstant.fieldref
    | GETSTATIC of HighConstant.fieldref
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
    | INSTANCEOF of HighConstant.typeref
    | INVOKEINTERFACE of HighConstant.methodref
    | INVOKESPECIAL of HighConstant.methodref
    | INVOKESTATIC of HighConstant.methodref
    | INVOKEVIRTUAL of HighConstant.methodref
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
    | LDC of HighConstant.stack
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
    | MULTIANEWARRAY of HighConstant.typeref * int
    | NEW of Name.for_class
    | NEWARRAY of Descriptor.java_type
    | NOP
    | POP
    | POP2
    | PUTFIELD of HighConstant.fieldref
    | PUTSTATIC of HighConstant.fieldref
    | RET of int
    | RETURN
    | SALOAD
    | SASTORE
    | SIPUSH of int
    | SWAP
    | TABLESWITCH of tableswitch
  val instruction_to_string : instruction -> string
  type t = label * instruction

  module LabelHash : Hashtbl.S with type key = label

  val version_bounds : t -> Version.bounds
end (* }}} *)
module HighAttribute : sig (* {{{ *)
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
    | `IgnoredAttribute
    | `LineNumberTable of int HighInstruction.LabelHash.t
    | `Unknown of Utils.UTF8.t * string

    (* TODO: Treating these properly requires some symbolic execution. *)
    | `LocalVariableTable of unit (** types for local variables *)
    | `LocalVariableTypeTable of unit (** signatures for local variables *)
  ]

  type exception_table_element = {
      try_start : HighInstruction.label;
      try_end : HighInstruction.label;
      catch : HighInstruction.label;
      caught : Name.for_class option;
    }

  type code_value = {
      code : HighInstruction.t list;
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
    [ `ConstantValue of HighConstant.field
    | `Synthetic
    | `FieldSignature of Signature.field_type_signature
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `Unknown of Utils.UTF8.t * string ]

  type t = [ for_class | for_method | for_field | code_attribute ]
end (* }}} *)

module HighMethod : sig (* {{{ *)
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

end (* }}} *)

module SymbExe : sig (* {{{ *)
  type t
  val make_empty : unit -> t

  type 'value stepper =
    'value    (* abstract value *)
    -> HighInstruction.t    (* instruction to execute *)
    -> HighInstruction.label    (* next instruction in program text *)
    -> 'value * HighInstruction.label list    (* successors *)

  type 'value executor =
    'value stepper
    -> ('value -> 'value) (* simulates a (possibly JVM produced) exception *)
    -> ('value -> 'value -> 'value)   (* unifier *)
    -> ('value -> 'value -> bool) (* equality test *)
    -> 'value   (* initial value *)
    -> HighAttribute.code_value   (* the code of the method *)
    -> 'value HighInstruction.LabelHash.t

  val step : t stepper
end (* }}} *)

module HighAttributeOps : sig (* {{{ *)
  val version_bounds : HighAttribute.t -> Version.bounds

  val decode_class :  ConstantPool.t -> Attribute.info -> HighAttribute.for_class
  val decode_field : ConstantPool.t -> Attribute.info -> HighAttribute.for_field
  val decode_method : ConstantPool.t -> Attribute.info -> HighAttribute.for_method

  val encode_class : ConstantPool.extendable -> HighAttribute.for_class -> Attribute.info
  val encode_field : ConstantPool.extendable -> HighAttribute.for_field -> Attribute.info
  val encode_method : HighMethod.t -> ConstantPool.extendable -> HighAttribute.for_method -> Attribute.info

end (* }}} *)

module HighMethodOps : sig (* {{{ *)
  val decode : bool -> ConstantPool.t -> Method.info -> HighMethod.t

  val encode : ConstantPool.extendable -> HighMethod.t -> Method.info
end (* }}} *)

module HighField : sig (* {{{ *)
  type t = {
      flags : AccessFlag.for_field list;
      name : Name.for_field;
      descriptor : Descriptor.for_field;
      attributes : HighAttribute.for_field list;
    }
  val decode : bool -> ConstantPool.t -> Field.info -> t
  val encode : ConstantPool.extendable -> t -> Field.info
end (* }}} *)

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : HighField.t list;
    methods : HighMethod.t list;
    attributes : HighAttribute.for_class list;
  }

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
  | Invalid_stack_map_table
  | Misplaced_attribute of (string * string)
  | SE_array_expected of string
  | SE_different_stack_sizes of (int * int)
  | SE_double_new
  | SE_empty_stack
  | SE_invalid_label
  | SE_invalid_local_contents of (int * string * string)
  | SE_invalid_stack_top of (string * string)
  | SE_missing_return
  | SE_reference_expected of string
  | SE_unexpected_size of (int * string)
  | SE_uninitialized_register of (int * int)
  | Too_many of string
  | Unsupported of string

val string_of_error : error -> string

exception Exception of error

val check_version_high : Version.t -> t -> t
val decode : ClassFile.t -> (t * Version.t)
val encode : (t * Version.t) -> ClassFile.t

(*
vim:tw=0:
*)
