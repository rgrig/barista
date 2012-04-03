(** Types used for the high-level representation of the bytecode.

  - agnostic of any byte counts (such as offsets or instruction sizes)
  - no instruction variants whose goal is to save speed/memory
    (such as ICONST/ICONST_0, LDC/LDC_W)
  - no constant pool
  - agnostic of value sizes (so a long takes one register, not two)

There is no mli file on purpose: It would be a verbatim copy of this file. *)

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

exception Exception of error

type constant_primitive =
  [ `Double of float
  | `Float of float
  | `Int of int32
  | `Long of int64
  | `String of Utils.UTF8.t ]

type constant_arrayref =
  [ `Array_type of Descriptor.array_type ]

type constant_classref =
  [ `Class_or_interface of Name.for_class ]

type constant_typeref =
  [ constant_arrayref | constant_classref ]

type constant_fieldref =
  [ `Fieldref of Name.for_class * Name.for_field * Descriptor.for_field ]

type constant_methodref =
  [ `Methodref of constant_typeref * Name.for_method * Descriptor.for_method ]

type constant_stack =
  [ constant_primitive | constant_typeref ]

type constant_field =
  constant_primitive

type constant =
  [ constant_arrayref
  | constant_classref
  | constant_field
  | constant_fieldref
  | constant_methodref
  | constant_primitive
  | constant_stack ]

type label = int64

type local = int

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
  | ANEWARRAY of constant_typeref
  | ARETURN
  | ARRAYLENGTH
  | ASTORE of int
  | ATHROW
  | BALOAD
  | BASTORE
  | BIPUSH of int
  | CALOAD
  | CASTORE
  | CHECKCAST of constant_typeref
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
  | GETFIELD of constant_fieldref
  | GETSTATIC of constant_fieldref
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
  | INSTANCEOF of constant_typeref
  | INVOKEINTERFACE of constant_methodref
  | INVOKESPECIAL of constant_methodref
  | INVOKESTATIC of constant_methodref
  | INVOKEVIRTUAL of constant_methodref
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
  | LDC of constant_stack
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
  | MULTIANEWARRAY of constant_typeref * int
  | NEW of Name.for_class
  | NEWARRAY of Descriptor.java_type
  | NOP
  | POP
  | POP2
  | PUTFIELD of constant_fieldref
  | PUTSTATIC of constant_fieldref
  | RET of int
  | RETURN
  | SALOAD
  | SASTORE
  | SIPUSH of int
  | SWAP
  | TABLESWITCH of tableswitch

type labeled_instruction = label * instruction

module LabelHash = Hashtbl.Make (struct
  type t = label
  let equal = (=)
  let hash = Hashtbl.hash
end)

module LabelSet = Set.Make(struct
  type t = label
  let compare = compare
end)

(* TODO(rgrig): This should be more similar to the types listed in 4.10.1.1. *)
type bytecode_type =
  | Top
  | Integer
  | Float
  | Long
  | Double
  | Null
  | Uninitialized_this
  | Object of constant_typeref
  | Uninitialized of label
  | Return_address of LabelSet.t

type inner_class_element =
  { inner_class : Name.for_class option
  ; outer_class : Name.for_class option
  ; inner_name : Utils.UTF8.t option
  ; inner_flags : AccessFlag.for_inner_class list }

type enclosing_method_value =
  { innermost_class : Name.for_class
  ; enclosing_method : (Name.for_method * Descriptor.for_method) option }

type code_attribute =
  [ `IgnoredAttribute
  | `LineNumberTable of int LabelHash.t
  | `Unknown of Utils.UTF8.t * string ]

type exception_table_element =
  { try_start : label  (* inclusive *)
  ; try_end : label  (* inclusive *)
  ; catch : label
  ; caught : Name.for_class option }

type code_value =
  { cv_code : labeled_instruction list
  ; cv_exception_table : exception_table_element list
  ; cv_attributes : code_attribute list
  ; cv_type_of_local : bytecode_type Utils.IntMap.t }

type class_attribute =
  [ `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
  | `ClassSignature of Signature.class_signature
  | `Deprecated
  | `EnclosingMethod of enclosing_method_value
  | `InnerClasses of inner_class_element list
  | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
  | `SourceFile of Utils.UTF8.t
  | `Synthetic (** auto-generated element *)
  | `Unknown of Utils.UTF8.t * string ]

type method_attribute =
  [ `AnnotationDefault of Annotation.element_value
  | `Code of code_value
  | `Deprecated
  | `Exceptions of Name.for_class list
  | `MethodSignature of Signature.method_signature
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleParameterAnnotations of Annotation.t list list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeVisibleParameterAnnotations of Annotation.t list list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `Synthetic (** auto-generated element *)
  | `Unknown of Utils.UTF8.t * string ]

type field_attribute =
  [ `ConstantValue of constant_field
  | `Deprecated
  | `FieldSignature of Signature.field_type_signature
  | `RuntimeInvisibleAnnotations of Annotation.t list
  | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
  | `RuntimeVisibleAnnotations of Annotation.t list
  | `RuntimeVisibleTypeAnnotations of Annotation.extended list
  | `Synthetic
  | `Unknown of Utils.UTF8.t * string ]

type attribute =
  [ class_attribute
  | code_attribute
  | field_attribute
  | method_attribute ]

type regular_method =
  { rm_flags : AccessFlag.for_method list
  ; rm_name : Name.for_method
  ; rm_descriptor : Descriptor.for_method
  ; rm_attributes : method_attribute list }

type init_method =
  { im_flags : AccessFlag.for_constructor list
  ; im_descriptor : Descriptor.for_parameter list
  ; im_attributes : method_attribute list }

type clinit_method =
  { cm_flags : AccessFlag.for_initializer list
  ; cm_attributes : method_attribute list }

(* XXX Flatten these three? *)
type method_ =
  | RegularMethod of regular_method
  | InitMethod of init_method
  | ClinitMethod of clinit_method

type field =
  { f_flags : AccessFlag.for_field list
  ; f_name : Name.for_field
  ; f_descriptor : Descriptor.for_field
  ; f_attributes : field_attribute list }

type class_ =
  { c_flags : AccessFlag.for_class list
  ; c_name : Name.for_class
  ; c_extends : Name.for_class option
  ; c_implements : Name.for_class list
  ; c_fields : field list
  ; c_methods : method_ list
  ; c_attributes : class_attribute list }
