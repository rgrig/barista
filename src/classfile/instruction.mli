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

(** Definition of instructions as well as conversion from/to bytecode and
    compilation/decompilation functions.

    Instructions are high-level code elements,
    low-level elements being provided by [ByteCode] module. *)

open Utils
open Consts


(** {6 Types} *)

type short_offset = s2
(** The type of short offsets for jump instructions. *)

type long_offset = s4
(** The type of long offsets for jump instructions. *)

type t =
  | AALOAD (** load reference from array. *)
  | AASTORE (** store into reference array. *)
  | ACONST_NULL (** push null. *)
  | ALOAD of u1 (** load reference from local variable. *)
  | ALOAD_0 (** load reference from local variable. *)
  | ALOAD_1 (** load reference from local variable. *)
  | ALOAD_2 (** load reference from local variable. *)
  | ALOAD_3 (** load reference from local variable. *)
  | ANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] (** create new array of references. *)
  | ARETURN (** return reference from method. *)
  | ARRAYLENGTH (** get length of array. *)
  | ASTORE of u1 (** store reference into local variable. *)
  | ASTORE_0 (** store reference into local variable. *)
  | ASTORE_1 (** store reference into local variable. *)
  | ASTORE_2 (** store reference into local variable. *)
  | ASTORE_3 (** store reference into local variable. *)
  | ATHROW (** throw exception or error. *)
  | BALOAD (** load byte or boolean from array. *)
  | BASTORE (** store into byte or boolean array. *)
  | BIPUSH of s1 (** push byte. *)
  | CALOAD (** load char from array. *)
  | CASTORE (** store into char array. *)
  | CHECKCAST of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] (** check whether object is of given type. *)
  | D2F (** convert double to float. *)
  | D2I (** convert double to int. *)
  | D2L (** convert double to long. *)
  | DADD (** add double. *)
  | DALOAD (** load double from array. *)
  | DASTORE (** store into double array. *)
  | DCMPG (** compare double. *)
  | DCMPL (** compare double. *)
  | DCONST_0 (** push double. *)
  | DCONST_1 (** push double. *)
  | DDIV (** divide double. *)
  | DLOAD of u1 (** load double from local variable. *)
  | DLOAD_0 (** load double from local variable. *)
  | DLOAD_1 (** load double from local variable. *)
  | DLOAD_2 (** load double from local variable. *)
  | DLOAD_3 (** load double from local variable. *)
  | DMUL (** multiply double. *)
  | DNEG (** negate double. *)
  | DREM (** remainder double. *)
  | DRETURN (** return double from method. *)
  | DSTORE of u1 (** store double into local variable. *)
  | DSTORE_0 (** store double into local variable. *)
  | DSTORE_1 (** store double into local variable. *)
  | DSTORE_2 (** store double into local variable. *)
  | DSTORE_3 (** store double into local variable. *)
  | DSUB (** subtract double. *)
  | DUP (** duplicate the top operand stack value. *)
  | DUP2 (** duplicate the top one or two operand stack values. *)
  | DUP2_X1 (** duplicate the top one or two operand stack values and insert two or three values down. *)
  | DUP2_X2 (** duplicate the top one or two operand stack values and insert two, three, or four values down. *)
  | DUP_X1 (** duplicate the top operand stack value and insert two values down. *)
  | DUP_X2 (** duplicate the top operand stack value and insert two or threee values down. *)
  | F2D (** convert float to double. *)
  | F2I (** convert float to int. *)
  | F2L (** convert float to long. *)
  | FADD (** add float. *)
  | FALOAD (** load float from array. *)
  | FASTORE (** store into float array. *)
  | FCMPG (** compare float. *)
  | FCMPL (** compare float. *)
  | FCONST_0 (** push float. *)
  | FCONST_1 (** push float. *)
  | FCONST_2 (** push float. *)
  | FDIV (** divide float. *)
  | FLOAD of u1 (** load float from local variable. *)
  | FLOAD_0 (** load float from local variable. *)
  | FLOAD_1 (** load float from local variable. *)
  | FLOAD_2 (** load float from local variable. *)
  | FLOAD_3 (** load float from local variable. *)
  | FMUL (** multiply float. *)
  | FNEG (** negate float. *)
  | FREM (** remainder float. *)
  | FRETURN (** return float from method. *)
  | FSTORE of u1 (** store float into local variable. *)
  | FSTORE_0 (** store float into local variable. *)
  | FSTORE_1 (** store float into local variable. *)
  | FSTORE_2 (** store float into local variable. *)
  | FSTORE_3 (** store float into local variable. *)
  | FSUB (** subtract float. *)
  | GETFIELD of (Name.for_class * Name.for_field * Descriptor.for_field) (** fetch field from object. *)
  | GETSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field) (** get static field from class. *)
  | GOTO of short_offset (** branch always. *)
  | GOTO_W of long_offset (** branch always. *)
  | I2B (** convert int to byte. *)
  | I2C (** convert int to char. *)
  | I2D (** convert int to double. *)
  | I2F (** convert int to float. *)
  | I2L (** convert int to long. *)
  | I2S (** convert int to short. *)
  | IADD (** add int. *)
  | IALOAD (** load int from array. *)
  | IAND (** boolean AND int. *)
  | IASTORE (** store into int array. *)
  | ICONST_0 (** push int constant. *)
  | ICONST_1 (** push int constant. *)
  | ICONST_2 (** push int constant. *)
  | ICONST_3 (** push int constant. *)
  | ICONST_4 (** push int constant. *)
  | ICONST_5 (** push int constant. *)
  | ICONST_M1 (** push int constant. *)
  | IDIV (** divide int. *)
  | IF_ACMPEQ of short_offset (** branch if reference comparison succeeds. *)
  | IF_ACMPNE of short_offset (** branch if reference comparison succeeds. *)
  | IF_ICMPEQ of short_offset (** branch if int comparison succeeds. *)
  | IF_ICMPGE of short_offset (** branch if int comparison succeeds. *)
  | IF_ICMPGT of short_offset (** branch if int comparison succeeds. *)
  | IF_ICMPLE of short_offset (** branch if int comparison succeeds. *)
  | IF_ICMPLT of short_offset (** branch if int comparison succeeds. *)
  | IF_ICMPNE of short_offset (** branch if int comparison succeeds. *)
  | IFEQ of short_offset (** branch if int comparison with zero succeeds. *)
  | IFGE of short_offset (** branch if int comparison with zero succeeds. *)
  | IFGT of short_offset (** branch if int comparison with zero succeeds. *)
  | IFLE of short_offset (** branch if int comparison with zero succeeds. *)
  | IFLT of short_offset (** branch if int comparison with zero succeeds. *)
  | IFNE of short_offset (** branch if int comparison with zero succeeds. *)
  | IFNONNULL of short_offset (** branch if reference not null. *)
  | IFNULL of short_offset (** branch if reference not null. *)
  | IINC of u1 * s1 (** increment local variable by constant. *)
  | ILOAD of u1 (** load int from local variable. *)
  | ILOAD_0 (** load int from local variable. *)
  | ILOAD_1 (** load int from local variable. *)
  | ILOAD_2 (** load int from local variable. *)
  | ILOAD_3 (** load int from local variable. *)
  | IMUL (** multiply int. *)
  | INEG (** negate int. *)
  | INSTANCEOF of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] (** determine if object is of given type. *)
  | INVOKEDYNAMIC of (Bootstrap.method_specifier * Name.for_method * Descriptor.for_method) (** invoke instance method; resolve and dispatch based on class. *)
  | INVOKEINTERFACE of (Name.for_class * Name.for_method * Descriptor.for_method) * u1 (** invoke interface method. *)
  | INVOKESPECIAL of (Name.for_class * Name.for_method * Descriptor.for_method) (** invoke instance method; special handling for superclass, private, and instance initialization method invocations. *)
  | INVOKESTATIC of (Name.for_class * Name.for_method * Descriptor.for_method) (** invoke a class (static) method. *)
  | INVOKEVIRTUAL of ([`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * Name.for_method * Descriptor.for_method) (** invoke instance method; dispatch based on class. *)
  | IOR (** boolean OR int. *)
  | IREM (** remainder int. *)
  | IRETURN (** return int from method. *)
  | ISHL (** shift left int. *)
  | ISHR (** arithmetic shift right int. *)
  | ISTORE of u1 (** store int into local variable. *)
  | ISTORE_0 (** store int into local variable. *)
  | ISTORE_1 (** store int into local variable. *)
  | ISTORE_2 (** store int into local variable. *)
  | ISTORE_3 (** store int into local variable. *)
  | ISUB (** subtract int. *)
  | IUSHR (** logical shift right int. *)
  | IXOR (** boolean XOR int. *)
  | JSR of short_offset (** jump subroutine. *)
  | JSR_W of long_offset (** jump subroutine (wide index). *)
  | L2D (** convert long to double. *)
  | L2F (** convert long to float. *)
  | L2I (** convert long to int. *)
  | LADD (** add long. *)
  | LALOAD (** load long from array. *)
  | LAND (** boolean AND long. *)
  | LASTORE (** store into long array. *)
  | LCMP (** compare long. *)
  | LCONST_0 (** push long constant. *)
  | LCONST_1 (** push long constant. *)
  | LDC of [`Int of int32 | `Float of float | `String of UTF8.t | `Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type | `Method_type of Descriptor.for_method | `Method_handle of Bootstrap.method_handle] (** push item from runtime constant pool. *)
  | LDC2_W of [`Long of int64 | `Double of float] (** push long or double from runtime constant pool (wide index). *)
  | LDC_W of [`Int of int32 | `Float of float | `String of UTF8.t | `Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type | `Method_type of Descriptor.for_method | `Method_handle of Bootstrap.method_handle] (** push item from runtime constant pool (wide index). *)
  | LDIV (** divide long. *)
  | LLOAD of u1 (** load long from local variable. *)
  | LLOAD_0 (** load long from local variable. *)
  | LLOAD_1 (** load long from local variable. *)
  | LLOAD_2 (** load long from local variable. *)
  | LLOAD_3 (** load long from local variable. *)
  | LMUL (** multiply long. *)
  | LNEG (** negate long. *)
  | LOOKUPSWITCH of long_offset * s4 * ((s4 * long_offset) list) (** access jump table by key match and jump. *)
  | LOR (** boolean OR long. *)
  | LREM (** remainder long. *)
  | LRETURN (** return long from method. *)
  | LSHL (** shift left long. *)
  | LSHR (** arithmetic shift right long. *)
  | LSTORE of u1 (** store long into local variable. *)
  | LSTORE_0 (** store long into local variable. *)
  | LSTORE_1 (** store long into local variable. *)
  | LSTORE_2 (** store long into local variable. *)
  | LSTORE_3 (** store long into local variable. *)
  | LSUB (** subtract long. *)
  | LUSHR (** logical shift right long. *)
  | LXOR (** boolean XOR long. *)
  | MONITORENTER (** enter monitor for object. *)
  | MONITOREXIT (** exit monitor for object. *)
  | MULTIANEWARRAY of [`Class_or_interface of Name.for_class | `Array_type of Descriptor.array_type] * u1 (** create new multidimensional array. *)
  | NEW of Name.for_class (** create new object. *)
  | NEWARRAY of Descriptor.java_type (** create new arrayhandler. *)
  | NOP (** do nothing. *)
  | POP (** pop the top operand stack value. *)
  | POP2 (** pop the top one or two operand stack values. *)
  | PUTFIELD of (Name.for_class * Name.for_field * Descriptor.for_field) (** set field in object. *)
  | PUTSTATIC of (Name.for_class * Name.for_field * Descriptor.for_field) (** set static field in class. *)
  | RET of u1 (** return from subroutine. *)
  | RETURN (** return void from method. *)
  | SALOAD (** load short from array. *)
  | SASTORE (** store into short array. *)
  | SIPUSH of s2 (** push short. *)
  | SWAP (** swap the top two operand stack values. *)
  | TABLESWITCH of long_offset * s4 * s4 * (long_offset list) (** access jump table by index and jump. *)
  | WIDE_ALOAD of u2 (** load reference from local variable. *)
  | WIDE_ASTORE of u2 (** store reference into local variable. *)
  | WIDE_DLOAD of u2 (** load double from local variable. *)
  | WIDE_DSTORE of u2 (** store double into local variable. *)
  | WIDE_FLOAD of u2 (** load float from local variable. *)
  | WIDE_FSTORE of u2 (** store float into local variable. *)
  | WIDE_IINC of u2 * s2 (** increment local variable by constant. *)
  | WIDE_ILOAD of u2 (** load int from local variable. *)
  | WIDE_ISTORE of u2 (** store int into local variable. *)
  | WIDE_LLOAD of u2 (** load long from local variable. *)
  | WIDE_LSTORE of u2 (** store long into local variable. *)
  | WIDE_RET of u2 (** return from subroutine. *)
(** The type of instructions. *)


(** {6 Exception} *)

BARISTA_ERROR =
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


(** {6 Conversion functions} *)

val decode : Bootstrap.methods -> ConstantPool.t -> ByteCode.instruction -> t
(** [decode bsm cpool i] decodes instruction [i] using constant pool [cpool],
    and bootstrap information [bsm].
    Raise [Exception] if an instruction parameter is invalid.
    Raise [ConstantPool.Exception] if a constant pool index is invalid.
    Raise [Exception] if constant pool is invalid. *)

val encode : Bootstrap.methods -> ConstantPool.extendable -> t -> ByteCode.instruction
(** [encode bsm cpool i] encodes instruction [i] using constant pool [cpool],
    and bootstrap information [bsm].
    Raise [ConstantPool.Exception] if constant pool is too large.
    Raise [Exception] if an instruction parameter is invalid. *)


(** {6 Compilation functions} *)

type parameter =
  | Int_constant of int64 (** Constant parameter: integer value. *)
  | Offset of int32 (** Constant parameter: offset value. *)
  | Float_constant of float (** Constant parameter: float value. *)
  | String_constant of UTF8.t (** Constant parameter: string value. *)
  | Class_name of Name.for_class (** Reference to a class. *)
  | Array_type of UTF8.t (** Reference to an array type. *)
  | Primitive_type of Descriptor.java_type (** Reference to a primitive type. *)
  | Field of Name.for_class * Name.for_field * Descriptor.for_field (** Reference to a field (either static or instance one). *)
  | Dynamic_method of Bootstrap.method_specifier * Name.for_method * Descriptor.for_method (** Reference to a dynamic method: method name and descriptor but no receiver. *)
  | Method of Name.for_class * Name.for_method * Descriptor.for_method (** Reference to a method (either static or instance one). *)
  | Array_method of Descriptor.array_type * Name.for_method * Descriptor.for_method (** Reference to an array method: name and descriptor with an array type as receiver. *)
  | Method_type_constant of Descriptor.for_method (** Constant method type (parameters and return type). *)
  | Method_handle_constant of Bootstrap.method_handle (** Constant method handle (reference to field of method). *)
(** Type of instruction parameters. *)

type parameters_tail =
  | No_tail (** No additional parameter. *)
  | Match_offset_pairs of (s4 * long_offset) list (** List of match/offset pairs (used for lookup switches). *)
  | Long_offsets of long_offset list (** List of offsets (used for table switches). *)
(** Type of instruction parameter tail (used to encode a variable number of instruction parameters). *)

val compile : int -> bool -> string -> parameter list -> parameters_tail -> (int * t)
(** [compile ofs wide mnemo params param_tail] compiles instruction whose
    mnemonic is [mnemo], [params] and [param_tail] being parameters while
    [wide] indicates whether the wide version of the instruction should
    be compiled. [ofs] is the compilation offset; the returned couple
    consists of instruction size and compiled instruction.
    Raises [Exception] if instruction does not exist or is passed invalid
    parameters. *)

val decompile : int -> t -> (int * bool * string * (parameter list) * parameters_tail)
(** [decompile ofs i] decompiles instruction [i] at offset [ofs].
    Returns [(sz, wide, mnemo, p, t)] where: [sz] is instruction size,
    [wide] indicates whether the instruction is a wide one,
    [mnemo] is instruction mnemonic, and [p] and [t] are parameters. *)


(** {6 Sizes} *)

val size_of : int -> t -> int
(** [size_of ofs i] returns the size of instruction [i] at offset [ofs]. *)

val size_of_list : int -> t list -> int
(** [size_of_list ofs l] returns the size of instruction list [l] at
    offset [ofs]. *)


(** {6 Versions} *)

val version_bounds : t -> Version.bounds
(** Returns the version bounds for the passed instruction. *)
