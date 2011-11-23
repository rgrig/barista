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

(** Definition of bytecode elements as well as i/o functions.

    Bytecode elements are low-level code elements,
    high-level elements being provided by [Instruction] module. *)

open Utils


(** {6 Types} *)

type short_offset = s2
(** The type of short offsets for jump instructions. *)

type long_offset = s4
(** The type of long offsets for jump instructions. *)

type instruction =
  | AALOAD (** load reference from array. *)
  | AASTORE (** store into reference array. *)
  | ACONST_NULL (** push null. *)
  | ALOAD of u1 (** load reference from local variable. *)
  | ALOAD_0 (** load reference from local variable. *)
  | ALOAD_1 (** load reference from local variable. *)
  | ALOAD_2 (** load reference from local variable. *)
  | ALOAD_3 (** load reference from local variable. *)
  | ANEWARRAY of u2 (** create new array of references. *)
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
  | CHECKCAST of u2 (** check whether object is of given type. *)
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
  | GETFIELD of u2 (** fetch field from object. *)
  | GETSTATIC of u2 (** get static field from class. *)
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
  | INSTANCEOF of u2 (** determine if object is of given type. *)
  | INVOKEDYNAMIC of u2 (** invoke instance method; resolve and dispatch based on class. *)
  | INVOKEINTERFACE of u2 * u1 (** invoke interface method. *)
  | INVOKESPECIAL of u2 (** invoke instance method; special handling for superclass, private, and instance initialization method invocations. *)
  | INVOKESTATIC of u2 (** invoke a class (static) method. *)
  | INVOKEVIRTUAL of u2 (** invoke instance method; dispatch based on class. *)
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
  | LDC of u1 (** push item from runtime constant pool. *)
  | LDC2_W of u2 (** push long or double from runtime constant pool (wide index). *)
  | LDC_W of u2 (** push item from runtime constant pool (wide index). *)
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
  | MULTIANEWARRAY of u2 * u1 (** create new multidimensional array. *)
  | NEW of u2 (** create new object. *)
  | NEWARRAY of u1 (** create new arrayhandler. *)
  | NOP (** do nothing. *)
  | POP (** pop the top operand stack value. *)
  | POP2 (** pop the top one or two operand stack values. *)
  | PUTFIELD of u2 (** set field in object. *)
  | PUTSTATIC of u2 (** set static field in class. *)
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

type error =
  | Invalid_padding_byte
  | Unknown_opcode
  | Invalid_trailing_byte
  | Invalid_trailing_short
  | Internal
  | Invalid_switch_cases

exception Exception of error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)


(** {6 I/O functions} *)

val read : InputStream.t -> int -> instruction list
(** [read st ofs] reads an instruction list from [st] using offset [ofs].
    The offset is needed because some instructions include padding in
    order to align data on given offsets.
    Raises [Exception] if an unknown opcode is encountered.
    Raises [Exception] if an invalid instruction is encountered.
    Raises [InputStream.Exception] if an i/o error occurs. *)

val write : OutputStream.t -> int -> instruction list -> unit
(** [write st ofs l] writes instruction list [l] onto [st] using offset
    [ofs]. The offset is needed because some instructions include padding
    in order to align data on given offsets.
    Raises [OutputStream.Exception] if an i/o error occurs. *)
