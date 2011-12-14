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


(* Types *)

type short_offset = s2

type long_offset = s4

type instruction =
  | AALOAD
  | AASTORE
  | ACONST_NULL
  | ALOAD of u1
  | ALOAD_0
  | ALOAD_1
  | ALOAD_2
  | ALOAD_3
  | ANEWARRAY of u2
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
  | CHECKCAST of u2
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
  | GETFIELD of u2
  | GETSTATIC of u2
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
  | INSTANCEOF of u2
  | INVOKEDYNAMIC of u2
  | INVOKEINTERFACE of u2 * u1
  | INVOKESPECIAL of u2
  | INVOKESTATIC of u2
  | INVOKEVIRTUAL of u2
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
  | LDC of u1
  | LDC2_W of u2
  | LDC_W of u2
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
  | MULTIANEWARRAY of u2 * u1
  | NEW of u2
  | NEWARRAY of u1
  | NOP
  | POP
  | POP2
  | PUTFIELD of u2
  | PUTSTATIC of u2
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
  | Invalid_padding_byte
  | Unknown_opcode
  | Invalid_trailing_byte
  | Invalid_trailing_short
  | Internal
  | Invalid_switch_cases

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Invalid_padding_byte -> "invalid padding byte"
  | Unknown_opcode -> "unknown opcode"
  | Invalid_trailing_byte -> "invalid trailing byte"
  | Invalid_trailing_short -> "invalid trailing short"
  | Internal -> "internal error"
  | Invalid_switch_cases -> "invalid number of switch cases"

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* I/O functions *)

let read st o =
  let ofs = ref o in
  let read_u1 () = ofs := !ofs + 1; InputStream.read_u1 st in
  let read_s1 () = ofs := !ofs + 1; InputStream.read_s1 st in
  let read_u2 () = ofs := !ofs + 2; InputStream.read_u2 st in
  let read_s2 () = ofs := !ofs + 2; InputStream.read_s2 st in
  let read_s4 () = ofs := !ofs + 4; InputStream.read_s4 st in
  let unpad () =
    while (!ofs mod 4) <> 0 do
      let b = read_u1 () in
      if (b :> int) <> 0 then
        fail Invalid_padding_byte
    done in
  let read_opcode () = try read_u1 () with _ -> raise Not_found in
  let read_s4_list hi lo =
    let hi = (hi : s4 :> int32) in
    let lo = (lo : s4 :> int32) in
    let nb = Int32.succ (Int32.sub hi lo) in Array.to_list (Array.init (Int32.to_int nb) (fun _ -> read_s4 ())) in
  let read_s4_pair_list nb =
    let nb = (nb : s4 :> int32) in
    Array.to_list (Array.init (Int32.to_int nb) (fun _ -> let x = read_s4 () in let y = read_s4 () in (x, y))) in
  let (++) tl hd = hd :: tl in
  let res = ref [] in
  try
    while true do
      res := !res ++ match (read_opcode () :> int) with
        | 0x00 -> NOP
        | 0x01 -> ACONST_NULL
        | 0x02 -> ICONST_M1
        | 0x03 -> ICONST_0
        | 0x04 -> ICONST_1
        | 0x05 -> ICONST_2
        | 0x06 -> ICONST_3
        | 0x07 -> ICONST_4
        | 0x08 -> ICONST_5
        | 0x09 -> LCONST_0
        | 0x0A -> LCONST_1
        | 0x0B -> FCONST_0
        | 0x0C -> FCONST_1
        | 0x0D -> FCONST_2
        | 0x0E -> DCONST_0
        | 0x0F -> DCONST_1
        | 0x10 -> BIPUSH (read_s1 ())
        | 0x11 -> SIPUSH (read_s2 ())
        | 0x12 -> LDC (read_u1 ())
        | 0x13 -> LDC_W (read_u2 ())
        | 0x14 -> LDC2_W (read_u2 ())
        | 0x15 -> ILOAD (read_u1 ())
        | 0x16 -> LLOAD (read_u1 ())
        | 0x17 -> FLOAD (read_u1 ())
        | 0x18 -> DLOAD (read_u1 ())
        | 0x19 -> ALOAD (read_u1 ())
        | 0x1A -> ILOAD_0
        | 0x1B -> ILOAD_1
        | 0x1C -> ILOAD_2
        | 0x1D -> ILOAD_3
        | 0x1E -> LLOAD_0
        | 0x1F -> LLOAD_1
        | 0x20 -> LLOAD_2
        | 0x21 -> LLOAD_3
        | 0x22 -> FLOAD_0
        | 0x23 -> FLOAD_1
        | 0x24 -> FLOAD_2
        | 0x25 -> FLOAD_3
        | 0x26 -> DLOAD_0
        | 0x27 -> DLOAD_1
        | 0x28 -> DLOAD_2
        | 0x29 -> DLOAD_3
        | 0x2A -> ALOAD_0
        | 0x2B -> ALOAD_1
        | 0x2C -> ALOAD_2
        | 0x2D -> ALOAD_3
        | 0x2E -> IALOAD
        | 0x2F -> LALOAD
        | 0x30 -> FALOAD
        | 0x31 -> DALOAD
        | 0x32 -> AALOAD
        | 0x33 -> BALOAD
        | 0x34 -> CALOAD
        | 0x35 -> SALOAD
        | 0x36 -> ISTORE (read_u1 ())
        | 0x37 -> LSTORE (read_u1 ())
        | 0x38 -> FSTORE (read_u1 ())
        | 0x39 -> DSTORE (read_u1 ())
        | 0x3A -> ASTORE (read_u1 ())
        | 0x3B -> ISTORE_0
        | 0x3C -> ISTORE_1
        | 0x3D -> ISTORE_2
        | 0x3E -> ISTORE_3
        | 0x3F -> LSTORE_0
        | 0x40 -> LSTORE_1
        | 0x41 -> LSTORE_2
        | 0x42 -> LSTORE_3
        | 0x43 -> FSTORE_0
        | 0x44 -> FSTORE_1
        | 0x45 -> FSTORE_2
        | 0x46 -> FSTORE_3
        | 0x47 -> DSTORE_0
        | 0x48 -> DSTORE_1
        | 0x49 -> DSTORE_2
        | 0x4A -> DSTORE_3
        | 0x4B -> ASTORE_0
        | 0x4C -> ASTORE_1
        | 0x4D -> ASTORE_2
        | 0x4E -> ASTORE_3
        | 0x4F -> IASTORE
        | 0x50 -> LASTORE
        | 0x51 -> FASTORE
        | 0x52 -> DASTORE
        | 0x53 -> AASTORE
        | 0x54 -> BASTORE
        | 0x55 -> CASTORE
        | 0x56 -> SASTORE
        | 0x57 -> POP
        | 0x58 -> POP2
        | 0x59 -> DUP
        | 0x5A -> DUP_X1
        | 0x5B -> DUP_X2
        | 0x5C -> DUP2
        | 0x5D -> DUP2_X1
        | 0x5E -> DUP2_X2
        | 0x5F -> SWAP
        | 0x60 -> IADD
        | 0x61 -> LADD
        | 0x62 -> FADD
        | 0x63 -> DADD
        | 0x64 -> ISUB
        | 0x65 -> LSUB
        | 0x66 -> FSUB
        | 0x67 -> DSUB
        | 0x68 -> IMUL
        | 0x69 -> LMUL
        | 0x6A -> FMUL
        | 0x6B -> DMUL
        | 0x6C -> IDIV
        | 0x6D -> LDIV
        | 0x6E -> FDIV
        | 0x6F -> DDIV
        | 0x70 -> IREM
        | 0x71 -> LREM
        | 0x72 -> FREM
        | 0x73 -> DREM
        | 0x74 -> INEG
        | 0x75 -> LNEG
        | 0x76 -> FNEG
        | 0x77 -> DNEG
        | 0x78 -> ISHL
        | 0x79 -> LSHL
        | 0x7A -> ISHR
        | 0x7B -> LSHR
        | 0x7C -> IUSHR
        | 0x7D -> LUSHR
        | 0x7E -> IAND
        | 0x7F -> LAND
        | 0x80 -> IOR
        | 0x81 -> LOR
        | 0x82 -> IXOR
        | 0x83 -> LXOR
        | 0x84 -> let p1 = read_u1 () in let p2 = read_s1 () in IINC (p1, p2)
        | 0x85 -> I2L
        | 0x86 -> I2F
        | 0x87 -> I2D
        | 0x88 -> L2I
        | 0x89 -> L2F
        | 0x8A -> L2D
        | 0x8B -> F2I
        | 0x8C -> F2L
        | 0x8D -> F2D
        | 0x8E -> D2I
        | 0x8F -> D2L
        | 0x90 -> D2F
        | 0x91 -> I2B
        | 0x92 -> I2C
        | 0x93 -> I2S
        | 0x94 -> LCMP
        | 0x95 -> FCMPL
        | 0x96 -> FCMPG
        | 0x97 -> DCMPL
        | 0x98 -> DCMPG
        | 0x99 -> IFEQ (read_s2 ())
        | 0x9A -> IFNE (read_s2 ())
        | 0x9B -> IFLT (read_s2 ())
        | 0x9C -> IFGE (read_s2 ())
        | 0x9D -> IFGT (read_s2 ())
        | 0x9E -> IFLE (read_s2 ())
        | 0x9F -> IF_ICMPEQ (read_s2 ())
        | 0xA0 -> IF_ICMPNE (read_s2 ())
        | 0xA1 -> IF_ICMPLT (read_s2 ())
        | 0xA2 -> IF_ICMPGE (read_s2 ())
        | 0xA3 -> IF_ICMPGT (read_s2 ())
        | 0xA4 -> IF_ICMPLE (read_s2 ())
        | 0xA5 -> IF_ACMPEQ (read_s2 ())
        | 0xA6 -> IF_ACMPNE (read_s2 ())
        | 0xA7 -> GOTO (read_s2 ())
        | 0xA8 -> JSR (read_s2 ())
        | 0xA9 -> RET (read_u1 ())
        | 0xAA -> unpad (); let def = read_s4 () in let low = read_s4 () in let high = read_s4 () in let offsets = read_s4_list high low in TABLESWITCH (def, low, high, offsets)
        | 0xAB -> unpad (); let def = read_s4 () in let nb = read_s4 () in let pairs = read_s4_pair_list nb in LOOKUPSWITCH (def, nb, pairs)
        | 0xAC -> IRETURN
        | 0xAD -> LRETURN
        | 0xAE -> FRETURN
        | 0xAF -> DRETURN
        | 0xB0 -> ARETURN
        | 0xB1 -> RETURN
        | 0xB2 -> GETSTATIC (read_u2 ())
        | 0xB3 -> PUTSTATIC (read_u2 ())
        | 0xB4 -> GETFIELD (read_u2 ())
        | 0xB5 -> PUTFIELD (read_u2 ())
        | 0xB6 -> INVOKEVIRTUAL (read_u2 ())
        | 0xB7 -> INVOKESPECIAL (read_u2 ())
        | 0xB8 -> INVOKESTATIC (read_u2 ())
        | 0xB9 -> let res = (let p1 = read_u2 () in let p2 = read_u1 () in INVOKEINTERFACE (p1, p2)) in let b = read_u1 () in if (b :> int) = 0 then res else fail Invalid_trailing_byte
        | 0xBA -> let res = (let p1 = read_u2 () in INVOKEDYNAMIC p1) in let b = read_u2 () in if (b :> int) = 0 then res else fail Invalid_trailing_short
        | 0xBB -> NEW (read_u2 ())
        | 0xBC -> NEWARRAY (read_u1 ())
        | 0xBD -> ANEWARRAY (read_u2 ())
        | 0xBE -> ARRAYLENGTH
        | 0xBF -> ATHROW
        | 0xC0 -> CHECKCAST (read_u2 ())
        | 0xC1 -> INSTANCEOF (read_u2 ())
        | 0xC2 -> MONITORENTER
        | 0xC3 -> MONITOREXIT
        | 0xC4 -> (* WIDE *)
          begin
            match (read_u1 () :> int) with
            | 0x15 -> WIDE_ILOAD (read_u2 ())
            | 0x16 -> WIDE_LLOAD (read_u2 ())
            | 0x17 -> WIDE_FLOAD (read_u2 ())
            | 0x18 -> WIDE_DLOAD (read_u2 ())
            | 0x19 -> WIDE_ALOAD (read_u2 ())
            | 0x36 -> WIDE_ISTORE (read_u2 ())
            | 0x37 -> WIDE_LSTORE (read_u2 ())
            | 0x38 -> WIDE_FSTORE (read_u2 ())
            | 0x39 -> WIDE_DSTORE (read_u2 ())
            | 0x3A -> WIDE_ASTORE (read_u2 ())
            | 0x84 -> let p1 = read_u2 () in let p2 = read_s2 () in WIDE_IINC (p1, p2)
            | 0xA9 -> WIDE_RET (read_u2 ())
            | _ -> fail Unknown_opcode
          end
        | 0xC5 -> let p1 = read_u2 () in let p2 = read_u1 () in MULTIANEWARRAY (p1, p2)
        | 0xC6 -> IFNULL (read_s2 ())
        | 0xC7 -> IFNONNULL (read_s2 ())
        | 0xC8 -> GOTO_W (read_s4 ())
        | 0xC9 -> JSR_W (read_s4 ())
        | _ -> fail Unknown_opcode
    done;
    fail Internal
  with Not_found -> List.rev !res

let write st o l =
  let ofs = ref o in
  let write_u1 x = OutputStream.write_u1 st x; ofs := !ofs + 1 in
  let write_u1' x = OutputStream.write_u1 st (u1 x); ofs := !ofs + 1 in
  let write_s1 x = OutputStream.write_s1 st x; ofs := !ofs + 1 in
  let write_u2 x = OutputStream.write_u2 st x; ofs := !ofs + 2 in
  let write_u2' x = OutputStream.write_u2 st (u2 x); ofs := !ofs + 2 in
  let write_s2 x = OutputStream.write_s2 st x; ofs := !ofs + 2 in
  let write_s4 x = OutputStream.write_s4 st x; ofs := !ofs + 4 in
  let pad () = while (!ofs mod 4) <> 0 do write_u1' 0 done in
  let write_instruction = function
    | AALOAD -> write_u1' 0x32
    | AASTORE -> write_u1' 0x53
    | ACONST_NULL -> write_u1' 0x01
    | ALOAD p1 -> write_u1' 0x19; write_u1 p1
    | ALOAD_0 -> write_u1' 0x2A
    | ALOAD_1 -> write_u1' 0x2B
    | ALOAD_2 -> write_u1' 0x2C
    | ALOAD_3 -> write_u1' 0x2D
    | ANEWARRAY p1 -> write_u1' 0xBD; write_u2 p1
    | ARETURN -> write_u1' 0xB0
    | ARRAYLENGTH -> write_u1' 0xBE
    | ASTORE p1 -> write_u1' 0x3A; write_u1 p1
    | ASTORE_0 -> write_u1' 0x4B
    | ASTORE_1 -> write_u1' 0x4C
    | ASTORE_2 -> write_u1' 0x4D
    | ASTORE_3 -> write_u1' 0x4E
    | ATHROW -> write_u1' 0xBF
    | BALOAD -> write_u1' 0x33
    | BASTORE -> write_u1' 0x54
    | BIPUSH p1 -> write_u1' 0x10; write_s1 p1
    | CALOAD -> write_u1' 0x34
    | CASTORE -> write_u1' 0x55
    | CHECKCAST p1 -> write_u1' 0xC0; write_u2 p1
    | D2F -> write_u1' 0x90
    | D2I -> write_u1' 0x8E
    | D2L -> write_u1' 0x8F
    | DADD -> write_u1' 0x63
    | DALOAD -> write_u1' 0x31
    | DASTORE -> write_u1' 0x52
    | DCMPG -> write_u1' 0x98
    | DCMPL -> write_u1' 0x97
    | DCONST_0 -> write_u1' 0x0E
    | DCONST_1 -> write_u1' 0x0F
    | DDIV -> write_u1' 0x6F
    | DLOAD p1 -> write_u1' 0x18; write_u1 p1
    | DLOAD_0 -> write_u1' 0x26
    | DLOAD_1 -> write_u1' 0x27
    | DLOAD_2 -> write_u1' 0x28
    | DLOAD_3 -> write_u1' 0x29
    | DMUL -> write_u1' 0x6B
    | DNEG -> write_u1' 0x77
    | DREM -> write_u1' 0x73
    | DRETURN -> write_u1' 0xAF
    | DSTORE p1 -> write_u1' 0x39; write_u1 p1
    | DSTORE_0 -> write_u1' 0x47
    | DSTORE_1 -> write_u1' 0x48
    | DSTORE_2 -> write_u1' 0x49
    | DSTORE_3 -> write_u1' 0x4A
    | DSUB -> write_u1' 0x67
    | DUP -> write_u1' 0x59
    | DUP2 -> write_u1' 0x5C
    | DUP2_X1 -> write_u1' 0x5D
    | DUP2_X2 -> write_u1' 0x5E
    | DUP_X1 -> write_u1' 0x5A
    | DUP_X2 -> write_u1' 0x5B
    | F2D -> write_u1' 0x8D
    | F2I -> write_u1' 0x8B
    | F2L -> write_u1' 0x8C
    | FADD -> write_u1' 0x62
    | FALOAD -> write_u1' 0x30
    | FASTORE -> write_u1' 0x51
    | FCMPG -> write_u1' 0x96
    | FCMPL -> write_u1' 0x95
    | FCONST_0 -> write_u1' 0x0B
    | FCONST_1 -> write_u1' 0x0C
    | FCONST_2 -> write_u1' 0x0D
    | FDIV -> write_u1' 0x6E
    | FLOAD p1 -> write_u1' 0x17; write_u1 p1
    | FLOAD_0 -> write_u1' 0x22
    | FLOAD_1 -> write_u1' 0x23
    | FLOAD_2 -> write_u1' 0x24
    | FLOAD_3 -> write_u1' 0x25
    | FMUL -> write_u1' 0x6A
    | FNEG -> write_u1' 0x76
    | FREM -> write_u1' 0x72
    | FRETURN -> write_u1' 0xAE
    | FSTORE p1 -> write_u1' 0x38; write_u1 p1
    | FSTORE_0 -> write_u1' 0x43
    | FSTORE_1 -> write_u1' 0x44
    | FSTORE_2 -> write_u1' 0x45
    | FSTORE_3 -> write_u1' 0x46
    | FSUB -> write_u1' 0x66
    | GETFIELD p1 -> write_u1' 0xB4; write_u2 p1
    | GETSTATIC p1 -> write_u1' 0xB2; write_u2 p1
    | GOTO p1 -> write_u1' 0xA7; write_s2 p1
    | GOTO_W p1 -> write_u1' 0xC8; write_s4 p1
    | I2B -> write_u1' 0x91
    | I2C -> write_u1' 0x92
    | I2D -> write_u1' 0x87
    | I2F -> write_u1' 0x86
    | I2L -> write_u1' 0x85
    | I2S -> write_u1' 0x93
    | IADD -> write_u1' 0x60
    | IALOAD -> write_u1' 0x2E
    | IAND -> write_u1' 0x7E
    | IASTORE -> write_u1' 0x4F
    | ICONST_0 -> write_u1' 0x03
    | ICONST_1 -> write_u1' 0x04
    | ICONST_2 -> write_u1' 0x05
    | ICONST_3 -> write_u1' 0x06
    | ICONST_4 -> write_u1' 0x07
    | ICONST_5 -> write_u1' 0x08
    | ICONST_M1 -> write_u1' 0x02
    | IDIV -> write_u1' 0x6C
    | IF_ACMPEQ p1 -> write_u1' 0xA5; write_s2 p1
    | IF_ACMPNE p1 -> write_u1' 0xA6; write_s2 p1
    | IF_ICMPEQ p1 -> write_u1' 0x9F; write_s2 p1
    | IF_ICMPGE p1 -> write_u1' 0xA2; write_s2 p1
    | IF_ICMPGT p1 -> write_u1' 0xA3; write_s2 p1
    | IF_ICMPLE p1 -> write_u1' 0xA4; write_s2 p1
    | IF_ICMPLT p1 -> write_u1' 0xA1; write_s2 p1
    | IF_ICMPNE p1 -> write_u1' 0xA0; write_s2 p1
    | IFEQ p1 -> write_u1' 0x99; write_s2 p1
    | IFGE p1 -> write_u1' 0x9C; write_s2 p1
    | IFGT p1 -> write_u1' 0x9D; write_s2 p1
    | IFLE p1 -> write_u1' 0x9E; write_s2 p1
    | IFLT p1 -> write_u1' 0x9B; write_s2 p1
    | IFNE p1 -> write_u1' 0x9A; write_s2 p1
    | IFNONNULL p1 -> write_u1' 0xC7; write_s2 p1
    | IFNULL p1 -> write_u1' 0xC6; write_s2 p1
    | IINC (p1, p2) -> write_u1' 0x84; write_u1 p1; write_s1 p2
    | ILOAD p1 -> write_u1' 0x15; write_u1 p1
    | ILOAD_0 -> write_u1' 0x1A
    | ILOAD_1 -> write_u1' 0x1B
    | ILOAD_2 -> write_u1' 0x1C
    | ILOAD_3 -> write_u1' 0x1D
    | IMUL -> write_u1' 0x68
    | INEG -> write_u1' 0x74
    | INSTANCEOF p1 -> write_u1' 0xC1; write_u2 p1
    | INVOKEDYNAMIC p1 -> write_u1' 0xBA; write_u2 p1; write_u2' 0
    | INVOKEINTERFACE (p1, p2) -> write_u1' 0xB9; write_u2 p1; write_u1 p2; write_u1' 0
    | INVOKESPECIAL p1 -> write_u1' 0xB7; write_u2 p1
    | INVOKESTATIC p1 -> write_u1' 0xB8; write_u2 p1
    | INVOKEVIRTUAL p1 -> write_u1' 0xB6; write_u2 p1
    | IOR -> write_u1' 0x80
    | IREM -> write_u1' 0x70
    | IRETURN -> write_u1' 0xAC
    | ISHL -> write_u1' 0x78
    | ISHR -> write_u1' 0x7A
    | ISTORE p1 -> write_u1' 0x36; write_u1 p1
    | ISTORE_0 -> write_u1' 0x3B
    | ISTORE_1 -> write_u1' 0x3C
    | ISTORE_2 -> write_u1' 0x3D
    | ISTORE_3 -> write_u1' 0x3E
    | ISUB -> write_u1' 0x64
    | IUSHR -> write_u1' 0x7C
    | IXOR -> write_u1' 0x82
    | JSR p1 -> write_u1' 0xA8; write_s2 p1
    | JSR_W p1 -> write_u1' 0xC9; write_s4 p1
    | L2D -> write_u1' 0x8A
    | L2F -> write_u1' 0x89
    | L2I -> write_u1' 0x88
    | LADD -> write_u1' 0x61
    | LALOAD -> write_u1' 0x2F
    | LAND -> write_u1' 0x7F
    | LASTORE -> write_u1' 0x50
    | LCMP -> write_u1' 0x94
    | LCONST_0 -> write_u1' 0x09
    | LCONST_1 -> write_u1' 0x0A
    | LDC p1 -> write_u1' 0x12; write_u1 p1
    | LDC2_W p1 -> write_u1' 0x14; write_u2 p1
    | LDC_W p1 -> write_u1' 0x13; write_u2 p1
    | LDIV -> write_u1' 0x6D
    | LLOAD p1 -> write_u1' 0x16; write_u1 p1
    | LLOAD_0 -> write_u1' 0x1E
    | LLOAD_1 -> write_u1' 0x1F
    | LLOAD_2 -> write_u1' 0x20
    | LLOAD_3 -> write_u1' 0x21
    | LMUL -> write_u1' 0x69
    | LNEG -> write_u1' 0x75
    | LOOKUPSWITCH (p1, p2, p3) -> write_u1' 0xAB; pad (); write_s4 p1; write_s4 p2; if (p2 :> int32) <> Int32.of_int (List.length p3) then fail Invalid_switch_cases else List.iter (fun (x, y) -> write_s4 x; write_s4 y) p3
    | LOR -> write_u1' 0x81
    | LREM -> write_u1' 0x71
    | LRETURN -> write_u1' 0xAD
    | LSHL -> write_u1' 0x79
    | LSHR -> write_u1' 0x7B
    | LSTORE p1 -> write_u1' 0x37; write_u1 p1
    | LSTORE_0 -> write_u1' 0x3F
    | LSTORE_1 -> write_u1' 0x40
    | LSTORE_2 -> write_u1' 0x41
    | LSTORE_3 -> write_u1' 0x42
    | LSUB -> write_u1' 0x65
    | LUSHR -> write_u1' 0x7D
    | LXOR -> write_u1' 0x83
    | MONITORENTER -> write_u1' 0xC2
    | MONITOREXIT -> write_u1' 0xC3
    | MULTIANEWARRAY (p1, p2) -> write_u1' 0xC5; write_u2 p1; write_u1 p2
    | NEW p1 -> write_u1' 0xBB; write_u2 p1
    | NEWARRAY p1 -> write_u1' 0xBC; write_u1 p1
    | NOP -> write_u1' 0x00
    | POP -> write_u1' 0x57
    | POP2 -> write_u1' 0x58
    | PUTFIELD p1 -> write_u1' 0xB5; write_u2 p1
    | PUTSTATIC p1 -> write_u1' 0xB3; write_u2 p1
    | RET p1 -> write_u1' 0xA9; write_u1 p1
    | RETURN -> write_u1' 0xB1
    | SALOAD -> write_u1' 0x35
    | SASTORE -> write_u1' 0x56
    | SIPUSH p1 -> write_u1' 0x11; write_s2 p1
    | SWAP -> write_u1' 0x5F
    | TABLESWITCH (p1, p2, p3, p4) -> write_u1' 0xAA; pad (); write_s4 p1; write_s4 p2; write_s4 p3; if (Int32.succ (Int32.sub (p3 :> int32) (p2 :> int32))) <> Int32.of_int (List.length p4) then fail Invalid_switch_cases else List.iter write_s4 p4
    | WIDE_ALOAD p1 -> write_u1' 0xC4; write_u1' 0x19; write_u2 p1
    | WIDE_ASTORE p1 -> write_u1' 0xC4; write_u1' 0x3A; write_u2 p1
    | WIDE_DLOAD p1 -> write_u1' 0xC4; write_u1' 0x18; write_u2 p1
    | WIDE_DSTORE p1 -> write_u1' 0xC4; write_u1' 0x39; write_u2 p1
    | WIDE_FLOAD p1 -> write_u1' 0xC4; write_u1' 0x17; write_u2 p1
    | WIDE_FSTORE p1 -> write_u1' 0xC4; write_u1' 0x38; write_u2 p1
    | WIDE_IINC (p1, p2) -> write_u1' 0xC4; write_u1' 0x84; write_u2 p1; write_s2 p2
    | WIDE_ILOAD p1 -> write_u1' 0xC4; write_u1' 0x15; write_u2 p1
    | WIDE_ISTORE p1 -> write_u1' 0xC4; write_u1' 0x36; write_u2 p1
    | WIDE_LLOAD p1 -> write_u1' 0xC4; write_u1' 0x16; write_u2 p1
    | WIDE_LSTORE p1 -> write_u1' 0xC4; write_u1' 0x37; write_u2 p1
    | WIDE_RET p1 -> write_u1' 0xC4; write_u1' 0xA9; write_u2 p1
  in List.iter write_instruction l

(*
let size_of _ _ = failwith "todo"
*)
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
