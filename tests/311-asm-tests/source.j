#
# This file is part of Barista.
# Copyright (C) 2007-2011 Xavier Clerc.
#
# Barista is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Barista is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.class public final pack.Test
.extends java.lang.Object

.method private static void result(int)
        iload_0
        lookupswitch default: 3
                -1 => lower:
                0 => equal:
                1 => greater:
lower:
        ldc "lower"
        goto end:
equal:
        ldc "equal"
        goto end:
greater:
        ldc "greater"
        goto end:
default:
        ldc "default"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void double()
        ldc2_w 1.2
        ldc2_w 3.4
        dcmpg
        invokestatic pack.Test.result(int):void
        ldc2_w 5.6
        ldc2_w 3.4
        dcmpl
        invokestatic pack.Test.result(int):void
        return

.method private static void float()
        ldc 1.2
        ldc 1.2
        fcmpg
        invokestatic pack.Test.result(int):void
        ldc 5.6
        ldc 3.4
        fcmpl
        invokestatic pack.Test.result(int):void
        return

.method private static void long()
        ldc2_w 12
        ldc2_w 34
        lcmp
        invokestatic pack.Test.result(int):void
        ldc2_w 56
        ldc2_w 34
        lcmp
        invokestatic pack.Test.result(int):void
        return

.method private static void instanceof()
        new java.lang.String
        dup
        invokespecial java.lang.String.<init>():void
        instanceof java.lang.Integer
        ifne inst:
        ldc "not inst"
        goto end:
inst:
        ldc "inst"
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void null(java.lang.Object)
        aload_0
        ifnull null:
        aload_0
        ifnonnull nonnull:
        goto error:
null:
        ldc "null"
        goto end:
nonnull:
        ldc "non null"
        goto end:
error:
        ldc "error"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void ref_eq(java.lang.Object,java.lang.Object)
        aload_0
        aload_1
        if_acmpeq eq:
        aload_0
        aload_1
        if_acmpne ne:
        ldc "error"
        goto end:
eq:
        ldc "equal references"
        goto end:
ne:
        ldc "different references"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void int_eq(int,int)
        iload_0
        iload_1
        if_icmpeq eq:
        iload_0
        iload_1
        if_icmpne ne:
        iload_0
        iload_1
        if_icmplt lt:
        iload_0
        iload_1
        if_icmple le:
        iload_0
        iload_1
        if_icmpgt gt:
        iload_0
        iload_1
        if_icmpge ge:
        ldc "error"
        goto end:
eq:
        ldc "eq"
        goto end:
ne:
        ldc "ne"
        goto end:
lt:
        ldc "lt/never reached"
        goto end:
le:
        ldc "le/never reached"
        goto end:
gt:
        ldc "gt/never reached"
        goto end:
ge:
        ldc "ge/never reached"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void int_eq0(int)
        iload_0
        ifeq eq:
        iload_0
        ifne ne:
        iload_0
        iflt lt:
        iload_0
        ifle le:
        iload_0
        ifgt gt:
        iload_0
        ifge ge:
        ldc "error"
        goto end:
eq:
        ldc "eq"
        goto end:
ne:
        ldc "ne"
        goto end:
lt:
        ldc "lt/never reached"
        goto end:
le:
        ldc "le/never reached"
        goto end:
gt:
        ldc "gt/never reached"
        goto end:
ge:
        ldc "ge/never reached"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method public static void main(java.lang.String[])
        invokestatic pack.Test.double():void
        invokestatic pack.Test.float():void
        invokestatic pack.Test.long():void
        invokestatic pack.Test.instanceof():void

        aconst_null
        invokestatic pack.Test.null(java.lang.Object):void

        new java.lang.String
        dup
        invokespecial java.lang.String.<init>():void
        invokestatic pack.Test.null(java.lang.Object):void

        aconst_null
        new java.lang.String
        dup
        invokespecial java.lang.String.<init>():void
        invokestatic pack.Test.ref_eq(java.lang.Object,java.lang.Object):void

        new java.lang.String
        dup
        dup
        invokespecial java.lang.String.<init>():void
        invokestatic pack.Test.ref_eq(java.lang.Object,java.lang.Object):void

        ldc 3
        ldc 4
        invokestatic pack.Test.int_eq(int,int):void

        ldc 3
        ldc -4
        invokestatic pack.Test.int_eq(int,int):void

        ldc 3
        ldc 3
        invokestatic pack.Test.int_eq(int,int):void

        ldc 4
        invokestatic pack.Test.int_eq0(int):void

        ldc -4
        invokestatic pack.Test.int_eq0(int):void

        ldc 0
        invokestatic pack.Test.int_eq0(int):void

        return
