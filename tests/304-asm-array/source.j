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

.field public static final int N
        @ConstantValue 5

.method private static void floats()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray float
        dup
        dup
        iconst_1
        faload
        fconst_2
        fadd
        iconst_1
        swap
        fastore
        invokestatic java.util.Arrays.toString(float[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void doubles()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray double
        dup
        dup
        iconst_1
        daload
        dconst_1
        dadd
        dstore_0
        iconst_1
        dload_0
        dastore
        invokestatic java.util.Arrays.toString(double[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void ints()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray int
        dup
        dup
        iconst_1
        iaload
        iconst_3
        iadd
        iconst_1
        swap
        iastore
        invokestatic java.util.Arrays.toString(int[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void longs()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray long
        dup
        dup
        iconst_2
        laload
        lconst_1
        ladd
        lstore_0
        iconst_2
        lload_0
        lastore
        invokestatic java.util.Arrays.toString(long[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void bytes()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray byte
        dup
        dup
        iconst_1
        baload
        iconst_3
        iadd
        iconst_1
        swap
        bastore
        invokestatic java.util.Arrays.toString(byte[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void chars()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray char
        dup
        dup
        dup
        iconst_1
        caload
        iconst_3
        iadd
        iconst_1
        swap
        castore
        iconst_2
        ldc 'Z'
        castore
        invokestatic java.util.Arrays.toString(char[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void shorts()
        .max_stack 8
        getstatic pack.Test.N:int
        newarray short
        dup
        dup
        iconst_1
        saload
        iconst_3
        iadd
        iconst_1
        swap
        sastore
        invokestatic java.util.Arrays.toString(short[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void strings()
        .max_stack 8
        getstatic pack.Test.N:int
        anewarray java.lang.String
        dup
        dup
        dup
        dup
        dup
        iconst_0
        ldc "first"
        aastore
        iconst_1
        ldc "second"
        aastore
        iconst_2
        ldc "third"
        aastore
        iconst_3
        ldc "fourth"
        aastore
        iconst_4
        ldc "fifth"
        aastore
        invokestatic java.util.Arrays.toString(java.lang.Object[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method private static void matrix()
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "matrix:"
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        getstatic pack.Test.N:int
        dup
        multianewarray java.lang.String[][] 2
        astore_0
        iconst_0
        istore_1
        aload_0
        arraylength
        istore_2
loop:
        iload_1
        iload_2
        if_icmpeq end:
        aload_0
        iload_1
        aaload
        invokestatic java.util.Arrays.toString(java.lang.Object[]):java.lang.String
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto loop:
end:
        return

.method private static void classes()
        getstatic java.lang.System.out:java.io.PrintStream
        ldc int[]
        invokevirtual java.lang.Object.toString():java.lang.String
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return

.method public static void main(java.lang.String[])
        invokestatic pack.Test.floats():void
        invokestatic pack.Test.doubles():void
        invokestatic pack.Test.ints():void
        invokestatic pack.Test.longs():void
        invokestatic pack.Test.bytes():void
        invokestatic pack.Test.chars():void
        invokestatic pack.Test.shorts():void
        invokestatic pack.Test.strings():void
        invokestatic pack.Test.matrix():void
        invokestatic pack.Test.classes():void
        return
