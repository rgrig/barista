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

.method private static float sum(float, float, float, float, float)
        .max_stack 5
        .max_locals 5
        fload_0
        fload_1
        fload_2
        fload_3
        fload 4
        fadd
        fadd
        fadd
        fadd
        freturn

.method private static double sum(double, double, double)
        .max_stack 6
        .max_locals 6
        dload_0
        dload_2
        dload 4
        dadd
        dadd
        dreturn

.method private static int sum(int, int, int, int, int, int, int)
        .max_stack 7
        .max_locals 7
        iload_0
        iload_1
        iload_2
        iload_3
        iload 4
        iload 5
        iload 6
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        ireturn

.method private static long sum(long, long, long)
        .max_stack 6
        .max_locals 6
        lload_0
        lload_2
        lload 4
        ladd
        ladd
        lreturn


.method public static void main(java.lang.String[])
        .max_stack 8

        # float
        getstatic java.lang.System.out:java.io.PrintStream
        fconst_0
        fconst_1
        fconst_1
        fconst_2
        fconst_2
        invokestatic pack.Test.sum(float, float, float, float, float):float
        invokevirtual java.io.PrintStream.println(float):void

        # double
        getstatic java.lang.System.out:java.io.PrintStream
        dconst_0
        dconst_1
        dconst_1
        invokestatic pack.Test.sum(double, double, double):double
        invokevirtual java.io.PrintStream.println(double):void

        # int
        getstatic java.lang.System.out:java.io.PrintStream
        iconst_0
        iconst_1
        iconst_2
        iconst_3
        iconst_4
        iconst_5
        iconst_m1
        invokestatic pack.Test.sum(int, int, int, int, int, int, int):int
        invokevirtual java.io.PrintStream.println(int):void

        # long
        getstatic java.lang.System.out:java.io.PrintStream
        lconst_0
        lconst_1
        lconst_1
        invokestatic pack.Test.sum(long, long, long):long
        invokevirtual java.io.PrintStream.println(long):void

        return
