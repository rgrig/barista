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

.method public static void main(java.lang.String[])
        .max_stack 5

        # ddiv
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 07.5
        ldc2_w 2.5
        ddiv
        invokevirtual java.io.PrintStream.println(double):void

        # dmul
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 1.5
        ldc2_w 5.0
        dmul
        invokevirtual java.io.PrintStream.println(double):void

        # dneg
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 1.5
        dneg
        invokevirtual java.io.PrintStream.println(double):void

        # drem
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 5.0
        ldc2_w 1.5
        drem
        invokevirtual java.io.PrintStream.println(double):void

        # dsub
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 5.0
        ldc2_w 1.5
        dsub
        invokevirtual java.io.PrintStream.println(double):void

        # fdiv
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 7.5
        ldc 2.5
        fdiv
        invokevirtual java.io.PrintStream.println(float):void

        # fmul
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 1.5
        ldc 5.0
        fmul
        invokevirtual java.io.PrintStream.println(float):void

        # fneg
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 1.5
        fneg
        invokevirtual java.io.PrintStream.println(float):void

        # frem
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 5.0
        ldc 1.5
        frem
        invokevirtual java.io.PrintStream.println(float):void

        # fsub
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 5.0
        ldc 1.5
        fsub
        invokevirtual java.io.PrintStream.println(float):void

        # iand
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 0xFC
        ldc 0xAB
        iand
        invokevirtual java.io.PrintStream.println(int):void

        # idiv
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 7
        idiv
        invokevirtual java.io.PrintStream.println(int):void

        # imul
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 6
        ldc 7
        imul
        invokevirtual java.io.PrintStream.println(int):void

        # ineg
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 7
        ineg
        invokevirtual java.io.PrintStream.println(int):void

        # ior
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 4
        ldc 3
        ior
        invokevirtual java.io.PrintStream.println(int):void

        # irem
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 7
        irem
        invokevirtual java.io.PrintStream.println(int):void

        # ishl
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 2
        ishl
        invokevirtual java.io.PrintStream.println(int):void

        # ishr
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 2
        ishr
        invokevirtual java.io.PrintStream.println(int):void

        # isub
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 17
        isub
        invokevirtual java.io.PrintStream.println(int):void

        # iushr
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 24
        ldc 2
        iushr
        invokevirtual java.io.PrintStream.println(int):void

        # ixor
        getstatic java.lang.System.out:java.io.PrintStream
        ldc 27
        ldc 8
        ixor
        invokevirtual java.io.PrintStream.println(int):void

        # land
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 0xFC
        ldc2_w 0xAB
        land
        invokevirtual java.io.PrintStream.println(long):void

        # ldiv
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc2_w 7
        ldiv
        invokevirtual java.io.PrintStream.println(long):void

        # lmul
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 6
        ldc2_w 7
        lmul
        invokevirtual java.io.PrintStream.println(long):void

        # lneg
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 7
        lneg
        invokevirtual java.io.PrintStream.println(long):void

        # lor
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 4
        ldc2_w 3
        lor
        invokevirtual java.io.PrintStream.println(long):void

        # lrem
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc2_w 7
        lrem
        invokevirtual java.io.PrintStream.println(long):void

        # lshl
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc 2
        lshl
        invokevirtual java.io.PrintStream.println(long):void

        # lshr
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc 2
        lshr
        invokevirtual java.io.PrintStream.println(long):void

        # lsub
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc2_w 17
        lsub
        invokevirtual java.io.PrintStream.println(long):void

        # lushr
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 24
        ldc 2
        lushr
        invokevirtual java.io.PrintStream.println(long):void

        # lxor
        getstatic java.lang.System.out:java.io.PrintStream
        ldc2_w 27
        ldc2_w 8
        lxor
        invokevirtual java.io.PrintStream.println(long):void

        return
