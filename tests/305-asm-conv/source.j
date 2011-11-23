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
        # d2f
        getstatic java.lang.System.out:java.io.PrintStream
        dconst_1
        d2f
        invokevirtual java.io.PrintStream.println(float):void

        # d2i
        getstatic java.lang.System.out:java.io.PrintStream
        dconst_1
        d2i
        invokevirtual java.io.PrintStream.println(int):void

        # d2l
        getstatic java.lang.System.out:java.io.PrintStream
        dconst_1
        d2l
        invokevirtual java.io.PrintStream.println(long):void

        # f2d
        getstatic java.lang.System.out:java.io.PrintStream
        fconst_0
        f2d
        invokevirtual java.io.PrintStream.println(double):void

        # f2i
        getstatic java.lang.System.out:java.io.PrintStream
        fconst_0
        f2i
        invokevirtual java.io.PrintStream.println(int):void

        # f2l
        getstatic java.lang.System.out:java.io.PrintStream
        fconst_0
        f2l
        invokevirtual java.io.PrintStream.println(long):void

        # i2b
        iconst_5
        i2b
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(int):void

        # i2c
        iconst_5
        i2c
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(int):void

        # i2d
        getstatic java.lang.System.out:java.io.PrintStream
        iconst_5
        i2d
        invokevirtual java.io.PrintStream.println(double):void

        # i2f
        iconst_5
        i2f
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(float):void

        # i2l
        getstatic java.lang.System.out:java.io.PrintStream
        iconst_5
        i2l
        invokevirtual java.io.PrintStream.println(long):void

        # i2s
        iconst_5
        i2s
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(int):void

        # l2d
        getstatic java.lang.System.out:java.io.PrintStream
        lconst_1
        l2d
        invokevirtual java.io.PrintStream.println(double):void

        # l2f
        getstatic java.lang.System.out:java.io.PrintStream
        lconst_1
        l2f
        invokevirtual java.io.PrintStream.println(float):void

        # l2i
        getstatic java.lang.System.out:java.io.PrintStream
        lconst_1
        l2i
        invokevirtual java.io.PrintStream.println(int):void

        return
