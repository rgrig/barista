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

.field private static final java.lang.String PREFIX
        @ConstantValue "  - << "

.field private static final java.lang.String SUFFIX
        @ConstantValue " >>"

.method public static void print(java.lang.String)
        getstatic java.lang.System.out:java.io.PrintStream
        dup
        dup
        getstatic pack.Test.PREFIX: java.lang.String
        invokevirtual java.io.PrintStream.print(java.lang.String): void
        aload_0
        invokevirtual java.io.PrintStream.print(java.lang.String) :void
        getstatic pack.Test.SUFFIX :java.lang.String
        invokevirtual java.io.PrintStream.println(java.lang.String) : void
        return

.method public static void main(java.lang.String[])
        nop
        getstatic java.lang.System.out : java.io.PrintStream
        ldc "hello\t... \n\t... \"world\""
        invokevirtual java.io.PrintStream.println(java.lang.String):void

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
        invokestatic pack.Test.print(java.lang.String):void
        iinc 1 1
        goto loop:
end:
        return
