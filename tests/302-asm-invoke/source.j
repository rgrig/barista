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

        # dynamic (not yet supported)
#        getstatic java.lang.System.out:java.io.PrintStream
#        ldc "dynamic"
#        invokedynamic println(java.lang.String):void

        # interface
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "interface"
        invokeinterface java.lang.Appendable.append(java.lang.CharSequence):java.lang.Appendable 2

        # special
        getstatic java.lang.System.out:java.io.PrintStream
        new java.lang.Integer
        dup
        ldc "012"
        invokespecial java.lang.Integer.<init>(java.lang.String):void
        invokevirtual java.io.PrintStream.println(java.lang.Object):void

        # static
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "123"
        invokestatic java.lang.Integer.parseInt(java.lang.String):int
        invokevirtual java.io.PrintStream.println(int):void

        # virtual (over array type)
        iconst_3
        newarray int
        invokevirtual int[].toString():java.lang.String
        pop

        # virtual (over class)
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "virtual"
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return
