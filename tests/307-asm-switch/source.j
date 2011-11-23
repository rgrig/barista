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

.method public static void table()
        iconst_0
        istore_1
loop:
        iload_1
        dup
        ldc 5
        if_icmpeq end:
        tableswitch default: 0 2
                => zero:
                => one:
                => two:
zero:
        ldc "zero"
        goto_w print:
one:
        ldc "once"
        goto_w print:
two:
        ldc "twice"
        goto_w print:
default:
        ldc "many"
        goto_w print:
print:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto_w loop:
end:
        return

.method public static void table1()
        iconst_0
        istore_1
loop:
        iload_1
        dup
        ldc 5
        if_icmpeq end:
        nop
        tableswitch default: 0 2
                => zero:
                => one:
                => two:
zero:
        ldc "zero"
        goto_w print:
one:
        ldc "once"
        goto_w print:
two:
        ldc "twice"
        goto_w print:
default:
        ldc "many"
        goto_w print:
print:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto_w loop:
end:
        return

.method public static void table2()
        iconst_0
        istore_1
loop:
        iload_1
        dup
        ldc 5
        if_icmpeq end:
        nop
        nop
        tableswitch default: 0 2
                => zero:
                => one:
                => two:
zero:
        ldc "zero"
        goto_w print:
one:
        ldc "once"
        goto_w print:
two:
        ldc "twice"
        goto_w print:
default:
        ldc "many"
        goto_w print:
print:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto_w loop:
end:
        return

.method public static void table3()
        iconst_0
        istore_1
loop:
        iload_1
        dup
        ldc 5
        if_icmpeq end:
        nop
        nop
        nop
        tableswitch default: 0 2
                => zero:
                => one:
                => two:
zero:
        ldc "zero"
        goto_w print:
one:
        ldc "once"
        goto_w print:
two:
        ldc "twice"
        goto_w print:
default:
        ldc "many"
        goto_w print:
print:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto_w loop:
end:
        return

.method public static void lookup()
        iconst_0
        istore_1
loop:
        iload_1
        dup
        ldc 5
        if_icmpeq end:
        lookupswitch default: 3
                0 => zero:
                1 => one:
                2 => two:
zero:
        ldc "zero"
        goto print:
one:
        ldc "once"
        goto print:
two:
        ldc "twice"
        goto print:
default:
        ldc "many"
        goto print:
print:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        iinc 1 1
        goto loop:
end:
        return

.method public static void main(java.lang.String[])
        invokestatic pack.Test.table():void
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "---"
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        invokestatic pack.Test.lookup():void
        return
