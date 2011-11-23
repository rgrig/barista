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
        iconst_0
        wide istore 1
        aload_0
        arraylength
        wide istore 2
loop:
        wide iload 1
        wide iload 2
        if_icmpeq end:
        wide aload 0
        wide iload 1
        aaload
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        wide iinc 1 1
        goto loop:
end:
        return
