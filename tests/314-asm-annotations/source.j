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

.class public final super pack.Test
.extends java.lang.Object
        @SourceFile "<<Filename>>"
        @Signature "<A:Ljava/lang/Number;>Ljava/lang/Object;"

.field public java.lang.Number x
        @Signature "TA;"

.method public static void main(java.lang.String[])
        @RuntimeVisibleAnnotations java.lang.Deprecated
        @RuntimeVisibleAnnotations pack.MyAnnotation a string "xyz"
        @RuntimeVisibleAnnotations pack.MyAnnotation b float 3.14
        @RuntimeVisibleAnnotations pack.MyAnnotation c 0 int 5
        @RuntimeVisibleAnnotations pack.MyAnnotation c 1 int 7
        @RuntimeVisibleAnnotations pack.MyAnnotation e enum pack.MyAnnotation$E E3
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "hello\n..."
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return
