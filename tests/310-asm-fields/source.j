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
.implements java.io.Serializable

.field private final int x

.field private final int y

.field private static java.lang.String prefix
        @ConstantValue "prefix: "

.method static void <clinit>()
        ldc "state: "
        putstatic pack.Test.prefix:java.lang.String
        return

.method private void <init>(int,int)
        aload_0
        invokespecial java.lang.Object.<init>():void
        aload_0
        iload_1
        putfield pack.Test.x:int
        aload_0
        iload_2
        putfield pack.Test.y:int
        return

.method private void set(int,int)
        aload_0
        iload_1
        putfield pack.Test.x:int
        aload_0
        iload_2
        putfield pack.Test.y:int
        return

.method public java.lang.String toString()
        new java.lang.StringBuilder
        dup
        invokespecial java.lang.StringBuilder.<init>():void
        getstatic pack.Test.prefix:java.lang.String
        invokevirtual java.lang.StringBuilder.append(java.lang.String):java.lang.StringBuilder
        ldc "x="
        invokevirtual java.lang.StringBuilder.append(java.lang.String):java.lang.StringBuilder
        aload_0
        getfield pack.Test.x:int
        invokevirtual java.lang.StringBuilder.append(int):java.lang.StringBuilder
        ldc ", y="
        invokevirtual java.lang.StringBuilder.append(java.lang.String):java.lang.StringBuilder
        aload_0
        getfield pack.Test.y:int
        invokevirtual java.lang.StringBuilder.append(int):java.lang.StringBuilder
        invokevirtual java.lang.StringBuilder.toString():java.lang.String
        areturn

.method public static void main(java.lang.String[])
        .max_stack 8
        new pack.Test
        dup
        iconst_3
        iconst_5
        invokespecial pack.Test.<init>(int,int):void
        dup
        dup
        dup2
        pop2
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.Object):void
        sipush -3
        bipush 7
        invokevirtual pack.Test.set(int,int):void
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.Object):void
        return
