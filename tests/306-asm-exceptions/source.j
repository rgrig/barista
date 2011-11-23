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

.method public static void catch()
try:
        new java.lang.StringBuilder
        dup
        invokespecial java.lang.StringBuilder.<init>():void
        checkcast int[]
        ldc "uncaught"
        goto end:
catch:
        ldc "caught"
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return
        .catch try: catch: catch:

.method private static void catch_npe()
try:
        aconst_null
        invokevirtual java.lang.Object.toString():java.lang.String
        ldc "uncaught"
        goto end:
catch:
catch_npe:
        ldc "caught"
        goto end:
catch_aioobe:
        ldc "wrong caught"
        goto end:
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return
        .catch try: catch: catch_npe: java.lang.NullPointerException
        .catch try: catch: catch_aioobe: java.lang.ArrayIndexOutOfBoundsException

.method private static void throw()
try:
        new java.lang.ClassCastException
        dup
        invokespecial java.lang.ClassCastException.<init>():void
        athrow
        ldc "uncaught"
        goto end:
catch:
        ldc "caught"
end:
        getstatic java.lang.System.out:java.io.PrintStream
        swap
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        return
        .catch try: catch: catch:

.method private static void finally()
try:
        aconst_null
        invokevirtual java.lang.Object.toString():java.lang.String
        jsr sub:
        return
finally:
        jsr sub:
        return
sub:
        astore_1
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "finally"
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        ret 1
        return
        .catch try: finally: finally:

.method private static void finally_w()
try:
        aconst_null
        invokevirtual java.lang.Object.toString():java.lang.String
        jsr_w sub:
        return
finally:
        jsr_w sub:
        return
sub:
        astore_1
        getstatic java.lang.System.out:java.io.PrintStream
        ldc "finally"
        invokevirtual java.io.PrintStream.println(java.lang.String):void
        ret 1
        return
        .catch try: finally: finally:

.method public static void main(java.lang.String[])
        invokestatic pack.Test.catch():void
        invokestatic pack.Test.catch_npe():void
        invokestatic pack.Test.throw():void
        invokestatic pack.Test.finally():void
        invokestatic pack.Test.finally_w():void
        return
