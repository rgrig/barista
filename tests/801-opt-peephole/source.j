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

.method public static void optimize_constants()
        ldc2_w 0.0
        ldc2_w 1.0
        dadd
        pop2
        ldc_w 0.0
        ldc_w 1.0
        fadd
        pop
        ldc 0.0
        ldc 1.0
        fadd
        pop
        ldc 0
        ldc 1
        ldc 2
        ldc 3
        ldc 4
        ldc 5
        ldc -1
        ldc 55
        ldc 555
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        pop
        ldc_w 0
        ldc_w 1
        ldc_w 2
        ldc_w 3
        ldc_w 4
        ldc_w 5
        ldc_w -1
        ldc_w 55
        ldc_w 555
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        iadd
        pop
        ldc2_w 0
        ldc2_w 1
        ladd
        pop2
        return

.method public static void optimize_locals(double,float,int,long)
        dload 0
        d2i
        wide fload 2
        f2i
        iload 3
        wide lload 4
        l2i
        iadd
        iadd
        iadd
        return

.method public static void nop()
        nop
        nop
        nop
        goto dest1:
        nop
        nop
dest1:
        nop
        goto dest2:
        nop
dest2:
        nop
        return

.method public static void unused()
        iconst_0
        iconst_0
        swap
        swap
        pop
        pop
        ldc 6
        pop
        ldc2_w 3.14
        pop2
        return

.method public static void load_store(float, int)
        fload 0
        wide fstore 0
        iload 1
        wide istore 1
        return

.method public static void store_store(float, int)
        fconst_0
        dup
        fstore 0
        wide fstore 0
        iconst_0
        dup
        istore 1
        wide istore 1
        return

.method public static void store_load(float, int)
        fconst_1
        wide fstore 0
        fload 0
        iconst_3
        wide istore 1
        iload 1
        return

.method public static void constant_on_the_top(float, int)
        ldc 5
        wide iload 1
        iadd
        ldc 3.14
        wide fload 0
        fadd
        return

.method public static void remove_neutral_elements(float, int)
        fload 0
        ldc 0.0
        fadd
        fconst_1
        fmul
        wide iload 1
        bipush 0
        iadd
        sipush 1
        imul
        return

.method public static void absorbing_elements(int)
        iload 0
        ldc 0
        imul
        iload 0
        ldc 1
        irem
        return

.method public static void apply_stength_reduction(int)
        iload 0
        ldc 8
        imul
        return

.method public static void remove_identity(int)
        wide iload 0
        ineg
        ineg
        ineg
        i2c
        i2c     
        i2s
        return

.method public static void optimize_iinc(int)
        wide iload 0
        ldc 3
        isub
        istore_0        
        wide iload 0
        ldc 2
        iadd
        istore_0
        return

.method public static void main(java.lang.String[])
        return
