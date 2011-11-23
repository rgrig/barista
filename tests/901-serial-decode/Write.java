/*
 * This file is part of Barista.
 * Copyright (C) 2007-2011 Xavier Clerc.
 *
 * Barista is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Barista is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package pack;

import java.io.FileOutputStream;
import java.io.ObjectOutputStream;

public final class Write {

    private static void output(final String file, final Object value) throws Throwable {
        final FileOutputStream fos = new FileOutputStream(file);
        final ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(value);
        oos.close();
        fos.close();
    }

    public static void main(final String[] args) throws Throwable {
        output("bool_array.ser", new boolean[] { false, true, true, false, true, false });
        output("byte_array.ser", new byte[] { 24, -98, 23, 1, -1, 0 });
        output("char_array.ser", new char[] { 0x0043, 0x0045 });
        output("double_array.ser", new double[] { 0.0, 3.14, -1.0 });
        output("float_array.ser", new float[] { 0.0f, 3.14f, -1.0f });
        output("int_array.ser", new int[] { 24, -98, 23, 1, -1, 0 });
        output("long_array.ser", new long[] { 24L, -98L, 23L, 1L, -1L, 0L });
        output("short_array.ser", new short[] { 24, -98, 23, 1, -1, 0 });
        output("string_array.ser", new String[] { "abc", "def", "zzz", "ghi", "jkl", "zzz" });

        output("bool2_array.ser", new boolean[][] { {false, true, true}, {false, true, false} });
        output("byte2_array.ser", new byte[][] { {24, -98, 23}, {1, -1, 0} });
        output("char2_array.ser", new char[][] { {0x0043}, {0x0045} });
        output("double2_array.ser", new double[][] { {0.0, 3.14}, {-1.0} });
        output("float2_array.ser", new float[][] { {0.0f, 3.14f}, {-1.0f} });
        output("int2_array.ser", new int[][] { {24, -98, 23}, {1, -1, 0} });
        output("long2_array.ser", new long[][] { {24L, -98L, 23L}, {1L, -1L, 0L} });
        output("short2_array.ser", new short[][] { {24, -98, 23}, {1, -1, 0} });
        output("string2_array.ser", new String[][] { {"abc", "def", "zzz"}, {"ghi", "jkl", "zzz"} });

        output("data.ser", new Data(0, "zero"));
    }

}
