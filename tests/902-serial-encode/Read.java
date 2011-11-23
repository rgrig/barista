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

import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.Arrays;

public final class Read {

    private static Object input(final String file) throws Throwable {
        System.out.printf("reading file '%s' ...\n", file);
        final FileInputStream fis = new FileInputStream(file);
        final ObjectInputStream ois = new ObjectInputStream(fis);
        final Object res = ois.readObject();
        ois.close();
        fis.close();
        return res;
    }

    private static String toString(boolean[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (boolean[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(byte[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (byte[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(char[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (char[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(double[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (double[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(float[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (float[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(int[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (int[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(long[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (long[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(short[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (short[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    private static String toString(String[][] x) {
        final StringBuilder sb = new StringBuilder();
        for (String[] y : x) {
            sb.append(Arrays.toString(y));
            sb.append('\n');
        }
        return sb.toString();
    }

    public static void main(final String[] args) throws Throwable {
        System.out.println(Arrays.toString((boolean[]) input("bool_array.ser")));
        System.out.println(Arrays.toString((byte[]) input("byte_array.ser")));
        System.out.println(Arrays.toString((char[]) input("char_array.ser")));
        System.out.println(Arrays.toString((double[]) input("double_array.ser")));
        System.out.println(Arrays.toString((float[]) input("float_array.ser")));
        System.out.println(Arrays.toString((int[]) input("int_array.ser")));
        System.out.println(Arrays.toString((long[]) input("long_array.ser")));
        System.out.println(Arrays.toString((short[]) input("short_array.ser")));
        System.out.println(Arrays.toString((String[]) input("string_array.ser")));
        System.out.print(toString((boolean[][]) input("bool2_array.ser")));
        System.out.print(toString((byte[][]) input("byte2_array.ser")));
        System.out.print(toString((char[][]) input("char2_array.ser")));
        System.out.print(toString((double[][]) input("double2_array.ser")));
        System.out.print(toString((float[][]) input("float2_array.ser")));
        System.out.print(toString((int[][]) input("int2_array.ser")));
        System.out.print(toString((long[][]) input("long2_array.ser")));
        System.out.print(toString((short[][]) input("short2_array.ser")));
        System.out.print(toString((String[][]) input("string2_array.ser")));
        System.out.println((pack.Data) input("data.ser"));
    }

}
