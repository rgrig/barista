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

public final class Source {

    public static void main(final String[] args) {
    }

    public static int testIf(final int x) {
        if (x < 3) {
            return 0;
        } else {
            return 1;
        }
    }

    public static int testFor(final int x) {
        int sum = 0;
        for (int i = 1; i < x; i++) {
            sum += i;
        }
        return sum;
    }

    public static int testWhile(final Object[] x) {
        final int len = x.length;
        int i = 0;
        while ((i < len) && (x[i] != null)) {
            i++;
        }
        return i;
    }

    public static int testTry(final String x) {
        try {
            return Integer.parseInt(x);
        } catch (final Throwable t) {
            return 0;
        }
    }

    public static String testSwitch(final int x) {
        switch (x) {
        case 0: return "never";
        case 1: return "once";
        case 2: return "twice";
        default: return "many";
        }
    }

    public static void testConstruction() {
        System.out.println(new Object());
    }

    public static Object testDiff(final boolean x) {
        return (x ? new Object() : new String ("")).toString();
    }

    public static long testLong(final int x) {
        long sum = 0;
        for (int i = 0; i < x; i++) {
            long partial = 0;
            for (int j = 0; j < x; j++) {
                partial += j;
            }
            sum += partial;
        }
        return sum;
    }

    public static double testDouble(final int x) {
        double sum = 0;
        for (int i = 0; i < x; i++) {
            double partial = 0;
            for (int j = 0; j < x; j++) {
                partial += j;
            }
            sum += partial;
        }
        return sum;
    }

    public String testInstance(final int x) {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < x; i++) {
            sb.append(toString());
        }
        return sb.toString();
    }

}
