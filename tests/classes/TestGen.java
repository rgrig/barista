package pack;

public class TestGen< A extends Number, B extends Exception & Comparable<B> > {
    public A field;
    public static < T extends Comparable<? super T> > void f_super(T t) {}
    public static < T extends Comparable<? extends T> > void f_extends(T t) {}
    public static < T extends Comparable<?> > void f_star(T t) {}
}
