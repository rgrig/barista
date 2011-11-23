package pack;

@Deprecated
public class TestAnnot {
    @Deprecated
    public static void f() {}

    @MyAnnotation(a="xyz", b=3.14f, c=5, e=MyAnnotation.E.E3)
    public static void g() {}
}
