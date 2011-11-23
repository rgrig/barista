package pack;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface MyAnnotation {
    String a();
    float b();
    int[] c();
    int d() default 0;
    E e();
    public static enum E {E1, E2, E3};
}
