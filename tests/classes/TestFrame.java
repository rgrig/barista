package pack;

public class TestFrame {
    public static int f(boolean b, int i, float f) {
        if (b) {
            return i - (int) f;
        } else {
            return i + (int) f;
        }
    }
}
