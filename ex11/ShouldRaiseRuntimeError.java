package ex11;

public class ShouldRaiseRuntimeError {

    public static void main(String[] args) {
        Object obj[] = new String[4];
        // Storing integer in array of strings...!!!
        // results into a Runtime Error
        // (Exception in thread "main" java.lang.ArrayStoreException: java.lang.Integer)
        obj[0] = 42;
    }
    
}