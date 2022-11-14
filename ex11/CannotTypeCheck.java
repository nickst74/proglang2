package ex11;

import java.util.ArrayList;

public class CannotTypeCheck {

    // Also the trivial one
    /*
    public static int foo() {
        if(true) {
            return 42;
        } else {
            // although the code is unreachable it still cannot compile
            return "boo";
        }
    }
    */

    public static void main(String[] args) {
        // Just creating an ArrayList of integers
        ArrayList<Integer> num = new ArrayList<Integer>();
        num.add(42);
        // Casting a list of class T to a list of a Superclass of T cannot type check
        // But there would still be no runtime errors if this was allowed
        // because Number is a superclass of Integer
        ArrayList<Number> ls = num; // **** THIS DOES NOT TYPE CHECK **** //
        
        // on the other hand a work-around is this, which results to a successfull type cast
        //ArrayList<Number> ls = new ArrayList<Number>();
        //for(Integer i : num) {
        //    ls.add((Number) i); // **** THIS WORKS PERFECTLY **** //
        //}
    }
}
