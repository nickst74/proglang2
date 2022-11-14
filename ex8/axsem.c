// Nick Stamatelopoulos
// 03116138

// frama-c version: 22.0 (Titanium)
// Alt-Ergo version: 2.4.0
// frama-c -wp -wp-rte -wp-prover alt-ergo -wp-timeout 30 axsem.c -then -report


// checks for <count> consecutive same numbers starting from index <low>
/*@ predicate sameCons(integer low, integer n, int *x) =
  @   \forall integer i; low <= i < low+n ==> x[low] == x[i];
  @*/

/*@ predicate bestExists(integer high, integer best, int *x) =
  @   \exists integer i; 0 <= i < high && i+best <= high && sameCons(i, best, x);
  @*/

// ensures that there is no better than best
// (for all possible <best>-length sequence, next number is different)
/*@ predicate notBetterThanBest(integer high, integer best, int *x) =
  @   \forall integer i; (0 <= i < high && i+best < high && sameCons(i, best, x)) ==> x[i] != x[i+best];
  @*/

// check that best exists and that there is no better sequence
/*@ predicate exactlyTheBest(integer high, integer best, int *x) =
  @   bestExists(high, best, x) && notBetterThanBest(high, best, x);
  @*/

/*@ requires    1 <= N <= 1000000;
  @ requires    \valid(x + (0 .. N-1));
  @ assigns     \nothing;
  @ ensures     0 < \result <= N;
  @ ensures     exactlyTheBest(N, \result, x);
  @*/
int countSameConsecutive(int N, int x[]) {
    int best = 0, i = 0;
    /*@ loop invariant  0 <= i <= N;
      @ loop invariant  i == 0 <==> best == 0;
      @ loop invariant  i > 0 <==> best > 0;
      @ loop invariant  0 < i < N ==> x[i] != x[i-1];
      @ loop invariant  i > 0 ==> exactlyTheBest(i, best, x);
      @ loop assigns    i, best;
      @ loop variant    N - i;
      @*/
    while (i < N) {
        int j = i+1;
        /*@ loop invariant  0 <= i < j <= N;
          @ loop invariant  sameCons(i, j-i, x);
          @ loop assigns    j;
          @ loop variant N - j;
          @*/
        while (j < N && x[j] == x[i]) ++j;
        if (j-i > best) best = j-i;
        i = j;
    }
    return best;
}