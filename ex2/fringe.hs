{-
Σταματελόπουλος Νικόλαος
Α.Μ. : 03116138
-}


import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)

-- Add your solutions here!

--(A)
leftish :: Int -> Tree Int
leftish n
    | n == 0 = Node 0 []
    | n > 0 =  Node n [lc, rc]
        where
            lc = leftish (n-1)
            rc = Node n []


--(B)
mirror :: Tree a -> Tree a
mirror (Node n kids) = Node n revkids
  where
    revkids = reverse $ map mirror kids


{-(C)
Η πολυπλοκότητα της συνάρτησης φαίνεται να είναι
εκθετική ως προς τον αριθμό των φύλλων του δέντρου (O(n^2))
Το παραπάνω επιβεβαιώνεται με εκτέλεση της σε leftish trees
Ποιο συγκεκριμένα για τα leftish trees η χρήση της concat επιφέρει
T(α) πολυπλοκότητα για συνένωση δύο λιστών μήκους α και β. Αφού λοιπόν
σε κάθε επίπεδο του δέντρου, υποδέντρο με ν φύλλα, γίνεται και ένα concat
μεταξύ λιστών με ν-1 φύλλα του αριστερού υποδέντρου και 1 του δεξιού
(λόγω δομής του leftish tree) έχουμε πολυπλοκότητα T(n) = T(n-1) + (n-1)
που εύκολα επαληθεύει και την πειραματική παρατήρηση καταλήγοντας σε T(n^2)
-}
fringe_naive :: Tree a -> [a]
fringe_naive (Node n kids)
  | null kids = [n]
  | otherwise = concat $ map fringe_naive kids


{-(D)
Η συνάρτηση εκτελεί απλή dfs διάσχυση του δένδρου με χρήση accumulator
για αποθήκευση των φύλλων (T(1) χρόνος εισαγωγής ενός φύλλου στη λίστα).
Άρα πρόκειται για T(n) πολυπλοκότητα
-}
fringe :: Tree a -> [a]
fringe t = leaves t []
  where
    leaves (Node x []) acc = x:acc
    leaves (Node _ kids) acc = helper kids acc
      where
        helper [] acc = acc
        helper (x:xs) acc = leaves x $ helper xs acc


{-(E)
Η συνάρτηση λόγω του lazy evaluation της haskell αναμένουμε ότι όλες οι τιμές και
οι τιμές θα υπολογίζονται τη στιγμή που χρειάζονται για κάποιο υπολογισμό. Στη
συγκεκριμένη περίπτωση έχουμε τον έλεγχο των περιγραμμάτων οπότε αναμένουμε αυτά
να υπολογίζονται σταδιακά καθώς προσπαθούμε να ελέγξουμε τα στοιχεία των μεταβλητών
ls1 και ls2 αν είναι ίσα. Έτσι η haskell θα κάνει ακριβώς όση δουλειά απαιτείται,
υπολογίζοντας τα ώσπου να βρεθεί ένα στοιχείο που να διαφέρει μεταξύ τους χωρίς να
συνεχίσει μέχρι και το τελευταίο φύλλο του δέντρου σε αυτήν την περίπτωση, ή στην
περίπτωση ισότητας συνεχίζει ώσπου να τα ελέγξει όλα επιστρέφοντας True.
Αυτό προφανώς είναι O(min(n1,n2)) αν τα δύο δέντρα έχουν n1 και n2 φύλλα αντίστοιχα.
-}
same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe tr1 tr2 = ls1 == ls2
  where
    ls1 = fringe tr1
    ls2 = fringe tr2


{-(F)
Στην περίπτωση που επρόκειτο για μια eager γλώσσα το παραπάνω πρόβλημα θα λυνόταν σε
T(n1+n2) καθώς θα έπρεπε να προηγηθεί η εύρεση ολόκληρου του περιγράμματος και των
δύο δέντρων πρωτού αρχίσει η σύγκριση αυτών
(Επιπλέον θα προτιμόταν η χρήση μιας tail recursive fringe με accumulator για partial result passing)

Αντίστοιχης απόδοσης υλοποίηση σε ML προφανώς είναι αρκετά πιο δύσκολη καθώς ο έλεγχος
θα πρέπει να γίνεται παράλληλα της διάσχυσης των δύο δέντρων.
Ακολουθεί υλοποίηση σε SML-NJ. Επιπλέον παρατ'ιθενται οι τύποι δεδομένων που χρησιμοποιούνται
καθώς και μία υλοποίηση της leftish για εύκολη επαλήθευση της λειτουργίας.

Η nextLeaves ξεκινά από μια λίστα με δέντρα εξάγοντας ένα από αυτή. Στη συνέχεια με αριστερότερη κατάβαση
σε αυτό αναζητεί για το αριστερότερο φύλλο ενώ προσθέτει στη λίστα κάθε υποδέντρο/παιδί των κόμβων που συναντά
αλλά δεν επισκέπτεται (άρα όλα τα παιδιά κάθε κόμβου πλην του αριστερού).
Οπότε διατηρεί ένα state για το ποιον κόμβο θα επισκεφθεί και με ποια σειρά (από αριστερά προς το δεξιά),
ενώ επιστρέφει όταν βρει φύλλο. Προφανώς αυτή θα πρέπει να τρέξει και για τα δύο δέντρα κάθε φορά.
Η aux που καλεί την nextLeaves ελέγχει αν τα φύλλα με τη σειρά που τα επιστρέφει η συνάρτηση των δύο δέντρων
έχουν ίδια labels, διανύοντας και συγκρίνοντας τελικά τα δύο περίγραμματα σταδιακά φτάνοντας μέχρι την πρώτη
διαφορά τους, ή μέχρι να διανύσει ένα από τα δύο (και τα δύο αν είναι ίδια).
-}
{-
datatype 'a Tree = Node of 'a * 'a Tree list;
datatype 'a option = None | Some of 'a * 'a Tree list;


fun leftish 0 = Node (0, nil)
|   leftish n = 
    let
        val rc = Node (n, nil)
        val lc = leftish (n-1)
    in
      Node (n, [lc, rc])
    end

fun same_fringe t1 t2 =
    let
      fun aux l1 l2 =
        let
            fun nextLeaves nil = None
            |   nextLeaves (x::xs) = case x of
                  Node(w, nil)  => Some (w, xs)
                | Node(_, kids)  => nextLeaves (kids @ xs)

            val ls1 = nextLeaves l1
            val ls2 = nextLeaves l2
        in
            case (ls1, ls2) of
              (None, None) => true
            | (None, _) => false
            | (_,  None) => false
            | (Some(x1, k1), Some(x2, k2)) => (x1=x2) andalso aux k1 k2
        end
    in
        aux [t1] [t2]
    end
-}


{-(G)
Το δέντρο που σχηματίζεται από την παρακάτω συνάρτηση καταλαμβάνει
O(2^n) χώρο στη μνήμη.
Εύκολα μπορεί να αποδειχθεί αν σκεφτούμε ότι το Tn είναι ένας
κόμβος/ρίζα με δύο υποδέντρα T(n-1) και T(n-2). Με κάθε αύξηση του
n έχουμε τελικά σχεδόν διπλασιασμό του μεγέθους του δέντρου.
-}
fibtree_naive :: Int -> Tree Int
fibtree_naive n
  | n == 0 = Node 0 []
  | n == 1 = Node 1 []
  | otherwise = Node fib [lc, rc]
      where
        lc = fibtree_naive (n-1)
        rc = fibtree_naive (n-2)
        (Node fibn1 _) = lc
        (Node fibn2 _) = rc
        fib = fibn1 + fibn2


{-(Η)
Aν Tn ένα fibonacci δέντρο που υπολογίζουμε, τότε το δεξί του υποδέντρο T(n-2)
θα έχει την ίδια δομή με το αριστερό υποδέντρο του αριστερού παιδιού του Tn.
Έτσι συσχετίζοντας τα εκμεταλλευόμαστε το graph reduction optimization της
haskell, έτσι το αποτέλεσμα γίνεται shared για τα δύο υποδέντρα αντί να
υπολογίζεται εκ νέου για το δεύτερο, καταλήγοντας να καταλαμβάνει O(n)
χώρο στη μνήμη το δέντρο Tn που δημιουργεί η fibtree.
-}
fibtree :: Int -> Tree Int
fibtree n
  | n == 0 = Node 0 []
  | n == 1 = Node 1 []
  | n == 2 = Node 1 [Node 1 [], Node 0 []]
  | otherwise = Node fib [lc, klc]
      where
        lc = fibtree (n-1)
        (Node fibn1 (klc:_)) = lc
        (Node fibn2 _) = klc
        fib = fibn1 + fibn2


--(I)
{-
Κατόπιν εφαρμογής της mirror στο δέντρο αυτό θα έχει πλέον O(2^n) χώρο
στη μνήμη. Αυτό συμβαίνει διότι η mirror δεν έχει κάποιο optimization
για την εκμετάλλευση του graph reduction, αλλά διασχύζει όλο το δέντρο
που δέχεται ως είσοδο δεσμεύοντας χώρο στη μνήμη για κάθε κόμβο του
τελικού δέντρου που επιστρέφει. Έτσι η έξοδος τελικά της mirror θα
είναι το αντικατοπτρικό fibtree που καταλμβάνει Ο(2^n) χώρο στη μνήμη
αφού δεν έχει σχδιαστεί με τρόπο που να αναγνωρίζει όμοια υποδέντρα
όπως το graph reduction που χρησιμοποιείται στη fibtree αλλά εξετάζει
το κάθε υποδέντρο ξεχωριστά.
-}


t = Node 'a' [ Node 'b' [ Node 'd' [Node 'i' []]
                        , Node 'e' [Node 'j' [], Node 'k' []]
                        ]
             , Node 'c' [ Node 'f' [Node 'l' [], Node 'm' []]
                        , Node 'g' []
                        , Node 'h' [Node 'n' []]
                        ]
             ]

tm = Node 'a' [ Node 'c' [ Node 'h' [ Node 'n' []]
                         , Node 'g' []
                         , Node 'f' [Node 'm' [], Node 'l' []]
                         ]
              , Node 'b' [ Node 'e' [Node 'k' [], Node 'j' []]
                         , Node 'd' [Node 'i' []]
                         ]
              ]

test_correctness msg testcases = do
  putStr $ msg ++ ": " ++ (if and testcases then "OK" else "FAIL!!!") ++ "\n"

test_complexity msg range f = forM_ range $ \n -> do
  putStr $ msg ++ " with size " ++ show n ++ ", "
  timeIt $ evaluate $ f n

main = do
  test_correctness "mirror correctness" $
    [mirror t == tm] ++
    [mirror (mirror t) == t | n <- [0..100], let t = leftish n] ++
    [mirror (mirror t) == t | n <- [0..15], let t = fibtree n]
  test_correctness "fringe_naive correctness" $
    [fringe_naive t == "ijklmgn"] ++
    [fringe_naive (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe_naive (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe_naive leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe_naive . leftish
  test_correctness "fringe correctness" $
    [fringe t == "ijklmgn"] ++
    [fringe (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe . leftish
  test_correctness "same_fringe correctness" $
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]] ++
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]]
  test_complexity "mirror fibtree_naive" [20, 25, 30, 32] $ \n ->
    let t = fibtree_naive n in mirror (mirror t) == t
  test_complexity "mirror fibtree" [20, 25, 30, 32] $ \n ->
    let t = fibtree n in mirror (mirror t) == t
  test_complexity "same_fringe fibtree_naive" [20, 25, 30, 32] $ \n ->
    same_fringe (fibtree_naive n) (fibtree_naive (n+1))
  test_complexity "same_fringe fibtree" [20, 25, 30, 32, 34, 36] $ \n ->
    same_fringe (fibtree n) (fibtree (n+1))
