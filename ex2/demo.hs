import Control.Exception (evaluate)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)


leftish :: (Ord t, Num t) => t -> Tree t
leftish n
    | n == 0 = Node 0 []
    | n > 0 =  Node n [lc, rc]
        where
            lc = leftish (n-1)
            rc = Node n []


eftish :: (Ord t, Num t) => t -> Tree t
eftish n
    | n == 0 = Node 8 []
    | n > 0 =  Node n [lc, rc]
        where
            lc = eftish (n-1)
            rc = Node n []


mirror :: Tree a -> Tree a
mirror (Node n kids) = Node n revkids
  where
    revkids = reverse $ map mirror kids


fringe_naive :: Tree a -> [a]
fringe_naive (Node n kids)
  | null kids = [n]
  | otherwise = concat $ map fringe_naive kids


fringe t = leaves t []
  where
    leaves (Node x []) acc = x:acc
    leaves (Node _ kids) acc = helper kids acc
      where
        helper [] acc = acc
        helper (x:xs) acc = leaves x $ helper xs acc


same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe tr1 tr2 = ls1 == ls2
  where
    ls1 = fringe tr1
    ls2 = fringe tr2


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

try1 x = print $ fibtree_naive x
try2 x = print $ mirror $ fibtree x
main = try2 20