import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import GHC.Base (join)

--Datatype for Tree representation
--Node (id, node_weight, subtree_weight) [...]
data Tree a = Node a [Tree a] deriving Show

--Read input from stdin and return all Nodes in a list
--IO [(id, weight, parent)]
parseInput :: IO (IO [(Int, Int, Int)], Int)
parseInput = do
   n <- readLn
   let
      getMultipleLines n i
         | n <= 0 = return []
         | otherwise = do
            [w, p] <- Prelude.map read . words <$> getLine
            xs <- getMultipleLines (n-1) (i+1)
            return ((i, w, p):xs)
      list = getMultipleLines n 1
   return (list, n)

--Takes [Node] and uses dict to create the tree from given parental relation O(nlogn)
--inspired from:
--https://stackoverflow.com/questions/47757073/grow-a-tree-from-parent-relations-in-haskell
buildForest items =
   head (map mkTree roots)
   where
   -- dict :: Map.Map (Maybe ID) [Item a]
      dict  = Map.fromListWith (++) [ (p, [(i, w, p)]) | (i, w, p) <- items ]
      itemsUnder k = join . maybeToList . Map.lookup k $ dict
      roots = itemsUnder 0
      -- using `dict`, recursively build the tree
      mkTree (i, w, _) = do
         let
            kids = map mkTree $ itemsUnder i
            subtree_sum [] sum = sum
            subtree_sum ((Node(_, _, kid_sum) _):ls) sum = subtree_sum ls (sum+kid_sum)
            sum = subtree_sum kids w
         Node (i, w, sum) kids


--return subtrees with max weight (could be more than one with same weight)
maxSubtrees :: [Tree (Int, Int, Int)] -> ([Tree (Int, Int, Int)], Int)
maxSubtrees ls = helper ls [] 0
   where
      helper [] acc max = (acc, max)
      helper (x:xs) [] _ = helper  xs [x] w
         where
            Node (_, _, w) _ = x
      helper (x:xs) acc max = do
         let
            Node(_, _, w) _= x
         if max < w then helper xs [x] w
         else if max > w then helper xs acc max
         else helper xs (x:acc) max


--implemented 0-weight nodes case
solve_v2 (Node(id, _, w) kids) sum (maxId, maxW) = do
   let
      helper :: [Tree (Int, Int, Int)] -> Int -> (Int, Int) -> (Int, Int) 
      helper [] _ max = max
      helper (x:xs) sum max = helper xs sum max'
         where
            max' = solve_v2 x sum max
   let
      above = sum - w
      (toSearch, below) = maxSubtrees kids
      currentMax = Prelude.max above below
      newMax = if maxW < currentMax || (maxW == currentMax && maxId < id) then (maxId, maxW)
               else (id, currentMax)
   if above > below then newMax
   else helper toSearch sum newMax
----------------------

main = do
   (x, n) <- parseInput
   input <- x
   let 
      tree = buildForest input
      (Node (_, _, sum) _) = tree
      (id, _) = solve_v2 tree sum (n+1, sum)
   print id