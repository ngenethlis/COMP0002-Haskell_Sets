module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type, or
  wrap a binary search tree.
  Extra marks will be awarded for efficient implementations if appropriate.

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
-- newtype Set a = Set { unSet :: [a] } deriving (Show)

-- Set as a binary tree
data Set a = Empty | Node (Set a) a (Set a) deriving (Show, Read, Ord)

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- Binary Tree Auxilerary Functions

-- Insert and Delete require singleton sets to be inserted into the tree
-- Insert a Set into a Tree
insertTree :: (Ord a) => a -> Set a -> Set a
insertTree s Empty = Node Empty s Empty
insertTree s (Node l n r)
  | s == n = Node l n r -- no duplicates
  | s < n = Node (insertTree s l) n r
  | s > n = Node l n (insertTree s r)
insertTree _ _ = error "Error: Inserting into a Tree"

-- --delete a Set from a Tree
-- deleteTree :: (Ord a) => a -> Set a -> Set a
-- deleteTree s Empty = Empty
-- deleteTree s (Node l n r)
--    | s == n =  deleteRoot(Node l n r)
--    | s < n = Node (deleteTree s l) n r
--    | s > n = Node l n (deleteTree s r)
-- deleteTree _ _ = error "Error: Deleting from a Tree"

-- prob not needed

-- --delete the root of a Tree
-- deleteRoot :: (Ord a) => Set a -> Set a
-- deleteRoot Empty = Empty
-- deleteRoot (Node Empty n Empty) = Empty
-- deleteRoot (Node Empty n r) = r
-- deleteRoot (Node l n Empty) = l
-- deleteRoot (Node l n r) = Node l r Empty

-- element of type a in the Set?
memberTree :: (Ord a) => a -> Set a -> Bool
memberTree s (Node l n r)
  | s == n = True
  | s < n = memberTree s l
  | s > n = memberTree s r
memberTree _ Empty = False
memberTree _ _ = error "Error: Searching a Tree"

-- sorting a list
sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList (x : xs) = sortList [y | y <- xs, y < x] ++ [x] ++ sortList [y | y <- xs, y >= x]

{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
-- basic tree walk
-- follow leftmost path Node with leaf on left
-- then follow rightmost path Node with leaf on right
-- then follow leftmost path Node with leaf on left
-- then follow rightmost path Node with leaf on right
-- etc
toList :: Set a -> [a]
toList Empty = []
toList (Node l n r) = toList l ++ [n] ++ toList r

toList' :: Set (Set a) -> [a]
toList' Empty = []
toList' (Node l n r) = toList' l ++ toList n ++ toList' r

-- fromList [2,1,1,4,5] => {2,1,4,5}
-- root is median of list
-- left is median of left half of list
-- right is median of right half of list
fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList [a] = insertTree a Empty
fromList list =
  let (x : xs) = sortList list
   in insertTree x (fromList xs)

-- need to give it a sorted list

-- list is sorted
fromList' :: [a] -> Set a
fromList' [] = Empty
fromList' [a] = Node Empty a Empty
-- list is sorted
-- fromList' (x:x':xs)
--    | x == x' = fromList' (x':xs)
--    | otherwise = fromList' (x:xs)
fromList' list = Node (fromList' (take (length list `div` 2) list)) (list !! (length list `div` 2)) (fromList' (drop ((length list `div` 2) + 1) list))

{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  -- s1 == s2 = toList s1 == toList s2
  Node l1 n1 r1 == Node l2 n2 r2 = l1 == l2 && n1 == n2 && r1 == r2
  Node _ n1 _ == Empty = False
  Empty == Node _ n2 _ = False
  Empty == Empty = True

-- could use intersection maybe

-- the empty set
empty :: Set a
empty = Empty

-- Set with one element
singleton :: a -> Set a
singleton a = Node Empty a Empty

-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert = insertTree

-- insert list of a into Set
insertList :: (Ord a) => [a] -> Set a -> Set a
insertList xs s = foldr insert s xs

-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union = setfoldr insert

-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = setfoldr (\x acc -> if memberTree x s2 then insert x acc else acc) s1 Empty

-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference Empty _ = Empty
difference s1 Empty = s1
difference s1 s2 = setfoldr (\x acc -> if not (memberTree x s2) then insert x acc else acc) s1 Empty

---                                    x taken from s1 if s2 not in s2 add to result else next x
-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
-- member (Node l n r) (Node (Node l1 n1 r1) x (Node l2 n2 r2)) = toList (Node l n r) `elem` toList s
member a s = intersection s (singleton a) == singleton a

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = setfoldr (\_ acc -> acc + 1) s 0

-- maps binary function onto a Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f s = fromList (map f (toList s))

-- setmap f (Node l n r) = Node (setmap f l) (f n) (setmap f r)
-- setmap _ Empty = Empty
-- above fn is wrong if f changes tree direction

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f (Node l n r) acc = setfoldr f l (f n (setfoldr f r acc))
setfoldr _ Empty acc = acc

-- createSingletonSet :: Set a -> Set (Set a)
-- createSingletonSet (Node Empty n Empty) = singleton (singleton n)
-- createSingletonSet (Node l n r) = undefined
-- createSingletonSet Empty = singleton Empty

-- union':: Set (Set a)-> Set(Set a) -> Set(Set a)
-- union' s1 Empty = singleton s1
-- union' s1 s2 = setfoldr (\x acc-> insert (singleton x) acc) s1 s2

-- unions::[Set a] -> Set( Set a)
-- unions []= singleton Empty
-- unions [s] = singleton s
-- unions (x:xs) = insertList xs (singleton x)

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
-- union of all possible sets made by the elements of s
-- empty
-- for every element of s create a singleton
-- make all possible combinations of singleton elements
-- repeat process until set created is s
-- insert all elements into s?
-- singleton $ fromList [1,2,3] set of sets {1,2,3}

powerSet :: Set a -> Set (Set a)
powerSet Empty = singleton Empty
powerSet (Node Empty n Empty) = singleton (singleton n)
-- work with lists
powerSet s = fromList' $ map fromList' $ powerList $ toList s

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList [a] = [[a]]
powerList l = [] : g l

g :: [a] -> [[a]]
g [] = [[]]
g [a] = [[a]]
-- g (x : xs) = [x : y | y <- g xs] ++ g xs ++ [[x]]
g (x : xs) = [[x]] ++ g xs ++ [x : y | y <- g xs]

setToList :: Set (Set a) -> [[a]]
setToList s = map toList (toList s)

pSettoList :: Set a -> [[a]]
pSettoList s = map toList (toList $ powerSet s)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [a] = [a]
removeDuplicates (x : xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian s1 s2 =
  let l1 = toList s1
   in let l2 = toList s2
       in fromList' $ [(x, y) | x <- l1, y <- l2]

-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
-- partition f s = undefined --(setfoldr (\x acc-> if f x then insert x acc else acc) s Empty, setfoldr (\x acc-> if f x == False then insert x acc else acc) s Empty)
partition f s = (fromList' $ filter f (toList s), fromList' $ filter (not . f) (toList s))

unzip' :: (Set a, Set b) -> ([a], [b])
unzip' (s1, s2) = (toList s1, toList s2)

{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

-}
