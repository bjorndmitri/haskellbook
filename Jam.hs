module Jam where

import Data.List

data Fruit = 
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars = 
  Jam { fruit :: Fruit
      , num :: Int }
  deriving (Eq, Ord, Show)

row1 = Jam Peach 0
row2 = Jam Plum 1
row3 = Jam Apple 2
row4 = Jam Blackberry 3
row5 = Jam Peach 4
row6 = Jam Plum 5
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars x = sum $ map num x

mostRow :: [JamJars] -> JamJars
mostRow = maximum

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind 
  where
  compareKind (Jam k _) (Jam k' _) = compare k k'

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy isSameKind
  where
  isSameKind (Jam k _) (Jam k' _) = k == k'

data OpSys = Linux | BSD | Mac | Win deriving (Eq, Show)
data PL = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OpSys, lang :: PL} deriving (Eq, Show)

allLangs = [Haskell, Agda, Idris, PureScript]
allOpSys = [Linux, BSD, Mac, Win]
allProgrammers = [Programmer {os = x, lang = y} | x <- allOpSys, y <- allLangs]

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "👍" else error "😢"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a:(preorder left ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

foldlTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldlTree f x Leaf = x
foldlTree f x (Node left a right) = foldlTree f y right
  where y = f a (foldlTree f x left)

foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree f x Leaf = x
foldrTree f x (Node left a right) = foldrTree f y left
  where y = f a (foldrTree f x right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldrTree g Leaf bt
  where g x bt' = Node Leaf (f x) bt'
