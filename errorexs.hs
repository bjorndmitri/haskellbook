import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe str = foldr g "" $ map f $ map notThe $ words str
  where 
  f :: Maybe String -> String
  f x = case x of
    Nothing -> "a"
    Just a -> a
  g :: String -> String -> String
  g x y = if y == "" then x else x ++ " " ++ y

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = g $ words x
  where
  g :: [String] -> Integer
  g [] = 0
  g (x:[]) = 0
  g (x:z@(y:ys):xs) = case (x, y `elem` "aeiou") of
    ("the", True) -> 1 + g xs
    _ -> g (z:xs)

countVowels :: Num a => String -> a
countVowels [] = 0
countVowels str = foldr (\x y -> if isVowel x then 1 + y else y) 0 str
  where
  isVowel = \x -> (toLower x) `elem` "aeiou" 

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord x = if 2*(countVowels x) <= length x then Just (Word' x) else Nothing

data Nat = Zero | Succ Nat deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = (natToInteger x) + 1

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x = if x < 0 then Nothing else Just (Succ y)
  where 
  Just y = integerToNat (x - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a b = mayybee a id b 

listToMaybe :: [a] -> Maybe a
listToMaybe x = case x of
  [] -> Nothing
  _ -> Just (head x)

maybeToList :: Maybe a -> [a]
maybeToList x = case x of
  Nothing -> []
  Just a -> [a]

catMaybes :: [Maybe a] -> [a]
catMaybes xs = foldr 
               (\x y -> if isJust x then (fromMaybe undefined x):y else y) [] xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = foldr g (Just []) x
  where 
  g :: Maybe a -> Maybe [a] -> Maybe [a]
  g Nothing _ = Nothing
  g x Nothing = Nothing
  g (Just a) (Just y) = Just (a:y)

lefts' :: [Either a b] -> [a]
lefts' xs = foldr g [] xs
  where 
  g :: Either a b -> [a] -> [a]
  g (Left x) y = x:y
  g _ y = y

rights' :: [Either a b] -> [b]
rights' xs = foldr g [] xs
  where
  g :: Either a b -> [b] -> [b]
  g (Right x) y = x:y
  g _ y = y

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x = case x of
  Right a -> Just (f a)
  Left a -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x = case x of
  Left a -> f a
  Right a -> g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\x -> Nothing) (\y -> Just (f y)) x

myIterate :: (a -> a) -> a -> [a]
myIterate f x = let y = f x in y:(myIterate f y)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (a, b) -> a:(myUnfoldr f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of 
  Nothing -> Leaf
  Just (x1, x2, x3) -> Node (unfold f x1) x2 (unfold f x3)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold g 0
  where
  g :: Integer -> Maybe (Integer, Integer, Integer)
  g m = if m >= n then Nothing else Just (m+1, m, m+1)

