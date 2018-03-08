stops = "pbtdkg"
vowels = "aeiou"

stopvowelstops = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]
psvs = filter (\(s, v, s') -> s == 'p') stopvowelstops

seekritFunc x = (sum (map f (words x))) / (f (words x))
  where f :: (Num b) => [a] -> b
        f = fromInteger.toInteger.length

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = foldr (\x y -> f x || y) False x

myElem :: Eq a => a -> [a] -> Bool
myElem el = myAny (== el)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x):y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f x = squish $ myMap f x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (z:zs) = foldl (\x y -> if f x y == GT then x else y) z zs 
