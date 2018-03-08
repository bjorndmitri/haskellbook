myOr :: [Bool] -> Bool
myOr = foldr (||) False

explicitMyOr :: [Bool] -> Bool
explicitMyOr [] = False
explicitMyOr (x:xs) = x || explicitMyOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) = if x == a then True else myElem a xs

anyMyElem :: Eq a => a -> [a] -> Bool
anyMyElem a = any (==a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = go (x:xs) []
  where go [] y = y
        go (y:ys) z = go ys (z ++ y)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f x = squish $ map f x

squishAgain :: [[a]] -> [a]
squishAgain x = squishMap id x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x 
  where go :: (a -> a -> Ordering) -> [a] -> a -> a
        go f [] y = y
        go f (x:xs) y = if f x y == GT then go f xs x else go f xs y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = myMaximumBy g (x:xs)
  where g x y = case f x y of
          EQ -> EQ
          GT -> LT
          LT -> GT
