extractDigits :: (Integral a) => a -> [a]
extractDigits x
  | x < 0 = extractDigits (-x)
  | x < 10 = [x]
  | otherwise = (extractDigits f) ++ [l]
    where (f, l) = divMod x 10

stringify :: (Integral a) => a -> String
stringify x = case x of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "not valid"

convertToString :: (Integral a) => a -> String
convertToString x = tail $ foldl (\x y -> x ++ "-" ++ y) [] (map stringify (extractDigits x))

apply :: (Integral a) => a -> (b -> b) -> (b -> b)
apply n f
  | n == 0 = id
  | n == 1 = f
  | otherwise = f . apply (n - 1) f

next :: Char -> Char
next ch
  | ch == 'z' = 'a'
  | ch == 'Z' = 'A'
  | otherwise = succ ch

eft :: (Ord a, Enum a) => a -> a -> [a]
eft x y
  | y < x = []
  | y == x = [x]
  | otherwise = [x] ++ eft (succ x) y

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:(take' (n-1) xs)

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n x = (take n x, drop n x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x:(takeWhile' f xs) else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs else xs

splitString :: Char -> [Char] -> [[Char]]
splitString _ [] = []
splitString ch (x:xs) = if x == ch then splitString ch xs else y
  where
    str = x:xs
    y = [takeWhile' (/= ch) str] ++ splitString ch (dropWhile' (/= ch) str)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)




