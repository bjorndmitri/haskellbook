module QCTest where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

half :: Double -> Double
half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a, Arbitrary a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

opAssociative :: (Integer -> Integer -> Integer) 
                -> Integer -> Integer -> Integer -> Bool 
opAssociative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

opCommutative :: (Integer -> Integer -> Integer) 
                -> Integer -> Integer -> Bool
opCommutative op x y = x `op` y == y `op` x

quotRemTest :: Integer -> Integer -> Bool
quotRemTest x y = if y == 0 then True else (quot x y)*y + (rem x y) == x

divModTest :: Integer -> Integer -> Bool
divModTest x y = if y == 0 then True else (div x y)*y + (mod x y) == x
--prop_listSorted :: Property
--prop_listSorted = forAll (arbitrary :: Gen [a] (\xs -> listOrdered $ sort xs)

dollarTest :: (Integer -> Integer) -> Integer -> Bool
dollarTest f x = (f x) == (f $ x)

compositionTest :: (Integer -> Integer) 
                -> (Integer -> Integer) 
                -> Integer -> Bool
compositionTest f g x = ((f . g) x) == (f (g x))

foldrPlusTest :: [Integer] -> [Integer] -> Bool
foldrPlusTest xs ys = (foldr (:) xs ys) == (xs ++ ys)

foldrConcatTest :: [[Integer]] -> Bool
foldrConcatTest xs = (foldr (++) [] xs) == (concat xs)

ltxsTest :: Int -> [Int] -> Bool
ltxsTest n xs = length (take n xs) == n

readShowTest :: (Read a, Eq a, Show a) => a -> Bool
readShowTest x = (read (show x)) == x

square :: Double -> Double
square x = x * x

squareTest :: Double -> Bool
squareTest x = ((square . sqrt) x) == x

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord str = map toUpper str

same :: (Eq a) => [a] -> Bool
same [] = True
same (x:xs) = snd $ foldr (\y (prev, bool) -> (y, bool && (prev == y))) (x, True) xs

capTest :: String -> Bool
capTest x = same [capitalizeWord x, 
                  twice capitalizeWord x, 
                  fourTimes capitalizeWord x]

sortTest :: [Int] -> Bool
sortTest xs = same [sort xs, twice sort xs, fourTimes sort xs]

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

biasedFoolGen :: Gen Fool
biasedFoolGen = elements [Fulse, Fulse, Frue] 

instance Arbitrary Fool where
    arbitrary = biasedFoolGen
main :: IO ()
main = do
    quickCheck (\x -> (halfIdentity x) == x)
    --quickCheck prop_listSorted
    quickCheck (\xs -> listOrdered $ sort (xs :: [Int]))
    quickCheck $ opCommutative (*)
    quickCheck $ opAssociative (*)
    quickCheck $ opCommutative (^)
    quickCheck $ opAssociative (^)
    quickCheck quotRemTest
    quickCheck divModTest
    quickCheck (\xs -> (xs :: [Int]) == (reverse $ reverse xs))
    quickCheck foldrPlusTest
    quickCheck foldrConcatTest
    quickCheck ltxsTest
    quickCheck (readShowTest :: Double -> Bool)
    quickCheck squareTest
    quickCheck capTest
    quickCheck sortTest


