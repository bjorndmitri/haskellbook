module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "2 times 3 is 6" $ do
            recMult 2 3 `shouldBe` 6
        it "15 times 14 is 210" $ do
            recMult 15 14 `shouldBe` 210
        it "10 times -2 is -20" $ do
            recMult 10 (-2) `shouldBe` (-20)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

recMult :: (Eq a, Ord a, Num a) => a -> a -> a
recMult 1 x = x
recMult x 1 = x
recMult 0 _ = 0
recMult _ 0 = 0
recMult x y 
    | x < 0 = negate $ recMult (negate x) y 
    | y < 0 = negate $ recMult x (negate y)
    | otherwise = x + recMult x (y - 1)
