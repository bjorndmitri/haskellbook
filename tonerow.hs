import Data.List (nub)
import System.Random
import Data.Maybe (fromMaybe)

data TwelveToneRow = Row [Int] deriving (Eq, Show)

isPermutation :: [Int] -> Bool
isPermutation xs = xs == nub xs

hasTwelveElements :: [Int] -> Bool
hasTwelveElements xs = length xs == 12

hasCorrectRange :: [Int] -> Bool
hasCorrectRange xs = (minimum xs, maximum xs) == (1, 12) 

satisfies :: (a -> Bool) -> a -> Maybe a
satisfies f x = if f x then Just x else Nothing

mkTwelveToneRow :: [Int] -> Maybe TwelveToneRow
mkTwelveToneRow row = do
    x <- satisfies isPermutation row
    y <- satisfies hasTwelveElements x
    z <- satisfies hasCorrectRange y
    return $ Row z

randomPermutation :: RandomGen g => [Int] -> g -> [Int]
randomPermutation [] _ = []
randomPermutation [x] _ = [x]
randomPermutation x seed = 
    let (y, seed') = randomR (0, (length x) - 1) seed
        z = x !! y
    in z:(randomPermutation (filter (/=z) x) seed')

mkRandomToneRow :: RandomGen g => g -> TwelveToneRow
mkRandomToneRow seed = fromMaybe (Row [1..12]) 
                     $ mkTwelveToneRow 
                     $ randomPermutation [1..12] seed

class ToneRow a where
    identity :: a
    transpose :: a -> Int -> a
    retrograde :: a -> a
    invert :: a -> Int -> a

instance ToneRow TwelveToneRow where
    identity = Row [1..12]
    transpose (Row xs) amount = Row $ fmap (\x -> (x `mod` 12) + (amount `mod` 12)) xs
    retrograde (Row xs) = Row $ reverse xs
    invert (Row xs) axis = Row $ fmap (\x -> (axis - x) `mod` 12 + 1) xs 
