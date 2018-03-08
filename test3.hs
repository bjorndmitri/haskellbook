import Data.Char

removeUppers :: [Char] -> [Char]
removeUppers = filter isLower

removeLowers :: [Char] -> [Char]
removeLowers = filter isUpper

capitaliseFirst :: [Char] -> [Char]
capitaliseFirst (x:xs) = (toUpper x):xs

capitalise :: [Char] -> [Char]
capitalise [] = []
capitalise (x:xs) = (toUpper x):(capitalise xs)

getCapitalisedFirst :: [Char] -> Char
getCapitalisedFirst = toUpper . head


