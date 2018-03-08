{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Char
import Data.List hiding (isSubsequenceOf)
import Test.QuickCheck

checkVigenere :: IO ()
checkVigenere = do 
    quickCheck (\str keyword -> unvigenere (vigenere str keyword) keyword == str)

newtype Vigenere = Vigenere String
mirrorChar :: Char -> Char
mirrorChar char
    | (toLower char) `notElem` ['a'..'z'] = char
    | otherwise = ['a'..'z'] !! n
                    where n = (negate (ord (toLower char) - ord 'a')) `mod` 26

caesarChar :: Char -> Char -> Char
caesarChar _ ' ' = ' '
caesarChar char' char = chr $ (f char) + ((ord char - (f char) + n) `mod` 26)
  where 
    n = ord char' - f char'
    f x = if isUpper x then ord 'A' else ord 'a'

-- whoa, can't believe this fucking worked~ 
vigenerifyString :: String -> String -> String
vigenerifyString plaintext keyword = 
  reverse 
  $ fst 
  $ foldr 
    (\x (y, l@(z:zs)) 
      -> if x == ' ' then (x:y, l) else (z:y, zs)) 
    ("", cycle keyword)  plaintext 

vigenere :: String -> String -> String
vigenere plaintext keyword = zipWith caesarChar cipher plaintext 
  where 
    cipher = vigenerifyString plaintext keyword

unvigenere :: String -> String -> String
unvigenere ciphertext keyword = vigenere ciphertext (map mirrorChar keyword)

vignereIO :: IO ()
vignereIO = do
  plaintext <- getLine
  keyword <- getLine
  putStrLn $ vigenere plaintext keyword

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf z@(x:xs) (y:ys) 
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf z ys

capitaliseWords :: String -> [(String, String)]
capitaliseWords str = map (\x@(z:zs) -> (x, (toUpper z):zs)) $ words str

capitaliseWord :: String -> String
capitaliseWord "" = ""
capitaliseWord (x:xs) = (toUpper x):xs

capitaliseParagraph :: String -> String
capitaliseParagraph str = foldr (\x y -> x ++ " "  ++ y) "" 
  $ map (\x@(z:zs) -> (toUpper z):zs) $ words str

data DaPhone = DaPhone [(Char, String)] 
phone = DaPhone [('1', ""), ('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"), 
         ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"), 
         ('*', "^"), ('0', "+ "), ('#', ".,")]

convo :: [String]
convo = ["Wanna play 20 questions", 
         "Ya", 
         "U 1st haha", 
         "Lol ok. Have u ever tasted alcohol lol", 
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps ph@(DaPhone phone) char = 
    if isUpper char then ('*', 1):(reverseTaps ph $ toLower char) 
      else
        [foldr (\(x1, x2) y -> if char `elem` x2 then (x1, index char x2) else y) (('1', 0) :: (Digit, Presses)) phone]        
        where 
          index x z = 1 + (length $ takeWhile (/= x) z)

reverseTapsString :: DaPhone -> String -> [(Digit, Presses)]
reverseTapsString ph str = concatMap (reverseTaps ph) str

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = sum $ map snd x

mostPopularLetter :: String -> Char
mostPopularLetter str = 
  fst $ foldr (\x w@(y, count) -> if z x >= count then (x, z x) else w) (head str, 0) str
  where 
    z x = length $ filter (\y -> (toLower y)==(toLower x)) str

tapsOfMostPopularLetter :: String -> Int
tapsOfMostPopularLetter str = 
  let char = mostPopularLetter str in 
    fingerTaps (reverseTapsString phone $ filter (\x -> (toLower x)==(toLower char)) str)

coolestLtr :: [String] -> Char
coolestLtr strs = mostPopularLetter $ concatMap (\x -> (mostPopularLetter x):"") strs

coolestWord :: [String] -> String
coolestWord x = mode $ concatMap words x

counts :: (Eq a, Num b) => [a] -> [(a, b)]
counts [] = []
counts z@(x:xs) = 
  let init = map (\x -> (x, 0)) $ nub z
      increment char lst = foldr 
        (\(x1, x2) y -> 
          if x1 == char 
            then (x1, x2 + 1):y
            else (x1, x2):y)
        [] lst
  in
    foldr increment init z

mode :: (Eq a) => [a] -> a
mode xs = fst $ foldr (\x@(x1, x2) y@(y1, y2) -> if x2 >= y2 then x else y) h t
  where (h:t) = counts xs

data Expr = Lit Integer | Add Expr Expr | Mult Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mult x y) = (eval x) * (eval y)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = concat [printExpr x, " + ", printExpr y]
printExpr (Mult x y) = concat ["(", printExpr x, " * ", printExpr y, ")"]

main :: IO ()
main = undefined
