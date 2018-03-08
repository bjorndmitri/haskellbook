module WordNumber where

import Data.Char
import qualified Data.Map as M
import Data.List

digitWords :: Integral a =>  M.Map a String
digitWords = M.fromList [
        (0, "zero")
      , (1, "one")
      , (2, "two")
      , (3, "three")
      , (4, "four")
      , (5, "five")
      , (6, "six")
      , (7, "seven")
      , (8, "eight")
      , (9, "nine")
      ]

digitToWord :: Integral a => a -> Maybe String
digitToWord x = M.lookup x digitWords

digits :: Integral a => a -> [a]
digits x = go x []
    where
        go :: Integral a => a -> [a] -> [a]
        go x y
            | x < 10 = x:y
            | otherwise = 
                let (q, r) = x `divMod` 10 in
                    go q (r:y)
wordNumber :: Integral a => a -> String
wordNumber num = y where
    Just y = fmap (concat.(intersperse "-")) 
           $ sequence
           $ map digitToWord 
           $ digits num 
