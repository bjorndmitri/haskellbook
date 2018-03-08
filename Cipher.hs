module Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar n str = map (caesarChar n) str 
  where
    caesarChar :: Int -> Char -> Char
    caesarChar _ ' ' = ' '
    caesarChar n char = chr $ first + ((ord char - first + n) `mod` 26)
      where first = if isUpper char then ord 'A' else ord 'a'

unCaesar :: Int -> [Char] -> [Char]
unCaesar n str = caesar (26 - n) str
