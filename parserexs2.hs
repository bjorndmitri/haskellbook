module PositiveIntParser where

import Text.Parser.Combinators
import Text.Trifecta
import Control.Applicative

numFromDigit :: Char -> Integer
numFromDigit = read . return

-- assumes xs consists only of nonnegative integers < 10
base10FromList :: [Integer] -> Integer
base10FromList xs = fst $ foldr (\x (y, n) -> (x*(10^n) + y, n+1)) (0, 0) xs 

parseDigit :: Parser Char
parseDigit = do
    raw <- anyChar
    if raw `elem` ['0'..'9'] then return raw else fail "not valid"

base10Integer :: Parser Integer
base10Integer = do
    dgts <- some $ try parseDigit
    return $ base10FromList $ numFromDigit <$> dgts

base10Integer' :: Parser Integer
base10Integer' = do
    op <- option ('+') (char '-')
    rest <- base10Integer
    case op of
        '+' -> return rest
        '-' -> negate <$> return rest

