import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Ratio ((%))

type DecOrRational = Either Double Rational

decimalParser :: Parser Double
decimalParser = do
    x <- integer
    y <- char '.'
    z <- integer
    return $ (fromInteger x) + (toDec z) 
    where
        toDec :: Integer -> Double
        toDec x
            | x < 0 = negate $ toDec $ negate x
            | x == 0 = 0
            | otherwise = read $ "0." ++ (show x) :: Double 

rationalParser :: Parser Rational
rationalParser = do
    x <- integer
    y <- char '/'
    z <- integer
    case z of
        0 -> fail "division by zero"
        _ -> return (x % z)

doRParser :: Parser DecOrRational
doRParser = (Left <$> try decimalParser) <|> (Right <$> rationalParser)

main :: IO ()
main = do
    print $ parseString doRParser mempty "12.345"
    print $ parseString doRParser mempty "12/34"
    print $ parseString doRParser mempty "124/0"
