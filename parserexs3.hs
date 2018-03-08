module USCanPhoneNumberParser where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative

type NumberingPlanArea = Integer
type Exchange = Integer
type LineNumber = Integer

readInteger :: String -> Integer
readInteger = read

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

zeroHyphenParser :: Parser PhoneNumber
zeroHyphenParser = do
    dgs <- count 10 digit
    let fs = readInteger $ take 3 dgs
        sn = read $ take 3 $ drop 3 dgs
        th = read $ take 4 $ drop 6 dgs
    return $ PhoneNumber fs sn th

oneHyphenParser :: Parser PhoneNumber
oneHyphenParser = do
    fs <- read <$> ((char '(') *> (count 3 digit) <* (char ')') <* (char ' '))
    sn <- read <$> ((count 3 digit) <* (char '-'))
    th <- read <$> (count 4 digit)
    return $ PhoneNumber fs sn th
    

twoHyphenParser :: Parser PhoneNumber
twoHyphenParser = do
    fs <- read <$> ((count 3 digit) <* char '-')
    sn <- read <$> ((count 3 digit) <* char '-')
    th <- read <$> (count 4 digit)
    return $ PhoneNumber fs sn th

threeHyphenParser :: Parser PhoneNumber
threeHyphenParser = digit >> (char '-') >> twoHyphenParser

parsePhone :: Parser PhoneNumber
parsePhone = choice [try zeroHyphenParser, try oneHyphenParser, try twoHyphenParser, threeHyphenParser]
    
