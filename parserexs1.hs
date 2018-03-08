module ParserEx1 where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative

lowers :: String
lowers = ['a'..'z']

uppers :: String
uppers = ['A'..'Z']

nums :: String
nums = ['0'..'9']

hyphen :: String
hyphen = ['-']

alphanum :: String
alphanum = lowers ++ uppers ++ nums ++ hyphen

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

admissibleInteger :: Parser Integer
admissibleInteger = do
    z@(r:rs) <- some digit
    case (r, rs == []) of 
        ('0', False) -> fail "must not contain leading zeros"
        _ -> return $ (read z :: Integer) 

tailParser :: Char -> Parser [NumberOrString]
tailParser ch = char ch >> sepBy1 unit (char '.')
        where unit = choice [NOSS <$> (some $ oneOf $ lowers ++ uppers ++ hyphen) 
                           , NOSI <$> admissibleInteger]

releaseParser :: Parser [NumberOrString]
releaseParser = tailParser '-' 

metadataParser :: Parser [NumberOrString]
metadataParser = tailParser '+'

parseSemVer :: Parser SemVer
parseSemVer = do
    major    <- admissibleInteger
    _        <- char '.'
    minor    <- admissibleInteger
    _        <- char '.'
    patch    <- admissibleInteger
    release  <- option [] releaseParser
    metadata <- option [] metadataParser
    eof
    return $ SemVer major minor patch release metadata 

instance Ord SemVer where
    a <= b = a == b || a < b
    (SemVer maj1 min1 pat1 rel1 met1) < (SemVer maj2 min2 pat2 rel2 met2)
        | maj1 > maj2 = True
        | maj2 > maj1 = False
        | min1 > min2 = True
        | min2 > min1 = False
        | pat1 > pat2 = True
        | pat2 > pat1 = False
        | otherwise = rel1 < rel2
