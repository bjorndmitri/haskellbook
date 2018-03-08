module LogFileParser where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.List
import Control.Monad (void)

test :: String
test = "-- wheee a comment\n\
       \ # 2025-02-05\n\
       \ 08:00 Breakfast\n\
       \ 09:00 Sanitizing moisture collector\n\
       \ 11:00 Exercising in high-grav gym\n\
       \ 12:00 Lunch\n\
       \ 13:00 Programming\n\
       \ 17:00 Commuting home in rover\n\
       \ 17:30 R&R\n\
       \ 19:00 Dinner\n\
       \ 21:00 Shower\n\
       \ 21:15 Read\n\
       \ 22:00 Sleep\n\
       \ # 2025-02-07 -- dates not necessarily sequential\n\
       \ 08:00 Breakfast -- should I try skipping bfast?\n\
       \ 09:00 Bumped head, passed out\n\
       \ 13:36 Wake up, headache\n\
       \ 13:37 Go to medbay\n\
       \ 13:40 Patch self up\n\
       \ 13:45 Commute home for rest\n\
       \ 14:15 Read\n\
       \ 21:00 Dinner\n\
       \ 21:15 Read\n\
       \ 22:00 Sleep\n"

data Minute = Minute Integer deriving Eq
data Hour = Hour Integer deriving Eq
data Day = Day Integer deriving Eq
data Month = Month Integer deriving Eq
data Year = Year Integer deriving Eq

type Description = String

data Time = Time Hour Minute deriving Eq

data Date = Date Year Month Day deriving Eq

-- there is obviously a DRYer way to do this, but w/e man
instance Show Minute where
    show (Minute int)
        | int < 10 = "0" ++ show int
        | otherwise = show int

instance Show Hour where
    show (Hour hr)
        | hr < 10 = "0" ++ show hr
        | otherwise = show hr

instance Show Day where
    show (Day day)
        | day < 10 = "0" ++ show day
        | otherwise = show day

instance Show Month where
    show (Month month)
        | month < 10 = "0" ++ show month
        | otherwise = show month

instance Show Year where
-- haskell log files are useful for documenting your time travelling adventures
    show (Year year)
        | year < 10 = "000" ++ show year
        | year < 100 = "00" ++ show year
        | year < 1000 = "0" ++ show year
        | otherwise = show year

instance Show Date where
    show (Date y m d) = (show y) ++ "-" ++ (show m) ++ "-" ++ (show d) 

instance Show Time where
    show (Time hr min) = (show hr) ++ ":" ++ (show min)

-- mkTime :: Int -> Int -> Maybe Time

data LogEntry = LogEntry Time Description deriving Eq

data LogDay = LogDay Date [LogEntry] deriving Eq

instance Show LogEntry where
    show (LogEntry time desc) = (show time) ++ " " ++ desc ++ "\n"

instance Show LogDay where
    show (LogDay date []) = "# " ++ show date
    show (LogDay date xs) = "# " ++ show date ++ foldr (\x y -> y ++ show x ++ "\n") "\n" xs

data Log = Log [LogDay] deriving Eq

instance Show Log where
    show (Log xs) = foldr (\x y -> y ++ show x ++ "\n") "" xs

readInteger :: String -> Integer
readInteger = read

skipComments :: Parser ()
skipComments = do
    string "--" >> manyTill anyChar (try skipNL <|> eof)
    return ()

skipWS :: Parser ()
skipWS = void $ char ' '

skipNL :: Parser ()
skipNL = void $ string "\n"

dateParser :: Parser Date
dateParser = do
    _ <- string "# "
    year <- Year <$> readInteger <$> count 4 digit
    _ <- char '-'
    month <- Month <$> readInteger <$> count 2 digit
    _ <- char '-'
    day <- Day <$> readInteger <$> count 2 digit
    (try skipComments) <|> skipNL
    return $ Date year month day

timeParser :: Parser Time
timeParser = do
    hr <- Hour <$> readInteger <$> count 2 digit
    _ <- char ':'
    min <- Minute <$> readInteger <$> count 2 digit
    return $ Time hr min

logEntryParser :: Parser LogEntry
logEntryParser = do
    time <- timeParser
    _ <- char ' '
    description <- manyTill (anyChar) (try (skipComments) <|> (try skipNL))
    return $ LogEntry time description 

logDayParser :: Parser LogDay
logDayParser = do
     -- many $ (try skipComments) <|> skipNL
    date <- dateParser
    entries <- some logEntryParser
    skipNL 
    return $ LogDay date entries

logParser :: Parser Log
logParser = do
    days <- some logDayParser
    eof
    return $ Log days
