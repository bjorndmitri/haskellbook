import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate list = foldr f [] list
  where 
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f a b  = case a of
      DbDate x -> x:b
      _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber list = foldr f [] list
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f a b = case a of 
      DbNumber x -> x:b
      _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = if y == 0 then 0 else z / y
  where
    f = fromInteger.toInteger
    y = (f.length) x
    z = (fromInteger.sumDb) x
