import Control.Monad
import Data.Char
import System.IO

palindrome :: IO ()
palindrome = forever $ do
  line <- do 
    l <- getLine
    return (concat.words $ map toLower l)
  case (line == reverse line) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person

mkPerson name age
 | name /= "" && age > 0 = Right $ Person name age
 | name == "" = Left NameEmpty
 | not (age > 0) = Left AgeTooLow
 | otherwise = Left $ PersonInvalidUnknown $
                      "Name was: " ++ show name ++
                      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  age <- do 
    input <- getLine
    return (read input :: Integer)
  person <- return $ mkPerson name age
  case person of 
    Left error -> putStrLn $ show error
    Right person -> putStrLn 
      $ "Yay! Successfully got a person: " ++ (show person)

