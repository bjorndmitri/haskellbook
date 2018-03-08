module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

one' = one >> stop

oneTwo = char '1' >> char '2' >> eof

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> String -> IO ()
testParse p test  = print $ parseString p mempty test

pNL s = putStrLn ('\n' : s)

-- ones = string "1" 

ones' = char '1'

main = do 
   testParse ones' "123"
   testParse ones' "12"
   testParse ones' "1"
