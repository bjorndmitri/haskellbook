{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshalling where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import Control.Applicative
import Data.Scientific (floatingOrInteger)

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"}, 
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = 
    TestData {
        section :: Host
    ,   what    :: Color
    } deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color = 
    Red Annotation 
  | Blue Annotation 
  | Yellow Annotation 
  deriving (Eq, Show)

main = do 
    let d = decode sectionJson :: Maybe TestData
    print d
    print $ dec "blah"
    print $ eitherDec "blah"

instance FromJSON TestData where
    parseJSON (Object v) = 
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ = fail "Expected object for TestData"

instance FromJSON Host where
    parseJSON (Object v) = 
        Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) = 
            (Red <$> v .: "red")
        <|> (Blue <$> v .: "blue")
        <|> (Yellow <$> v .: "yellow")
    parseJSON _ = fail "Expected an object for Color"

data NumberOrString = Numb Integer | Str Text deriving (Eq, Show)

instance FromJSON NumberOrString where
    parseJSON (Number i) = case floatingOrInteger i of
        (Left _) -> fail "must be integral"
        (Right integer) -> return $ Numb integer
    parseJSON (String s) = return $ Str s
    parseJSON _ = fail "must be number or string"

dec :: ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString -> Either String NumberOrString
eitherDec = eitherDecode

