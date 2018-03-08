module Tests where

import Main 
import Test.QuickCheck
import Control.Monad
import Test.Hspec

admissibleStringGen :: Gen String
admissibleStringGen = 
        listOf1 
      $ elements ['a'..'z']
puzzleGen :: Gen Puzzle
puzzleGen = do
    word <- admissibleStringGen 
    return (Puzzle word ([] :: [Maybe Char]) "")

instance Arbitrary Puzzle where
    arbitrary = puzzleGen

test :: IO ()
test = hspec $ do
    describe "fillInCharacter" $ do
        it "adds correct guess" $ do
            fillInCharacter (Puzzle "sam" [Nothing, Nothing, Nothing] "") 's'
                `shouldBe` Puzzle "sam" [Just 's', Nothing, Nothing] ""
        it "doesn't add incorrect guess" $ do
            fillInCharacter (Puzzle "w" [Just 'w'] "w") 'z'
                `shouldBe` Puzzle "w" [Just 'w'] "wz"
        it "doesn't add previous correct guess" $ do
            fillInCharacter (Puzzle "wa" [Just 'w', Nothing] "w") 'w'
                `shouldBe` Puzzle "wa" [Just 'w', Nothing] "w"
    
