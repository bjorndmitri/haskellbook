module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Boool = Faalse | Truue deriving (Eq, Show)

instance Arbitrary Boool where
    arbitrary = frequency [ (1, return Faalse), (1, return Truue) ]

instance Monoid Boool where
    mempty = Faalse
    mappend _ _ = Faalse

instance EqProp Boool where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Truue)

