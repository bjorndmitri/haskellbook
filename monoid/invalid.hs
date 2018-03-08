import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        frequency [(1, return Nada), (1, return $ Only x)]
instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        return $ First' { getFirst' = x}

instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada }
    mappend x y = case (x, y) of
        (First' { getFirst' = Nada }, y') -> y'
        (x', First' { getFirst' = Nada }) -> x'
        _ -> x

type First'Mappend = First' (Optional Int) 
                  -> First' (Optional Int)
                  -> First' (Optional Int)
                  -> Bool
main :: IO ()
main = do
    quickCheck (monoidAssoc :: First'Mappend) 
    quickCheck (monoidLeftIdentity :: First' (Optional Int) -> Bool)
    quickCheck (monoidRightIdentity :: First' (Optional Int) -> Bool)

