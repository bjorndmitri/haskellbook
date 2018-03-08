module Main where

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Test.QuickCheck hiding (Success, Failure)

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Only mempty
    mappend x y = case (x, y) of
        (Nada, n) -> n
        (m, Nada) -> m
        (Only m, Only n) -> Only (m `mappend` n)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _  = Trivial

instance Arbitrary Trivial where 
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x

type Assoc a = a -> a -> a -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj x <> BoolConj y = BoolConj (x && y)

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return (BoolConj x)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Snd x <> _ = Snd x
    _ <> Snd x = Snd x
    x <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Fst x, Snd y]

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

--instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--    arbitrary = do
--        f <- arbitrary
--        return $ Combine f

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    x <> y = case (x, y) of
        (Failure a, Failure b) -> Failure (a <> b)
        (Failure a, Success b) -> Success b
        (Success a, _) -> Success a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Failure x, Success y]

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight x) <> (AccumulateRight y) = case (x, y) of 
        (Failure a, _) -> AccumulateRight $ Failure a
        (Success a, Failure b) -> AccumulateRight $ Failure b
        (Success a, Success b) -> AccumulateRight $ Success (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        x <- arbitrary
        return $ AccumulateRight x

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth x) <> (AccumulateBoth y) = case (x, y) of
        (Failure a, Failure b) -> AccumulateBoth $ Failure (a <> b)
        (Success a, Success b) -> AccumulateBoth $ Success (a <> b)
        (Failure a, _) -> AccumulateBoth $ Failure a
        (_, Failure b) -> AccumulateBoth $ Failure b

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        x <- arbitrary
        return $ AccumulateBoth x

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)
    mappend = (<>)

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    (Mem f) `mappend` (Mem g) = Mem (\s -> let (a, s') = f s;
                                               (b, s'') = g s'
                                            in (a `mappend` b, s''))

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: Assoc Trivial)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (semigroupAssoc :: Assoc (Identity [Int]))
    quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
    quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)
    quickCheck (semigroupAssoc :: Assoc (Two String String))
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    quickCheck (semigroupAssoc :: Assoc (Three String String String))
    quickCheck (semigroupAssoc :: Assoc BoolConj)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (semigroupAssoc :: Assoc BoolDisj)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (semigroupAssoc :: Assoc (Or String String))
    --quickCheck (semigroupAssoc :: Assoc (Combine Int Int))
    quickCheck (semigroupAssoc :: Assoc (Validation String Int))
    quickCheck (semigroupAssoc :: Assoc (AccumulateRight Int String))
    quickCheck (semigroupAssoc :: Assoc (AccumulateBoth String String))
