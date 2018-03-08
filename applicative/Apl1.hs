module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- this is an orphan instance of Monoid, because the Monoid typeclass and the
-- Ziplist type are defined elsewhere. this is bad! bad bad bad, don't doooo it
instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty 
    mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _  <*> Nil = Nil
    (Cons f fs) <*> z = (fmap f z) `append` (fs <*> z)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' 1 (Cons x xs) = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs 

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l
instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' $ pure x
    ZipList' Nil <*> _ = ZipList' Nil
    _ <*> ZipList' Nil = ZipList' Nil
    ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' $ Cons (f x) (fs <*> xs)

data Validation err a = Failure err | Success a deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

data Errors = DividedByZero | StackOverflow | ChewedWires deriving (Eq, Show)

success = Success (+1) <*> Success 1 :: Validation [Errors] Int
failure = Success (+1) <*> Failure [StackOverflow]

failure' = Failure [StackOverflow] <*> Success (+1)

failures = Failure [ChewedWires] <*> Failure [StackOverflow]

instance Functor (Validation e) where
    fmap f (Failure x) = Failure x
    fmap f (Success x) = Success $ f x

instance Monoid e => Applicative (Validation e) where
    pure x = Success x
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> _ = Failure x
    _ <*> Failure x = Failure x
    Success f <*> Success x = Success (f x)
instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

instance (Arbitrary e, Arbitrary a)  => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary 
        elements [Failure x, Success y]

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    Two m f <*> Two n x = Two (m <> n) (f x)

data Three a b c  = Three a b c
instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three m n f <*> Three m' n' x = Three (m <> m') (n <> n') (f x)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)
instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' m f g <*> Three' n x y = Three' (m <> n) (f x) (g y)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    Four m n p f <*> Four m' n' p' x = Four (m <> m') (n <> n') (p <> p') (f x)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)
instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' m n o f <*> Four' m' n' o' x = Four' (m <> m') (n <> n') (o <> o') (f x)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos x y z = liftA3 (,,) x y z

main :: IO ()
main = do
    quickBatch (applicative $ (undefined :: Validation String (Int, Int, Int))) 
