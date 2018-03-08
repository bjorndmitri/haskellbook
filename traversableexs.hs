{-# LANGUAGE FlexibleContexts #-}

import Data.Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

type TI = []
-- Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
    foldr f z (Identity x) = f x z

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= (return . Identity)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f x = Constant $ getConstant x

instance Foldable (Constant a) where
    foldr f z x = z

instance Traversable (Constant a) where
    traverse f x = pure $ Constant $ getConstant x

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = arbitrary >>= (return . Constant)

instance (Eq a) => EqProp (Constant a b) where (=-=) = eq

-- Optional (isomorphic to Maybe)
data Optional a = No | Yes a deriving (Eq, Show)

instance Functor Optional where
    fmap f x = case x of
        No -> No
        Yes y -> Yes (f y)

instance Foldable Optional where
    foldr f z x = case x of 
        No -> z
        Yes y -> f y z

instance Traversable Optional where
    traverse f x = case x of
        No -> pure No
        Yes y -> Yes <$> f y

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = arbitrary >>= (return . Yes)

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f xs = case xs of
        Nil -> Nil
        Cons y ys -> Cons (f y) (fmap f ys)

instance Foldable List where
    foldr f x xs = case xs of
        Nil -> x
        Cons y ys -> f y (foldr f x ys)

instance Traversable List where
    sequenceA Nil = pure Nil
    sequenceA (Cons x xs) = liftA2 Cons x (sequenceA xs)

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- S n a
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (f <$> x) (f y)

instance Foldable n => Foldable (S n) where
    foldr f z (S x y) = let k = f y z in foldr f k x

instance Traversable n => Traversable (S n) where
    traverse f (S x y) = liftA2 S (traverse f x) (f y)

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = liftA2 S arbitrary arbitrary 

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq 
   --  (S x y) =-= (S z w) = (x, y) `eq` (z, w) 

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap f xs = case xs of
        Empty -> Empty
        Leaf x -> Leaf (f x)
        Node ys x zs -> Node (fmap f ys) (f x) (fmap f zs)

instance Foldable Tree where
    foldMap f xs = case xs of 
        Empty -> mempty
        Leaf x -> f x
        Node ys x zs -> (foldMap f ys) `mappend` (f x) `mappend` (foldMap f zs)

instance Traversable Tree where
    traverse f xs = case xs of 
        Empty -> pure Empty
        Leaf x -> Leaf <$> (f x)
        Node ys x zs -> Node <$> (traverse f ys) <*> (f x) <*> (traverse f zs)

main = do
    let trigger = undefined :: Identity (String, String, String)
    quickBatch (traversable trigger)
    let t2 = undefined :: Constant Int (String, String, String)
    quickBatch (traversable t2)
    let t3 = undefined :: Optional (String, String, String)
    quickBatch (traversable t3)
    let t4 = undefined :: Three Int Int (String, String, String)
    quickBatch (traversable t4)
    let t5 = undefined :: S Maybe (String, String, String)
    quickBatch (traversable t5)
