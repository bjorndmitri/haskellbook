import Control.Monad
import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = Nope

instance Functor Nope where
    fmap f Nope = Nope

instance Applicative Nope where
    pure x = Nope
    Nope <*> Nope = Nope

instance Monad Nope where
    return = pure
    Nope >>= f = Nope

data FlipEither b a = L a | R b deriving (Eq, Show)

instance Functor (FlipEither b) where
    fmap f x = case x of 
        L y -> L $ f y
        R y -> R y

instance Monoid b =>  Applicative (FlipEither b) where
    pure = L
    f <*> x = case (f, x) of
        (L g, L y) -> L (g y)
        (L g, R y) -> R y
        (R g, R y) -> R (g <> y)
        (R g, L y) -> R g

instance Monoid b => Monad (FlipEither b) where
    return = pure
    x >>= f = case x of
        R y -> R y
        L y -> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (FlipEither a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [L x, R y]

instance (Eq a, Eq b) => EqProp (FlipEither a b) where (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= return

instance Eq a => EqProp (Identity a) where (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    --(Cons f fs) <*> z = (fmap f z) `append` (fs <*> z) where
     --   xs `append` ys = case xs of
      --      Nil -> ys
       --     Cons w ws -> Cons w (ws `append` ys)
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
instance Monad List where
    return x = Cons x Nil
    x >>= f = case x of
        Nil -> Nil
        Cons y ys -> (f y) `append` (ys >>= f) where
            xs `append` ys = case xs of
                Nil -> ys
                Cons z zs -> Cons z (zs `append` ys)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary :: Arbitrary a => Gen [a]
        return $ foldr (\z y -> Cons z y) Nil x

instance Eq a => EqProp (List a) where (=-=) = eq

j :: Monad m => m (m a) -> m a
j x = x >>= id 

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= (return.f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = do
    x' <- x
    y' <- y
    return (f x' y')

a :: Monad m => m a -> m (a -> b) -> m b
a x f = do
    x' <- x
    f' <- f
    return $ f' x'

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
    y <- meh xs f
    z <- f x
    return (z:y)

flipType :: (Monad m) => [m a] -> m [a]
flipType m = meh m id

main = do
    let test = undefined :: FlipEither String (Int, String, Int)
    quickBatch $ functor test
    quickBatch $ applicative test
    quickBatch $ monad test
    let test2 = undefined :: Identity (Int, String, Int)
    quickBatch $ functor test
    quickBatch $ applicative test
    quickBatch $ monad test
    let test3 = undefined :: List (Int, String, Int)
    quickBatch $ functor test3
    quickBatch $ applicative test3
    quickBatch $ monad test3
