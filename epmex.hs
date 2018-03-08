{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
        Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = \x -> Compose $ pure $ pure x

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ ((pure (\x -> (\y -> x <*> y))) <*> f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    -- foldMap :: (Monoid m) => (a -> m) -> t a -> m
    foldMap am (Compose fga) = foldMap (\ga -> foldMap am ga) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
--    (a -> f b) -> t a -> f (t b)
    traverse t (Compose fga) = Compose <$> traverse (\ga -> traverse t ga) fga

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const' a b = Const' a deriving (Eq, Show)

instance Bifunctor Const' where
    bimap f g (Const' a) = Const' (f a)

data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
    bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
    bimap f g (SemiDrei a) = SemiDrei a

data Quadriceps a b c d  = Quads a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quads a b c d) = Quads a b (f c) (g d)

data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Bifunctor Either' where
    bimap f g x = case x of
        Left' a -> Left' (f a)
        Right' b -> Right' (g b)

