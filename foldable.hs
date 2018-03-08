 -- import Data.Foldable
import Data.Monoid



sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 1 xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (\y -> Any (y == x)) xs 
 
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = foldr comps Nothing xs where
    comps x y = case y of 
        Nothing -> Just x
        Just y' -> min (Just x) y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = foldr comps Nothing xs where
    comps x y = case y of
        Nothing -> Just x
        Just y' -> max (Just x) y

null' :: (Foldable t) => t a -> Bool
null' xs = foldr (\x y -> False) True xs

length' :: (Foldable t) => t a -> Int
length' xs = foldr (\x y -> y + 1) 0 xs

toList' :: (Foldable t) => t a -> [a]
toList' xs = foldr (\x y -> x:y) [] xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap id xs

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x y -> (f x) `mappend` y) mempty xs

data Constant a b = Constant a deriving (Eq, Show)
instance Foldable (Constant a) where
    foldr f x _ = x

data Two a b = Two a b deriving (Eq, Show)
instance Foldable (Two a) where
    foldr f x (Two z y) = f y x

data Three a b c = Three a b c deriving (Eq, Show)
instance Foldable (Three a b) where
    foldr f x (Three z' z'' y) = f y x

filterF' :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' f xs = foldMap (\x -> if f x then pure x else mempty) xs 
