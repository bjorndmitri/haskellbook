{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad
import Data.Functor ((<$>))

functorCompose :: (Eq (f c), Functor f) =>
                    f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = 
                (fmap (g . f) x) == (fmap g . fmap f $ x)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++ ) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) (fmap show ioi)
    in fmap (*3) changed

newtype Identity a = Identity a deriving (Eq, Show)
type IdentityTest = Identity Int -> Fun Int Int -> Fun Int Int -> Bool

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = liftM Identity arbitrary 

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)


data Possibly a = Nope | Ya a deriving (Eq, Show)
instance Functor Possibly where
    fmap f Nope = Nope
    fmap f (Ya x) = Ya $ f x

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (First x) = First x
    fmap f (Second y) = Second $ f y

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

data K a b = K a deriving (Eq, Show)
instance Functor (K a) where
    fmap f (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)
instance Functor (Flip K a) where
    fmap f (Flip (K x)) = Flip (K (f x))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst $ f x

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut (fa)) = LiftItOut $ fmap f fa

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)
instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat 
                | OneGoat a 
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)
instance Functor GoatLord where
    fmap f x = case x of 
        NoGoat -> NoGoat
        OneGoat x -> OneGoat (f x)
        MoreGoats x y z -> MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f x = case x of 
        Halt -> Halt
        Print x y -> Print x (f y)
        Read g -> Read (f.g) 

main :: IO ()
main = do 
    quickCheck $ (functorCompose :: IdentityTest)
    

