{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

data Reader r a = Reader { runReader :: r -> a }
instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra
newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
                humanName :: HumanName
              , dogName :: DogName
              , address :: Address
              } deriving (Eq, Show)

data Dog =
    Dog {
        dogsName :: DogName
    ,   dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = 
    Person (HumanName "Sam Walls")
           (DogName "Doggo")
           (Address "Paturoa Rd")

pers2 :: Person
pers2 =
    Person (HumanName "Wam Salls")
           (DogName "Pupper")
           (Address "Richardson Rd")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 m n p = m <$> n <*> p

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (\_ -> a)

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb =
        Reader $ \r -> (runReader $ aRb (ra r)) r

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    add <- address
    return $ Dog name add
