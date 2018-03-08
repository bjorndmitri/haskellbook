module EitherMonad where

type Founded = Int

type Coders = Int

data SoftwareShop = 
    Shop {
        founded         :: Founded
      , programmers     :: Coders
    } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
    | n < 0   = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0    = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f x = case x of 
        First y -> First y
        Second y -> Second $ f y

instance Monoid a => Applicative (Sum a) where
    pure x = Second x
    f <*> x = case (f, x) of
        (_, First z) -> First z
        (First y, Second z) -> First mempty
        (Second y, Second z) -> Second $ y z

instance Monoid a => Monad (Sum a) where
    return = pure
    x >>= f = case x of
        First x -> First x
        Second x -> f x


