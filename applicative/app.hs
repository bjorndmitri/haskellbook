import Data.List (elemIndex)
import Data.Monoid

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1,2,3] [4,5,6])

y' :: Maybe Integer 
y' = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y :: Maybe Int
y = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y

xs = [1,2,3]
ys = [4,5,6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

newtype Constant a b = Constant {getConstant :: a}  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant { getConstant = x}) = Constant { getConstant = x}

instance Monoid a => Applicative (Constant a) where
    pure x = Constant (mempty) 
    Constant x <*> Constant y = Constant (x <> y)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen 
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of 
    Nothing -> Nothing
    Just n' -> 
        case mkAddress a of 
            Nothing -> Nothing
            Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a


