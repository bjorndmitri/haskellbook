module Test where

class Test a where
  test1 :: a -> a
  test2 :: a -> a -> a

data Testy = A | B | C | D deriving (Eq, Ord, Show)

data Testz = A' | B' | C' | D' deriving (Eq, Ord, Show)

instance Test Testy where
  test1 x 
    | x == A = B
    | x == B = C
    | x == C = D
    | x == D = A
  test2 x y
    | x == y = test1 $ test1 x
    -- | otherwise = test1.test1 y

instance Test Testz where
  test1 x
    | x == A' = B'
    | x == B' = C'
    | x == C' = D'
    | x == D' = A'
  test2 x y = if x == y then test1 x else test1 y

