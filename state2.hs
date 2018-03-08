{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> moi f g s
        where 
            moi :: (a -> b) -> (s -> (a, s)) -> s -> (b, s)
            moi f g s = let (y, s') = g s in (f y, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s -> moi f g s
        where 
            moi f g s = let (y, s') = g s in ((fst $ f s) y, s')

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s -> moi f g s
        where
            moi f g s = let (y, s') = f s in (runMoi (g y)) s' 

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \s -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify fn = Moi $ \s -> ((), fn s)
