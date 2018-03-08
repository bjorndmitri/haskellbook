import Control.Monad

newtype IdentityT m a = IdentityT { runIdentity :: m a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT ma) = IdentityT $ fmap f ma

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT $ pure x
    (IdentityT mab) <*> (IdentityT ma) = IdentityT $ mab <*> ma

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    -- f :: a -> IdentityT m b
    -- runIdentity . f :: a -> m b
    (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentity . f
