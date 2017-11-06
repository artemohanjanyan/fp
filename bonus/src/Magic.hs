module Magic where

data T a b f = MkT (a (b f))
    deriving Show

instance (Functor a, Functor b) => Functor (T a b) where
    fmap f (MkT x) = MkT (fmap (fmap f) x)

instance (Applicative a, Applicative b) => Applicative (T a b) where
    pure = MkT . pure . pure
    (MkT x) <*> (MkT y) = MkT $ (<*>) <$> x <*> y

instance (Foldable a, Foldable b) => Foldable (T a b) where
    foldMap toMonoid (MkT x) = foldMap (foldMap toMonoid) x

instance (Traversable a, Traversable b) => Traversable (T a b) where
    sequenceA (MkT x) = MkT <$> traverse sequenceA x
