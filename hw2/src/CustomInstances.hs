{-# OPTIONS_GHC -Wno-orphans #-}

module CustomInstances where

import           Prelude hiding (Either (..))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    sequenceA (Identity x) = fmap Identity x


data Either a b = Left a | Right b

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right x) = Right (f x)

instance Applicative (Either a) where
    pure = Right
    (Left x) <*> _ = Left x
    _ <*> (Left x) = Left x
    (Right f) <*> (Right x) = Right (f x)

instance Foldable (Either a) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right x) = f x

instance Traversable (Either a) where
    sequenceA (Left x)  = pure (Left x)
    sequenceA (Right x) = fmap Right x


data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tree where
    pure x = Node x Leaf Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node f fl fr) <*> (Node x xl xr) = Node (f x) (fl <*> xl) (fr <*> xr)

instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
    sequenceA Leaf         = pure Leaf
    sequenceA (Node x l r) = Node <$> x <*> sequenceA l <*> sequenceA r


newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const x) = Const x

instance Monoid a => Applicative (Const a) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x `mappend` y)

instance Monoid a => Foldable (Const a) where
    foldMap _ _ = mempty

instance Monoid a => Traversable (Const a) where
    sequenceA (Const x) = pure (Const x)


data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
    pure = Pair mempty
    (Pair x1 f) <*> (Pair x2 y) = Pair (x1 `mappend` x2) (f y)

instance Foldable (Pair a) where
    foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
    sequenceA (Pair x y) = Pair x <$> y
