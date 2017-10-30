{-# OPTIONS_GHC -Wno-orphans #-}

module CustomInstances
    ( Identity (..)
    , Either (..)
    , Tree (..)
    , Const (..)
    , Pair (..)
    ) where

import           Data.Foldable (Foldable (..))
import           Prelude       hiding (Either (..), Foldable (..))

{- Functor
    1. fmap id         ≡ id
    2. fmap f . fmap g ≡ fmap (f . g)
-}

{- Applicative
    pure id <*> v = v
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    pure f <*> pure x = pure (f x)
    u <*> pure y = pure ($ y) <*> u
-}

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x
    {- 1. fmap id (Identity x)
     - == Identity (id x)
     - == Identity x
     -
     - 2. fmap f (fmap g (Identity x))
     - == fmap f (Identity (g x))
     - == Identity (f (g x))
     - == Identity ((f . g) x)
     - == fmap (f . g) (Identity x)
     -}

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Foldable Identity where
    foldMap f (Identity x) = f x

    fold (Identity x) = x

    {- fold (Identity x)                           ≡ foldMap id (Identity x)
     - =[def fold]= x
     - =[def id]= id x
     - =[def foldMap] foldMap id (Identity x)
     -}

    {- foldMap f (Identity x)                      ≡ fold (fmap f Identity x)
     - =[def foldMap]= f x
     - =[def fold]= fold (Identity (f x))
     - =[def fmap]= fold (fmap f (Identity x))
     -}

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

    {- pure (.) <*> u <*> v <*> w                  = u <*> (v <*> w)
     - =[def pure]= Right (.) <*> u <*> v <*> w
     -
     - Right (.) <*> Left u <*> v <*> w
     - = ... = Left u
     -
     - Right (.) <*> Right u <*> v <*> w
     - = Right (u .) <*> v <*> w
     -
     - Right (u .) <*> Right v <*> w
     - = Right (u . v) <*>
     -
     - Right (u . v) <*> Right w
     - = Right ((u . v) w)
     - = Right (u (v w))
     - = ... = Right u <*> (Right v <*> Right w)
    -}

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
    pure x = Node x (pure x) (pure x)
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node f fl fr) <*> (Node x xl xr) = Node (f x) (fl <*> xl) (fr <*> xr)

    {- pure id <*> v                               = v
     -
     - pure id <*> Leaf
     - =[def <*>]= Leaf
     -
     - pure id <*> (Node x l r)
     - =[def <*>]= Node (id x) (pure id <*> l) (pure id <*> r)
     - =[def id]= Node x (pure id <*> l) (pure id <*> r)
     - =[ind]= Node x l r
    -}

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

    {- pure f <*> pure x                           = pure (f x)
     - =[def pure]= Const mempty <*> Const mempty
     - =[def <*>]= Const (mappend mempty mempty)
     - =[Monoid 1.]= mempty
     - =[def pure]= pure (f x)
     -}

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

    {- u <*> pure y                                = pure ($ y) <*> u
     - =[def pure]= u <*> Pair mempty y
     - =[def <*>]= Pair (u1 `mappend` mempty) (u2 y)
     - =[Monoid 2.]= Pair mempty (u2 y)
     - =[Monoid 1.]= Pair (mempty `mappend` u1) (u2 y)
     - =[def $]= Pair (mempty `mappend` u1) (($ y) u2)
     - =[def <*>]= Pair mempty ($ y) <*> Pair u1 u2
     - =[def pure]= pure ($ y) <*> u
     -}

instance Foldable (Pair a) where
    foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
    sequenceA (Pair x y) = Pair x <$> y
