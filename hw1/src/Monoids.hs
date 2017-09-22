{-# LANGUAGE TupleSections #-}
module Monoids
    ( maybeConcat
    , eitherConcat

    , NonEmpty (..)
    , Identity (..)

    , Name (..)
    , Endo (..)
    , Arrow (..)
    ) where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))

maybeConcat :: Monoid a => [Maybe a] -> a
maybeConcat = mconcat . map (fromMaybe mempty)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = mconcat . map (either (, mempty) (mempty, ))


data NonEmpty a = a :| [a]
    deriving Show

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ [y] ++ ys)

newtype Identity a = Identity { runIdentity :: a }
    deriving Show

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)


newtype Name = Name String
    deriving Show

instance Semigroup Name where
    (<>) = mappend

instance Monoid Name where
    mempty = Name ""
    mappend xn@(Name x) yn@(Name y)
        | null x    = yn
        | null y    = xn
        | otherwise = Name (x ++ "." ++ y)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) = mappend

instance Monoid (Endo a) where
    mempty = Endo id
    mappend (Endo x) (Endo y) = Endo (x . y)

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    a <> b = Arrow $ \x -> getArrow a x <> getArrow b x

instance Monoid b => Monoid (Arrow a b) where
    mempty = Arrow $ const mempty
    mappend a b = Arrow $ \x -> getArrow a x `mappend` getArrow b x
