{-# LANGUAGE TupleSections #-}
module Monoids
    ( maybeConcat
    , eitherConcat

    , NonEmpty (..)
    , Identity (..)
    ) where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))

maybeConcat :: Monoid a => [Maybe a] -> a
maybeConcat = mconcat . map (fromMaybe mempty)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = mconcat . map (either (, mempty) (mempty, ))

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
    (<>) = undefined
    sconcat = undefined
    stimes = undefined

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup (Identity a) where
    (<>) = undefined
    sconcat = undefined
    stimes = undefined

instance Monoid (Identity a) where
    mempty = undefined
    mappend = undefined

{-
Усложнённая версия

Дополнительно реализуйте следующие инстансы:

Semigroup и Monoid для строк, объединяемых при помощи '.'.
ghci> Name "root" <> Name "server"
Name "root.server"
Semigroup и Monoid для newtype Endo a = Endo { getEndo :: a -> a }.
Semigroup и Monoid для
newtype Arrow a b = Arrow { getArrow :: a -> b }
instance Semigroup b => Semigroup (Arrow a b)
instance Monoid    b => Monoid    (Arrow a b)

Задание 3
Реализуйте инстансы классов типов Semigroup и Monoid для типа Tree. Объединение двух деревьев должно создавать новое дерево, в котором присутствуют элементы из обоих деревьев.
 -}
