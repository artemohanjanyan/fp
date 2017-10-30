{-# LANGUAGE TypeOperators #-}
module Optionals
    ( Expr (..)
    , eval
    , partial
    , total
    , apply
    , applyOrElse
    , withDefault
    , isDefinedAt
    , orElse
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Category    as C
import           Control.Monad       ((>=>))
import           Data.Maybe          (fromMaybe, isJust)

data Expr a
    = Expr a :+: Expr a
    | Expr a :-: Expr a
    | Expr a :*: Expr a
    | Expr a :/: Expr a
    | Expr a :^: Expr a
    | Const a
    deriving (Show, Eq)
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixl 8 :^:

data ArithmeticError
    = DivisionByZero
    | NegativeExponent
    deriving (Show, Eq)

leftIf :: (a -> Bool) -> e -> a -> Either e a
leftIf p e x
    | p x       = Left e
    | otherwise = pure x

eval :: Integral a => Expr a -> Either ArithmeticError a
eval (a :+: b) = (+) <$> eval a <*> eval b
eval (a :-: b) = (-) <$> eval a <*> eval b
eval (a :*: b) = (*) <$> eval a <*> eval b
eval (a :/: b) = div <$> eval a <*> (eval b >>= leftIf (== 0) DivisionByZero)
eval (a :^: b) = (^) <$> eval a <*> (eval b >>= leftIf (< 0) NegativeExponent)
eval (Const x) = pure x


data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial (Just . f)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) x       = f x
apply (Defaulted f def) x = case apply f x of
    Just y  -> Just y
    Nothing -> Just def

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f arg def = fromMaybe def (apply f arg)

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault f@(Partial _) def   = Defaulted f def
withDefault (Defaulted f _) def = withDefault f def

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f arg = isJust $ apply f arg

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g = Partial (\x -> apply f x <|> apply g x)

instance C.Category (~>) where
    id = Partial Just
    f . g = Partial $ apply g >=> apply f

{- Lemma
 - (f >=> g) >=> h === f >=> (g >=> h)
 -
 - f >=> g = \x -> f x >>= g
 -
 - (f >=> g) >=> h
 - =[def (>=>)]= (\x -> f x >>= g) >=> h
 - =[def (>=>)]= \y -> (\x -> f x >>= g) y >>= h
 - =[beta]= \y -> (f y >>= g) >>= h
 -
 - f >=> (g >=> h)
 - =[def (>=>)]= f >=> (\x -> g x >>= h)
 - =[def (>=>)]= \y -> f y >>= (\x -> g x >>= h)
 - =[monad law]= \y -> (f y >>= g) >>= h
 -}

{- LAWS
 - (1) h . (g . f) === (h . g) . f
 - (2.1) id . f === f
 - (2.2) f . id === f
 -
 - (1):
 - h . (g . f)
 - =[def (.)]= Partial (apply (g . f) >=> apply h)
 - =[def (.)]= Partial (apply (Partial (apply f >=> apply g)) >=> apply h)
 - =[def apply]= Partial ((apply f >=> apply g) >=> apply h)
 - =[Lemma]= Partial (apply f >=> (apply g >=> apply h))
 - =[def apply)]= Partial (apply f >=> (apply (Partial (apply g >=> apply h))))
 - =[def (.)]= Partial (apply f >=> (apply (h . g)))
 - =[def (.)]= (h . g) . f
 -
 - (2):
 - id . f
 - =[def id]= Partial Just . f
 - =[def (.)]= Partial (apply f >=> apply (Partial Just))
 - =[def apply]= Partial (apply f >=> Just)
 - =[def pure]= Partial (apply f >=> pure)
 - =[def (>=>)]= Partial (\x -> apply f x >>= pure)
 - =[monad law]= Partial (\x -> apply f x)
 - =[eta equality]= Partial (apply f)
 -
 - f . id
 - =[def id]= f . Partial Just
 - =[def (.)]= Partial (apply (Partial Just) >=> apply f)
 - =[def apply]= Partial (Just >=> apply f)
 - =[def pure]= Partial (pure >=> apply f)
 - =[def (>=>)]= Partial (\x -> pure x >>= apply f)
 - =[monad law]= Partial (\x -> apply f x)
 - =[eta equality]= Partial (apply f)
 -
 - apply (Partial (apply f))
 - =[def apply]= apply f
 -}
