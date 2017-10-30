{-# LANGUAGE NoImplicitPrelude #-}

module Monads.Definitions
    ( Monad (..)
    , MonadFish (..)
    , MonadJoin (..)
    ) where


{- LAWS
    1. fmap id         ≡ id
    2. fmap f . fmap g ≡ fmap (f . g)
-}

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {- LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {- LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {- LAWS
        1. join . returnJoin      ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
        4* join . fmap (fmap f)   ≡ fmap f . join
    -}
