{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.FromMonad
    (
    ) where

import           Monads.Definitions

import           Prelude            (id)

{- LAWS
    1. m >>= return    ≡ m
    2. return a >>= f  ≡ f a
    3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
-}

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> f x >>= g

    {-
        1. f >=> returnFish                        ≡ f
        =[def returnFish]= f >=> return
        =[def >=>]= \x -> f x >>= return
        =[1.]= \x -> f x
        =[eta]= f
    -}

instance Monad m => MonadJoin m where
    returnJoin = return
    join m = m >>= id

    {-
        1. join . returnJoin                       ≡ id
        =[def returnJoin]= join . return
        =[def (.)]= \x -> join (return x)
        =[def join]= \x -> return x >>= id
        =[2.]= \x -> id x
        =[eta]= id
    -}
