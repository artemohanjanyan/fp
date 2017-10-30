{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.FromMonadJoin
    (
    ) where

import           Monads.Definitions

import           Prelude            (Functor, fmap, (.))

{- LAWS
    1. join . returnJoin      ≡ id
    2. join . fmap returnJoin ≡ id
    3. join . fmap join       ≡ join . join
    4* join . fmap (fmap f)   ≡ fmap f . join
-}

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    m >>= f = join (fmap f m)
    {- 1. m >>= return                             ≡ m
     - =[def return]= m >>= returnJoin
     - =[def >>=]= join (fmap returnJoin m)
     - =[def (.)]= (join . fmap returnJoin) m
     - =[2.]= id m
     - =[def id]= m
     -}

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = join . fmap g . f
    {- 1. f >=> returnFish                         ≡ f
     - =[def returnFish]= f >=> returnJoin
     - =[def (>=>)]= join . fmap returnJoin . f
     - =[2.] = f
     -}
