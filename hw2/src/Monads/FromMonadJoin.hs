{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.FromMonadJoin where

import           Monads.Definitions

import           Prelude            (Functor, fmap, (.))

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    m >>= f = join (fmap f m)

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = join . fmap g . f
