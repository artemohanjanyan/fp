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

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> f x >>= g

instance Monad m => MonadJoin m where
    returnJoin = return
    join m = m >>= id
