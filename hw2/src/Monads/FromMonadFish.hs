{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.FromMonadFish where

import           Monads.Definitions

import           Prelude            (const, id)

instance MonadFish m => Monad m where
    return = returnFish
    m >>= f = (const m >=> f) "some value"

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
