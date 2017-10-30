{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monads.FromMonadFish
    (
    ) where

import           Monads.Definitions

import           Prelude            (const, id)

{- LAWS
    1. f >=> returnFish ≡ f
    2. returnFish >=> f ≡ f
    3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
-}

instance MonadFish m => Monad m where
    return = returnFish
    m >>= f = (const m >=> f) ()

    {-
        1. m >>= return                            ≡ m
        =[def return]= m >>= returnFish
        =[def >>=]= (const m >=> returnFish) ()
        =[1.]= const m ()
        =[def const]= m
    -}

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
