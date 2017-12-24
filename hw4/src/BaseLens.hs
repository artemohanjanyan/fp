{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TupleSections #-}

module BaseLens
    ( Lens
    , Lens'

    , set'
    , view'
    , over'

    , lens
    , set
    , view
    , over

    , choosing
    , _1
    , _2

    , (<%~)
    , (<<%~)
    ) where

import           Data.Either           (either)
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

{-
type Lens' s a    = forall f . Functor f => (a -> f a) -> s -> f s
-}
type Lens' s a = Lens s s a a

set'  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set' lens' field obj = runIdentity $ lens' (const $ Identity field) obj

view' :: Lens' s a -> s -> a              -- lookup value (getter)
view' lens' obj = getConst $ lens' Const obj

over' :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over' lens' updater obj = runIdentity $ lens' (Identity . updater) obj

{-
infixr 4 .~
(.~) :: Lens' s a -> a -> s -> s
(.~) = set

infixl 8 ^.
(^.) :: s -> Lens' s a -> a
(^.) obj lens' = view lens' obj

infixr 4 %~
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over
-}

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 bindArg (a, b) = (, b) <$> bindArg a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 bindArg (a, b) = (a, ) <$> bindArg b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get sett bindArg obj = sett obj <$> bindArg (get obj)

set  :: Lens s t a b -> b -> s -> t         -- set    value (setter)
set lens' field obj = runIdentity $ lens' (const $ Identity field) obj

view :: Lens s t a b -> s -> a              -- lookup value (getter)
view lens' obj = getConst $ lens' Const obj

over :: Lens s t a b -> (a -> b) -> s -> t  -- change value (modifier)
over lens' updater obj = runIdentity $ lens' (Identity . updater) obj

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens
    (either (view l1) (view l2))
    (either (\a b -> Left  $ set l1 b a)
            (\a b -> Right $ set l2 b a))

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (\a -> let res = f a in (res, res))

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (\a -> (a, f a))
