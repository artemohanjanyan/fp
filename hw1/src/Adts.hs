module Adts
    ( Day(..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty

    , Nat(..)
    ) where

import GHC.Real (Ratio((:%)))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Возвращает следующий за переданным день недели.
nextDay :: Day -> Day
nextDay Sun = Mon
nextDay day = succ day

-- Возвращает день недели, который наступит после заданного через переданное
-- число дней.
afterDays :: Day -> Int -> Day
afterDays day dayN = afterDays' day (dayN `mod` 7)
  where
    afterDays' day' dayN' = iterate succ day' !! dayN'

-- Проверяет, является ли день недели выходным.
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- Выводит число дней, оставшихся до пятницы.
daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = daysToParty (nextDay day) + 1


data Nat = Z | S Nat deriving Show

instance Eq Nat where
    Z    == Z    = True
    S n1 == S n2 = n1 == n2
    _    == _    = False

instance Ord Nat where
    Z    <= _    = True
    S n1 <= S n2 = n1 <= n2
    _    <= Z    = False

instance Num Nat where
    Z    + n  = n
    S n1 + n2 = n1 + S n2

    Z    * _  = Z
    S n1 * n2 = n2 + n1 * n2

    abs n = n

    signum Z = Z
    signum _ = S Z

    fromInteger 0 = Z
    fromInteger n
        | n > 0     = S $ fromInteger $ n - 1
        | otherwise = error "fromInteger (negative integer) :: Nat"

    n    - Z    = n
    S n1 - S n2 = n1 - n2
    Z    - _    = error "Z - S n"

instance Real Nat where
    toRational = (GHC.Real.:% 1) . toInteger

instance Enum Nat where
    toEnum = fromInteger . toEnum
    fromEnum = fromEnum . toInteger

instance Integral Nat where
    toInteger Z     = 0
    toInteger (S n) = 1 + toInteger n

    a `quotRem` b
        | a >= b =
            let (quot', rem') = (a - b) `quotRem` b
            in (quot' + 1, rem')
        | otherwise = (Z, a)

    divMod = quotRem

-- ‘even’ and ‘gcd’ defined in ‘GHC.Real’ now work
