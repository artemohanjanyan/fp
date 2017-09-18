module Adts
    ( Day(..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty
    ) where

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
