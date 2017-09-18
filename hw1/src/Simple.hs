module Simple
    ( order3
    , highestBit
    , highestBitHard
    , smartReplicate
    , contains
    ) where

-- Упорядочить по возрастанию переданную тройку элементов.
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z)
    | x <= y    = if y <= z then (x, y, z)
                            else (min x z, max x z, y)
    | otherwise = if y >= z then (z, y, x)
                            else (y, min x z, max x z)

-- Необходимо реализовать функцию, которая возвращает наибольшую степень двойки,
-- не превосходящую заданного числа.
highestBit :: (Ord a, Num a) => a -> a
highestBit = fst . highestBitHard

-- Дополнительно необходимо вернуть показатель степени.
highestBitHard :: (Ord a, Num a) => a -> (a, Int)
highestBitHard n = head $ filter ((>= n) . fst) $ iterate nextExp (fromInteger 1, 0)
  where
    nextExp (value, power) = (value * 2, power + 1)

-- Функция должна повторять каждый элемент столько раз, чему равен сам элемент.
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\n -> replicate n n)

-- Напишите функцию, которой передаётся список списков и некоторый элемент.
-- Эта функция должна вернуть список только тех списков, которые содержат
-- переданный элемент.
contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)
