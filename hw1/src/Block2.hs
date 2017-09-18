module Block2
    ( randomIntList
    , removeAt
    , removeAtHard
    , collectEvery
    , stringSum
    , stringSumHard
    , mergeSort
    ) where

import Data.Char (isDigit, isSpace)
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- Реализовать функцию, которая удаляет элемент по заданному индексу.
removeAt :: Int -> [a] -> [a]
removeAt i = snd . removeAtHard i

-- Тип функции removeAt должен быть следующим:
removeAtHard :: Int -> [a] -> (Maybe a, [a])
removeAtHard 0 (x:xs) = (Just x, xs)
removeAtHard _ []     = (Nothing, [])
removeAtHard n (x:xs) = (x:) <$> removeAtHard (n - 1) xs

-- Необходимо написать функцию, которая выбрасывает из списка каждый k-ый
-- элемент, при этом все выброшенные элементы нужно тоже вернуть.
collectEvery :: Int -> [a] -> ([a], [a])
collectEvery k = collectEveryIt 0
  where
    collectEveryIt n (x:xs)
        | k == n    = (x:) <$> collectEveryIt 0 xs
        | otherwise =
            let (left, dropped) = collectEveryIt (n + 1) xs
            in (x:left, dropped)
    collectEveryIt _ [] = ([], [])

-- Требуется написать функцию, которая принимает строку, состоящую только из
-- чисел, разделённых пробельными символами, и находит сумму всех чисел в
-- этой строке.
stringSum :: String -> Maybe Integer
stringSum = stringSumHard

-- Функция должна корректно обрабатывать числа с унарным плюсом.
stringSumHard :: String -> Maybe Integer
stringSumHard []         = Just 0
stringSumHard str@(c:cs)
    | c == '+'  = takeIntAndAdd (+) cs
    | c == '-'  = takeIntAndAdd (-) cs
    | isSpace c = stringSumHard cs
    | isDigit c = takeIntAndAdd (+) str
    | otherwise = Nothing
  where
    takeInt :: String -> Maybe (Integer, String)
    takeInt str' =
        let (digits, rest1) = span isDigit str'
        in case digits of
            [] -> Nothing
            _  -> case rest1 of
                []              -> Just (read digits, [])
                (s:rest2)
                    | isSpace s -> Just (read digits, rest2)
                    | otherwise -> Nothing

    takeIntAndAdd f str' = do
        (nextInt, rest) <- takeInt str'
        restSum <- stringSumHard rest
        pure $ f restSum nextInt

-- Требуется реализовать сортировку слиянием.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge sorted1 sorted2
  where
    (part1, part2) = splitAt (length list `div` 2) list

    sorted1 = mergeSort part1
    sorted2 = mergeSort part2

    merge p1@(x:xs) p2@(y:ys)
        | x < y     = x : merge xs p2
        | otherwise = y : merge p1 ys
    merge [] p2 = p2
    merge p1 [] = p1
