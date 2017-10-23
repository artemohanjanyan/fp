module NonDeterministic where

import           Control.Monad (replicateM)

bin :: Int -> [[Int]]
--bin n | n <= 0 = [[]]
--bin n =
--    [0, 1] >>= \x ->
--    bin (n - 1) >>= \xs ->
--    pure (x:xs)
bin = flip replicateM [0, 1]

combinations :: Int -> Int -> [[Int]]
combinations n k
    | n == k                  = [reverse [1..n]]
    | k == 0                  = [[]]
    | n < k || n < 0 || k < 0 = []
    | otherwise =
        [True, False] >>= \shouldTake ->
        if shouldTake
            then (n:) <$> combinations (n - 1) (k - 1)
            else combinations (n - 1) k

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations list =
    splits list >>= \(first, rest) ->
    permutations rest >>= \restPerm ->
    pure (first:restPerm)
  where
    splits :: [a] -> [(a, [a])]
    splits []     = undefined
    splits [x]    = [(x, [])]
    splits (x:xs) = (x, xs) : map (fmap (x:)) (splits xs)
