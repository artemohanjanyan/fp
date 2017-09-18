module Folds
    ( splitOn
    , joinWith
    ) where

splitOn :: (Eq a, Foldable t) => a -> t a -> [[a]]
splitOn x = foldr it [[]]
  where
    it y acc@(accHead:accRest)
        | x == y    = []:acc
        | otherwise = (y:accHead):accRest
    it _ [] = error "impossible"

joinWith :: a -> [[a]] -> [a]
joinWith x = foldr1 (\next acc -> next ++ [x] ++ acc)
