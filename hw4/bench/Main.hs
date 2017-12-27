{-# LANGUAGE TypeApplications #-}

module Main where

import           Criterion.Main (bench, bgroup, defaultMain, nf)
import qualified Data.IntSet    as S
import           Data.List      (group, nub, partition, sort)
import           System.Random  (randomRIO)

intSetNub :: [Int] -> [Int]
intSetNub = S.toList . S.fromList

unique :: [Int] -> [Int]
unique = map head . group

slowSortNub :: [Int] -> [Int]
slowSortNub = unique . qsort
  where
    qsort :: [Int] -> [Int]
    qsort []     = []
    qsort [x]    = [x]
    qsort (x:xs) =
        let (le, gt) = partition (<= x) xs
        in le ++ [x] ++ gt

libSortNub :: [Int] -> [Int]
libSortNub = unique . sort

randomList :: Int -> Int -> IO ([Int])
randomList 0 _   = pure []
randomList n lim = do
  r <- randomRIO (1, lim)
  rs <- randomList (n - 1) lim
  return (r : rs)

main :: IO ()
main = do
  list1000 <- randomList 1000 100
  list5000 <- randomList 5000 300
  defaultMain $
    let testList = [1,2 .. 100] ++ [1,3 .. 100] ++ [1,5 .. 100]
    in [ bgroup
           "test300"
           [ bench "lib nub" $ nf (nub @Int) testList
           , bench "intset nub" $ nf intSetNub testList
           , bench "slowsort nub" $ nf slowSortNub testList
           , bench "libsort nub" $ nf libSortNub testList
           ]
       , bgroup
           "random1000"
           [ bench "lib nub" $ nf (nub @Int) list1000
           , bench "intset nub" $ nf intSetNub list1000
           , bench "slowsort nub" $ nf slowSortNub list1000
           , bench "libsort nub" $ nf libSortNub list1000
           ]
       , bgroup
           "random5000"
           [ bench "lib nub" $ nf (nub @Int) list5000
           , bench "intset nub" $ nf intSetNub list5000
           , bench "slowsort nub" $ nf slowSortNub list5000
           , bench "libsort nub" $ nf libSortNub list5000
           ]
       ]
