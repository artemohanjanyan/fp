{-# LANGUAGE TypeApplications #-}

module Main where

import           Criterion.Main      (bench, bgroup, defaultMain, nf)
import qualified Data.IntSet         as S
import           Data.List           (group, nub, partition, sort)
import qualified Data.Vector.Unboxed as V
import           System.Random       (randomRIO)


libUnique :: [Int] -> [Int]
libUnique = map head . group

libSortNub :: [Int] -> [Int]
libSortNub = libUnique . sort


unique :: [Int] -> [Int]
unique xs' = unique' xs' []
  where
    unique' [] acc = acc
    unique' (x:xs) [] = unique' xs [x]
    unique' (x:xs) (acc@(lastX:_))
        | x == lastX = unique' xs acc
        | otherwise  = unique' xs (x:acc)

manualNub :: [Int] -> [Int]
manualNub = unique . qsort
  where
    qsort :: [Int] -> [Int]
    qsort []     = []
    qsort [x]    = [x]
    qsort (x:xs) =
        let (le, gt) = partition (<= x) xs
        in le ++ [x] ++ gt


vecSort :: V.Vector Int -> V.Vector Int
vecSort vec
    | V.null vec = vec
    | V.length vec == 1 = vec
    | otherwise =
        let x = vec V.! 0
            (le, gt) = V.partition (<= x) vec
        in le V.++ gt

vecNub :: [Int] -> [Int]
vecNub list =
    let vec = V.fromList list
        sorted = vecSort vec
        ans = V.uniq sorted
    in V.toList ans


intSetNub :: [Int] -> [Int]
intSetNub = S.toList . S.fromList


randomList :: Int -> Int -> IO [Int]
randomList 0 _   = pure []
randomList n maxElem = do
  r <- randomRIO (1, maxElem)
  rs <- randomList (n - 1) maxElem
  pure $ r:rs


main :: IO ()
main = do
  list1000 <- randomList 1000 100
  list5000 <- randomList 5000 300
  list10000 <- randomList 10000 1000
  defaultMain $
    let testList = [1,2 .. 100] ++ [1,3 .. 100] ++ [1,5 .. 100]
    in [ bgroup
           "test300"
           [ bench "intset nub"  $ nf intSetNub  testList
           , bench "vec nub"     $ nf vecNub     testList
           , bench "manual nub"  $ nf manualNub  testList
           , bench "libsort nub" $ nf libSortNub testList
           , bench "lib nub"     $ nf (nub @Int) testList
           ]
       , bgroup
           "random1000"
           [ bench "intset nub"  $ nf intSetNub  list1000
           , bench "vec nub"     $ nf vecNub     list1000
           , bench "manual nub"  $ nf manualNub  list1000
           , bench "libsort nub" $ nf libSortNub list1000
           , bench "lib nub"     $ nf (nub @Int) list1000
           ]
       , bgroup
           "random5000"
           [ bench "intset nub"  $ nf intSetNub  list5000
           , bench "vec nub"     $ nf vecNub     list5000
           , bench "manual nub"  $ nf manualNub  list5000
           , bench "libsort nub" $ nf libSortNub list5000
           , bench "lib nub"     $ nf (nub @Int) list5000
           ]
       , bgroup
           "random10000"
           [ bench "intset nub"  $ nf intSetNub  list10000
           , bench "vec nub"     $ nf vecNub     list10000
           , bench "manual nub"  $ nf manualNub  list10000
           , bench "libsort nub" $ nf libSortNub list10000
           , bench "lib nub"     $ nf (nub @Int) list10000
           ]
       ]
