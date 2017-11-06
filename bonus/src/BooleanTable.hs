{-# LANGUAGE FlexibleInstances #-}

module BooleanTable where

import           Control.Monad  (forM_)
import           Data.Bifunctor (first)

class BooleanTableDomain a where
    makeTable :: a -> [([Bool], Bool)]

instance BooleanTableDomain Bool where
    makeTable = pure . pure

instance BooleanTableDomain a => BooleanTableDomain (Bool -> a) where
    makeTable f = do
        arg <- [True, False]
        restTable <- makeTable (f arg)
        pure $ first (arg:) restTable

printTable :: BooleanTableDomain a => a -> IO ()
printTable f = do
    let table = makeTable f
    forM_ table printRow
  where
    printRow :: ([Bool], Bool) -> IO ()
    printRow (args, ans) = do
        forM_ args (putStr . boolToAlignedStr)
        putStr "= "
        print ans

    boolToAlignedStr :: Bool -> String
    boolToAlignedStr True  = "True  "
    boolToAlignedStr False = "False "
