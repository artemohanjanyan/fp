{-# LANGUAGE TupleSections #-}

module TemplateHaskell1
    ( choseByIndices'
    ) where

import           TemplateHaskell            (choseByIndices)

import           Language.Haskell.TH        (Exp (TupE), Q, runQ)
import           Language.Haskell.TH.Syntax (Lift)

choseByIndices' :: Lift a => [Int] -> a -> Q Exp
choseByIndices' indices tuple = do
    parsed <- runQ [| tuple |]
    case parsed of
        TupE elems -> let n = length elems in [| $(choseByIndices n indices) tuple |]
        _          -> undefined
