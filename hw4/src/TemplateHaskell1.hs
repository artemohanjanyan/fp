{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module TemplateHaskell1
    ( choseByIndices'
    , A (..)
    ) where

import           TemplateHaskell            (choseByIndices, deriveText)

import           Language.Haskell.TH        (Exp (TupE), Q, runQ)
import           Language.Haskell.TH.Syntax (Lift)

import           Control.Monad              (fail)

choseByIndices' :: Lift a => [Int] -> a -> Q Exp
choseByIndices' indices tuple = do
    parsed <- runQ [| tuple |]
    case parsed of
        TupE elems -> let n = length elems in [| $(choseByIndices n indices) tuple |]
        _          -> fail "second argument must be a tuple"

data A
    = B { getInt :: Int, getD :: Double }
    | C { getQ :: Bool }
    | D
deriveText ''A
