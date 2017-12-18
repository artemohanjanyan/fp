{-# LANGUAGE TupleSections #-}

module TemplateHaskell
    ( choseByIndices
    ) where

import qualified Data.IntMap.Strict  as M
import qualified Data.IntSet         as S
import           Data.Maybe          (maybe)
import           Language.Haskell.TH (Exp, Q, lamE, newName, tupE, tupP, varE, varP,
                                      wildP)

choseByIndices :: Int -> [Int] -> Q Exp
choseByIndices tupleLength indices = do
    let indexSet = S.fromList indices

    varNames <- fmap M.fromList $
            mapM (\x -> fmap (x, ) $ newName "x") $
            filter (`S.member` indexSet)
            [0..(tupleLength - 1)]

    let arg = tupP $ map (maybe wildP varP . (`M.lookup` varNames))
                     [0..(tupleLength - 1)]
    let body = tupE $ map (varE . (varNames M.!)) indices

    lamE [arg] body
