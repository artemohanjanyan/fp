{-# LANGUAGE TupleSections #-}

module TemplateHaskell
    ( choseByIndices
    , ShowText (..)
    , deriveText
    ) where

import           Control.Monad       (fail, when)
import qualified Data.IntMap.Strict  as M
import qualified Data.IntSet         as S
import           Data.Maybe          (maybe)
import qualified Data.Text           as T
import           Language.Haskell.TH (Dec, Exp, Name, Q, conT, lamE, newName, tupE, tupP,
                                      varE, varP, wildP)

choseByIndices :: Int -> [Int] -> Q Exp
choseByIndices tupleLength indices = do
    when (any (>= tupleLength) indices) $ fail "index > tuple length"
    let indexSet = S.fromList indices

    varNames <- fmap M.fromList $
            mapM (\x -> fmap (x, ) $ newName "x") $
            filter (`S.member` indexSet)
            [0..(tupleLength - 1)]

    let arg = tupP $ map (maybe wildP varP . (`M.lookup` varNames))
                     [0..(tupleLength - 1)]
    let body = tupE $ map (varE . (varNames M.!)) indices

    lamE [arg] body

class ShowText a where
    showText :: a -> T.Text

deriveText :: Name -> Q [Dec]
deriveText name = [d|instance ShowText $(conT name) where showText _ = T.empty|]
