{-# LANGUAGE TupleSections #-}

module TemplateHaskell
    ( choseByIndices
    , ShowText (..)
    , deriveText
    , deriveShow
    ) where

import           Control.Monad              (fail, when)
import qualified Data.IntMap.Strict         as M
import qualified Data.IntSet                as S
import           Data.List                  (intercalate)
import           Data.Maybe                 (maybe)
import qualified Data.Text                  as T
import           Language.Haskell.TH        (Clause, Con (NormalC, RecC),
                                             Dec (DataD, FunD, InstanceD), Exp,
                                             Info (TyConI), Name, Q, asP, clause, conP,
                                             conT, lamE, listE, mkName, nameBase, newName,
                                             normalB, reify, tupE, tupP, varE, varP,
                                             wildP)
import           Language.Haskell.TH.Syntax (VarBangType)

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

deriveShow :: Name -> Q [Dec]
deriveShow typeName = do
    TyConI (DataD _ _ _ _ cons _) <- reify typeName

    let names fields = map (\(name', _, _) -> name') fields

    let showField :: Name -> Q Exp
        showField name' = [|\x -> s ++ " = " ++ show ($(varE name') x)|]
            where s = nameBase name'

    let showFields :: [VarBangType] -> Q Exp
        showFields fields = listE $ map showField (names fields)

    let showForCon :: Con -> Q Clause
        showForCon con =
            let conName = case con of
                    RecC    conName _ -> conName
                    NormalC conName _ -> conName
                    _                 -> error "unsupported constructor"
                conNameStr = nameBase conName in
            case con of
                RecC    _ fields -> do
                    patName <- newName "x"
                    let expr = [| conNameStr
                            ++ " { "
                            ++ intercalate ", " (map ($ $(varE patName)) $(showFields fields))
                            ++ " } " |]
                    clause [asP patName $ conP conName (replicate (length fields) wildP)]
                           (normalB expr) []
                NormalC _ fields -> do
                    let expr = [| conNameStr |]
                    when (length fields > 0) $ fail "unsupported constructor"
                    clause [conP conName []]
                           (normalB expr) []

    funD <- FunD (mkName "show") <$> mapM showForCon cons

    [InstanceD overlap cxt ttype _] <- [d|
            instance Show $(conT typeName) where
                show _ = undefined |]
    pure [InstanceD overlap cxt ttype [funD]]
