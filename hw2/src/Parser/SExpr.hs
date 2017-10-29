module Parser.SExpr where

import           Parser.Common (ParserC, ident, posInt)
import           Parser.Core

type Ident = String

data Atom = N Integer | I Ident
    deriving Show

data SExpr
    = A Atom
    | Comb [SExpr]
    deriving Show

parseSExpr :: ParserC SExpr
parseSExpr = char '('

parseAtom :: ParserC Atom
parseAtom = N <$> posInt <|> I <$> ident
