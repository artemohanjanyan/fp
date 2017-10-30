module Parser.SExpr where

import           Parser.Common       (ParserC, char, ident, posInt)

import           Control.Applicative (Alternative (..))

type Ident = String

data Atom = N Integer | I Ident
    deriving Show

data SExpr
    = SAtom Atom
    | Comb [SExpr]
    deriving Show

parseSExpr :: ParserC SExpr
parseSExpr = (char '(' *> (Comb <$> some parseSExpr) <* char ')')
        <|> SAtom <$> parseAtom

parseAtom :: ParserC Atom
parseAtom = N <$> posInt <|> I <$> ident
