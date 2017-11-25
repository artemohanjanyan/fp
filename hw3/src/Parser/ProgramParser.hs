module Parser.ProgramParser
    ( srcParser
    ) where

import           Core.Program                    (Program, Statement (..))

import           Parser.Common                   (Parser, identifier, symbol_)
import           Parser.ExprParser               (exprParser)
import           Parser.VariablesParser          (variableAssignmentParser,
                                                  variableDeclarationParser)

import           Control.Applicative             ((<*), (<|>))
import           Control.Applicative.Combinators (sepBy)
import           Text.Megaparsec                 (eof, try)
import           Text.Megaparsec.Byte            (space)

statementParser :: forall a . Integral a => Parser (Statement a)
statementParser =
        try (VariableDeclarationStatement <$> variableDeclarationParser)
            <|> try (VariableAssignmentStatement <$> variableAssignmentParser)
            <|> try printParser
            <|> try readParser
            <|> try forParser
            <|> try breakParser
  where
    printParser :: Parser (Statement a)
    printParser = PrintStatement <$> (symbol_ "<" *> exprParser)

    readParser :: Parser (Statement a)
    readParser = ReadStatement <$> (symbol_ ">" *> identifier)

    forParser :: Parser (Statement a)
    forParser = do
        symbol_ "for"
        varName <- identifier
        symbol_ "="
        fromExpr <- exprParser
        symbol_ "to"
        toExpr <- exprParser
        symbol_ "{"
        body <- programParser
        symbol_ "}"
        pure $ ForStatement varName fromExpr toExpr body

    breakParser :: Parser (Statement a)
    breakParser = do
        symbol_ "break"
        pure BreakStatement

programParser :: Integral a => Parser (Program a)
programParser = sepBy statementParser space

srcParser :: Integral a => Parser (Program a)
srcParser = space *> programParser <* eof
