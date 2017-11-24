module Parser.ProgramParser
    ( programParser
    ) where

import           Core.Program                    (Program, Statement (..))

import           Parser.Common                   (Parser, identifier, symbol_)
import           Parser.ExprParser               (exprParser)
import           Parser.VariablesParser          (variableAssignmentParser,
                                                  variableDeclarationParser)

import           Control.Applicative             ((<*), (<|>))
import           Control.Applicative.Combinators (sepBy)
import           Text.Megaparsec                 (eof)
import           Text.Megaparsec.Byte            (space)

statementParser :: forall a . Integral a => Parser (Statement a)
statementParser =
        (VariableDeclarationStatement <$> variableDeclarationParser)
            <|> (VariableAssignmentStatement <$> variableAssignmentParser)
            <|> printParser
            <|> readParser
  where
    printParser :: Parser (Statement a)
    printParser = PrintStatement <$> (symbol_ "<" *> exprParser)

    readParser :: Parser (Statement a)
    readParser = ReadStatement <$> (symbol_ ">" *> identifier)

programParser :: Integral a => Parser (Program a)
programParser = sepBy statementParser space <* eof
