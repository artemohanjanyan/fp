module Parser.ProgramParser
    ( programParser
    ) where

import           Core.Program                    (Program, Statement (..))

import           Parser.Common                   (Parser)
import           Parser.VariablesParser          (variableAssignmentParser,
                                                  variableDeclarationParser)

import           Control.Applicative             ((<*), (<|>))
import           Control.Applicative.Combinators (sepBy)
import           Text.Megaparsec                 (eof)
import           Text.Megaparsec.Byte            (space)

statementParser :: Integral a => Parser (Statement a)
statementParser =
        (VariableDeclarationStatement <$> variableDeclarationParser)
            <|> (VariableAssignmentStatement <$> variableAssignmentParser)

programParser :: Integral a => Parser (Program a)
programParser = sepBy statementParser space <* eof
