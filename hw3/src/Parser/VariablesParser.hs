module Parser.VariablesParser
    ( variableAssignmentParser
    , variableDeclarationParser
    ) where

import           Core.Variables    (VariableAssignment (..), VariableDeclaration (..))

import           Parser.Common     (Parser, identifier, symbol_)
import           Parser.ExprParser (exprParser)

variableAssignmentParser :: Integral a => Parser (VariableAssignment a)
variableAssignmentParser = do
    varName <- identifier
    symbol_ "="
    expr <- exprParser
    pure $ VariableAssignment varName expr

variableDeclarationParser :: Integral a => Parser (VariableDeclaration a)
variableDeclarationParser = do
    symbol_ "mut"
    VariableDeclaration <$> variableAssignmentParser
