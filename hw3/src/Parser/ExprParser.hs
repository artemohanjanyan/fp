module Parser.ExprParser
    ( exprParser
    ) where

import           Core.Expr                  (Expr (..))

import           Parser.Common              (Parser, identifier, lexeme, symbol_)

import           Control.Applicative        ((*>), (<*), (<|>))

import           Text.Megaparsec            (try)
import           Text.Megaparsec.Byte       (space)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

parens :: Parser a -> Parser a
parens p = symbol_ "(" *> p <* symbol_ ")"

termParser :: Integral a => Parser (Expr a)
termParser =
    Lit <$> lexeme (L.signed space L.decimal)
      <|> Var <$> identifier
      <|> parens (try letParser <|> exprParser)
  where
    letParser = do
        symbol_ "let"
        varName <- lexeme identifier
        symbol_ "="
        varValue <- exprParser
        symbol_ "in"
        expr <- exprParser
        pure $ Let varName varValue expr

exprParser :: Integral a => Parser (Expr a)
exprParser = makeExprParser termParser table
  where
    table =
        [ [ binary "*" (:*:)
          , binary "/" (:/:)
          ]
        , [ binary "+" (:+:)
          , binary "-" (:-:)
          ]
        ]

    binary name f = InfixL (f <$ symbol_ name)
