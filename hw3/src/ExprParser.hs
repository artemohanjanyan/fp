{-# LANGUAGE OverloadedStrings #-}

module ExprParser
    ( exprParser
    ) where

import           Expr                       (Expr (..))


import           Control.Applicative        (empty, many, (*>), (<*), (<|>))
import           Control.Monad              (void)

import           Data.ByteString            (ByteString, pack)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, try)
import           Text.Megaparsec.Byte       (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

type Parser = Parsec Void ByteString

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol_ :: ByteString -> Parser ()
symbol_ = void . L.symbol space

parens :: Parser a -> Parser a
parens p = symbol_ "(" *> p <* symbol_ ")"

termParser :: Integral a => Parser (Expr a)
termParser =
    Lit <$> lexeme L.decimal
      <|> Var <$> identifier
      <|> parens (try letParser <|> exprParser)
  where
    identifier = lexeme (pack <$> ((:) <$> letterChar <*> many alphaNumChar))

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
