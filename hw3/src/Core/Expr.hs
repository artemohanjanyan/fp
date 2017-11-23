module Core.Expr
    ( Expr (..)
    , EvalError (..)
    , EvalContext
    , eval
    ) where

import           Data.ByteString      (ByteString)
import qualified Data.Map.Strict      as Map

import           Control.Applicative  (liftA2)
import           Control.Monad        (when)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ask, local)

data Expr a
    = Lit a
    | Var ByteString
    | Expr a :+: Expr a
    | Expr a :-: Expr a
    | Expr a :*: Expr a
    | Expr a :/: Expr a
    | Let ByteString (Expr a) (Expr a)
    deriving (Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

data EvalError
    = DivisionByZero
    | UndefinedVariable
    deriving (Show)

type EvalContext a = Map.Map ByteString a

eval :: ( MonadReader (EvalContext a) m
        , MonadError EvalError m
        , Integral a
        ) => Expr a -> m a
eval (Lit expr)        = pure expr
eval (Var var)         = do
    context <- ask
    case Map.lookup var context of
        Nothing    -> throwError UndefinedVariable
        Just value -> pure value
eval (expr1 :+: expr2) = liftA2 (+) (eval expr1) (eval expr2)
eval (expr1 :-: expr2) = liftA2 (-) (eval expr1) (eval expr2)
eval (expr1 :*: expr2) = liftA2 (*) (eval expr1) (eval expr2)
eval (expr1 :/: expr2) = do
    right <- eval expr2
    when (right == 0) $ throwError DivisionByZero
    left <- eval expr1
    pure (left `div` right)
eval (Let var varExpr expr) = do
    varValue <- eval varExpr
    local (Map.insert var varValue) (eval expr)