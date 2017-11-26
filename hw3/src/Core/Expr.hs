module Core.Expr
    ( Expr (..)
    , EvalError (..)
    , EvalContext
    , eval
    ) where

import           Core.Typesystem      (EvalContext, VariableName)

import qualified Data.Map.Strict      as Map

import           Control.Applicative  (liftA2)
import           Control.Monad        (when)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ask, local)

data Expr a
    = Lit a
    | Var VariableName
    | Expr a :+: Expr a
    | Expr a :-: Expr a
    | Expr a :*: Expr a
    | Expr a :/: Expr a
    | Let VariableName (Expr a) (Expr a)
    deriving (Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

data EvalError
    = DivisionByZero
    | UndefinedVariable VariableName
    deriving (Show)

eval :: ( Integral a
        , MonadReader (EvalContext a) m
        , MonadError EvalError m
        )
     => Expr a -> m a
eval (Lit expr)        = pure expr
eval (Var var)         = do
    context <- ask
    case Map.lookup var context of
        Nothing    -> throwError $ UndefinedVariable var
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
