module Core.Variables
    ( VariableName
    , VariableAssignment (..)
    , VariableDeclaration (..)
    , VariableError (..)
    , declareVariable
    , updateVariable
    ) where

import           Core.Expr            (Expr)
import           Core.Typesystem      (EvalContext, VariableName)

import qualified Data.Map.Strict      as Map

import           Control.Monad        (when)
import           Control.Monad.Except (MonadError, throwError)

import           Ether.State          (MonadState', get', put')

data VariableAssignment a
    = VariableAssignment VariableName (Expr a)
    deriving (Show)

newtype VariableDeclaration a
    = VariableDeclaration (VariableAssignment a)
    deriving (Show)

data VariableError
    = UndefinedVariableAssignment VariableName
    | AlreadyDefinedVariable VariableName
    deriving (Show)

declareVariable :: ( MonadState' (EvalContext a) m
                   , MonadError VariableError m
                   )
                => VariableName -> a -> m ()
declareVariable varName varValue = do
    context <- get'
    when (Map.member varName context)
            $ throwError $ AlreadyDefinedVariable varName
    put' $ Map.insert varName varValue context

updateVariable :: ( MonadState' (EvalContext a) m
                  , MonadError VariableError m
                  )
               => VariableName -> a -> m ()
updateVariable varName varValue = do
    context <- get'
    when (Map.notMember varName context)
            $ throwError $ UndefinedVariableAssignment varName
    put' $ Map.insert varName varValue context
