module Core.Variables
    ( VariableAssignment (..)
    , VariableDeclaration (..)
    , VariableError (..)
    , declareVariable
    , updateVariable
    ) where

import           Core.Expr            (EvalContext, Expr)

import           Data.ByteString      (ByteString)
import qualified Data.Map.Strict      as Map

import           Control.Monad        (when)
import           Control.Monad.Except (MonadError, throwError)

import           Ether.State          (MonadState', get', put')

data VariableAssignment a = VariableAssignment ByteString (Expr a)

newtype VariableDeclaration a = VariableDeclaration (VariableAssignment a)

data VariableError
    = UndefinedVariableAssignment ByteString
    | AlreadyDefinedVariable ByteString
    deriving (Show)

declareVariable :: ( MonadState' (EvalContext a) m
                   , MonadError VariableError m
                   )
                => ByteString -> a -> m ()
declareVariable varName varValue = do
    context <- get'
    when (Map.member varName context)
            $ throwError $ AlreadyDefinedVariable varName
    put' $ Map.insert varName varValue context

updateVariable :: ( MonadState' (EvalContext a) m
                  , MonadError VariableError m
                  )
               => ByteString -> a -> m ()
updateVariable varName varValue = do
    context <- get'
    when (Map.notMember varName context)
            $ throwError $ UndefinedVariableAssignment varName
    put' $ Map.insert varName varValue context
