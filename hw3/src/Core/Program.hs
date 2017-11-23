module Core.Program
    ( Program
    , Statement (..)
    ) where

import           Core.Expr              (EvalContext)
import           Core.Variables         (VariableAssignment (..),
                                         VariableDeclaration (..), VariableError (..),
                                         declareVariable, updateVariable)

import           Data.ByteString        (ByteString, append)

import           Control.Monad.Except   (MonadError, throwError, withExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State    (MonadState, get, put)

type Program a = [Statement a]

data Statement a
    = VariableDeclarationStatement (VariableDeclaration a)
    | VariableAssignmentStatement (VariableAssignment a)

data ProgramError = ProgramError
    { programErrorDescription :: ByteString
    , programErrorLine        :: Int
    }

data ProgramState a = ProgramState
    { programEvalContext :: EvalContext a
    , statementLine      :: Int
    }

class ProgramErrorCause a where
    getDescription :: a -> ByteString

instance ProgramErrorCause VariableError where
    getDescription (UndefinedVariableAssignment varName) =
            "can't assign to undefined variable " `append` varName
    getDescription (AlreadyDefinedVariable varName) =
            "variable " `append` varName `append` " already defined"

runStatement :: ( Integral a
                , MonadState (ProgramState a) m
                , MonadError ProgramError m
                , MonadIO m
                ) => Statement a -> m ()
runStatement = undefined
