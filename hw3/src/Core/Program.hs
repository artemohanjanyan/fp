module Core.Program
    ( Program
    , Statement (..)
    , ProgramError (..)
    , StatementLine
    , ProgramErrorCause (..)
    , ProgramConstraint
    , runProgram
    ) where

import           Core.Expr              (EvalContext, EvalError (..), eval)
import           Core.Variables         (VariableAssignment (..),
                                         VariableDeclaration (..), VariableError (..),
                                         declareVariable, updateVariable)

import           Data.ByteString        (ByteString, append)

import           Control.Monad          (mapM_)
import           Control.Monad.Except   (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (runReaderT)

import           Control.Error.Util     (exceptT)
import           Ether.State            (MonadState', get', modify')

type ProgramConstraint a m =
    ( Integral a
    , MonadState' (EvalContext a) m
    , MonadState' StatementLine m
    , MonadError ProgramError m
    , MonadIO m
    )

type Program a = [Statement a]

data Statement a
    = VariableDeclarationStatement (VariableDeclaration a)
    | VariableAssignmentStatement (VariableAssignment a)
    deriving (Show)

data ProgramError = ProgramError
    { programErrorDescription :: ByteString
    , programErrorLine        :: StatementLine
    }

type StatementLine = Int

class ProgramErrorCause a where
    getDescription :: a -> ByteString

instance ProgramErrorCause VariableError where
    getDescription (UndefinedVariableAssignment varName) =
            "can't assign to undefined variable " `append` varName
    getDescription (AlreadyDefinedVariable varName) =
            "variable " `append` varName `append` " already defined"

instance ProgramErrorCause EvalError where
    getDescription DivisionByZero              = "division by zero"
    getDescription (UndefinedVariable varName) =
            "can't evaluate undefined variable " `append` varName

handleProgramErrorCause :: ( MonadState' StatementLine m
                           , MonadError ProgramError m
                           , ProgramErrorCause e
                           )
                        => e -> m a
handleProgramErrorCause err = do
    line <- get'
    throwError (ProgramError (getDescription err) line)

myHandle :: ( MonadState' StatementLine m
            , MonadError ProgramError m
            , ProgramErrorCause e
            )
         => ExceptT e m a -> m a
myHandle = exceptT handleProgramErrorCause pure

runVariableStatement :: ( Integral a
                        , MonadState' (EvalContext a) m
                        , MonadState' StatementLine m
                        , MonadError ProgramError m
                        )
                     => VariableAssignment a
                     -> (ByteString -> a -> ExceptT VariableError m ())
                     -> m ()
runVariableStatement (VariableAssignment varName expr) action = do
    evalContext <- get'
    exprValue <- myHandle $ runReaderT (eval expr) evalContext
    myHandle $ action varName exprValue
    modify' @StatementLine (+1)

runStatement :: ProgramConstraint a m => Statement a -> m ()
runStatement (VariableDeclarationStatement (VariableDeclaration statement)) =
        runVariableStatement statement declareVariable
runStatement (VariableAssignmentStatement statement) =
        runVariableStatement statement updateVariable

runProgram :: ProgramConstraint a m => Program a -> m ()
runProgram = mapM_ runStatement
