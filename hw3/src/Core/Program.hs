module Core.Program
    ( Program
    , Statement (..)
    , ProgramError (..)
    , StatementLine
    , ProgramErrorCause (..)
    , ProgramConstraint
    , runProgram
    ) where

import           Core.Expr                  (EvalContext, EvalError (..), Expr, eval)
import           Core.Variables             (VariableAssignment (..),
                                             VariableDeclaration (..), VariableError (..),
                                             VariableName, declareVariable,
                                             updateVariable)

import           Data.ByteString            (ByteString, append)
import           Data.Void                  (Void)

import           Control.Monad              (forM_, mapM_)
import           Control.Monad.Except       (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (runReaderT)

import           Control.Error.Util         (exceptT)
import           Ether.State                (MonadState', get', modify', put')

import           Text.Megaparsec            (Parsec, parse)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Text.Megaparsec.Error      (parseErrorPretty)

type ProgramConstraint a m =
    ( Show a
    , Integral a
    , MonadState' (EvalContext a) m
    , MonadState' StatementLine m
    , MonadError ProgramError m
    , MonadIO m
    )

type Program a = [Statement a]

data Statement a
    = VariableDeclarationStatement (VariableDeclaration a)
    | VariableAssignmentStatement (VariableAssignment a)
    | PrintStatement (Expr a)
    | ReadStatement VariableName
    | ForStatement VariableName (Expr a) (Expr a) (Program a)
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
                     -> (VariableName -> a -> ExceptT VariableError m ())
                     -> m ()
runVariableStatement (VariableAssignment varName expr) action = do
    evalContext <- get'
    exprValue <- myHandle $ runReaderT (eval expr) evalContext
    myHandle $ action varName exprValue
    modify' @StatementLine (+1)

runStatement :: forall a m . ProgramConstraint a m => Statement a -> m ()
runStatement (VariableDeclarationStatement (VariableDeclaration statement)) =
        runVariableStatement statement declareVariable
runStatement (VariableAssignmentStatement statement) =
        runVariableStatement statement updateVariable
runStatement (PrintStatement expr) = do
    evalContext <- get'
    exprValue <- myHandle $ runReaderT (eval expr) evalContext
    liftIO $ print exprValue
    modify' @StatementLine (+1)
runStatement (ReadStatement varName) = do
    input <- liftIO getLine
    case parse inputParser "input" input of
        Left err    -> liftIO $ putStr $ parseErrorPretty err
        Right value -> myHandle $ updateVariable varName value
    modify' @StatementLine (+1)
  where
    inputParser :: Parsec Void String a
    inputParser = signed space decimal
runStatement (ForStatement varName fromExpr toExpr body) = do
    evalContext <- get'
    fromValue <- myHandle $ runReaderT (eval fromExpr) evalContext
    toValue <- myHandle $ runReaderT (eval toExpr) evalContext

    modify' @StatementLine (+1)
    forLine <- get' @StatementLine

    forM_ [fromValue..toValue] $ \varValue -> do
        myHandle $ updateVariable varName varValue
        put' forLine
        runProgram body

runProgram :: ProgramConstraint a m => Program a -> m ()
runProgram = mapM_ runStatement
