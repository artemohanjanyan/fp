{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Core.Program
    ( Program
    , Statement (..)
    , ProgramError (..)
    , StatementLine
    , ProgramErrorCause (..)
    , ProgramConstraint
    , ProgramMonad (..)
    , executeProgram
    , executeProgramWithInitialCC
    ) where

import           Prelude                    hiding (break)

import           Core.Expr                  (EvalContext, EvalError (..), Expr, eval)
import           Core.Variables             (VariableAssignment (..),
                                             VariableDeclaration (..), VariableError (..),
                                             VariableName, declareVariable,
                                             updateVariable)

import           Data.ByteString            (ByteString, append, getLine)
import           Data.Void                  (Void)

import           Control.Monad              (forM_, join, mapM_)
import           Control.Monad.Cont         (ContT, MonadCont, callCC)
import           Control.Monad.Except       (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, local, runReaderT)

import           Control.Error.Util         (exceptT)
import           Ether.State                (MonadState, MonadState', StateT', get, get',
                                             modify', put, put')

import           Text.Megaparsec            (Parsec, parse)
import           Text.Megaparsec.Byte       (space)
import           Text.Megaparsec.Byte.Lexer (decimal, signed)
import           Text.Megaparsec.Error      (parseErrorPretty)

type IntegralConstraint a =
    ( Integral a
    , Show a
    )

type ProgramConstraint a m =
    ( IntegralConstraint a
    , MonadState' (EvalContext a) m
    , MonadState' StatementLine m
    , MonadError ProgramError m
    , MonadCont m
    , MonadReader (m ()) m
    , MonadIO m
    )

newtype ProgramMonad a b = ProgramMonad
    {
        runProgramMonad ::
        (StateT' (EvalContext a)
            (StateT' StatementLine
                (ExceptT ProgramError
                    (ContT (Either ProgramError
                                   (((), EvalContext a), StatementLine)
                           )
                        (ReaderT (ProgramMonad a ())
                            IO
                        )
                    )
                )
            )
        )
        b
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError ProgramError,
              MonadCont, MonadReader (ProgramMonad a ()))

instance MonadState (EvalContext a) (EvalContext a) (ProgramMonad a) where
    get = ProgramMonad get'
    put = ProgramMonad . put'

instance MonadState StatementLine StatementLine (ProgramMonad a) where
    get = ProgramMonad get'
    put = ProgramMonad . put'

type Program a = [Statement a]

data Statement a
    = VariableDeclarationStatement (VariableDeclaration a)
    | VariableAssignmentStatement (VariableAssignment a)
    | PrintStatement (Expr a)
    | ReadStatement VariableName
    | ForStatement VariableName (Expr a) (Expr a) (Program a)
    | BreakStatement
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

executeVariableStatement :: ( Integral a
                        , MonadState' (EvalContext a) m
                        , MonadState' StatementLine m
                        , MonadError ProgramError m
                        )
                     => VariableAssignment a
                     -> (VariableName -> a -> ExceptT VariableError m ())
                     -> m ()
executeVariableStatement (VariableAssignment varName expr) action = do
    evalContext <- get'
    exprValue <- myHandle $ runReaderT (eval expr) evalContext
    myHandle $ action varName exprValue
    modify' @StatementLine (+1)

executeStatement :: forall a . (IntegralConstraint a)
                 => Statement a -> ProgramMonad a ()
executeStatement (VariableDeclarationStatement (VariableDeclaration statement)) =
        executeVariableStatement statement declareVariable
executeStatement (VariableAssignmentStatement statement) =
        executeVariableStatement statement updateVariable
executeStatement (PrintStatement expr) = do
    evalContext <- get'
    exprValue <- myHandle $ runReaderT (eval expr) evalContext
    liftIO $ print exprValue
    modify' @StatementLine (+1)
executeStatement (ReadStatement varName) = do
    input <- liftIO Data.ByteString.getLine
    case parse inputParser "input" input of
        Left err    -> liftIO $ putStr $ parseErrorPretty err
        Right value -> myHandle $ updateVariable varName value
    modify' @StatementLine (+1)
  where
    inputParser :: Parsec Void ByteString a
    inputParser = signed space decimal
executeStatement (ForStatement varName fromExpr toExpr body) =
        callCC $ \break -> do
    evalContext <- get'
    fromValue <- myHandle $ runReaderT (eval fromExpr) evalContext
    toValue <- myHandle $ runReaderT (eval toExpr) evalContext

    modify' @StatementLine (+1)
    forLine <- get' @StatementLine

    local (const $ break ()) $ forM_ [fromValue..toValue] $ \varValue -> do
        myHandle $ updateVariable varName varValue
        put' forLine
        executeProgram body
executeStatement BreakStatement = join ask

executeProgram :: (IntegralConstraint a) => Program a -> ProgramMonad a ()
executeProgram = mapM_ executeStatement

executeProgramWithInitialCC :: (IntegralConstraint a)
                            => Program a -> ProgramMonad a ()
executeProgramWithInitialCC program = callCC $ \break ->
        local (const $ break ()) $ executeProgram program
