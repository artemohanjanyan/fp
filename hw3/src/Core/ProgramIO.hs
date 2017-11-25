module Core.ProgramIO
    ( runProgramIO
    , Program
    ) where

import           Prelude               hiding (break)

import           Core.Expr             (EvalContext)
import           Core.Program          (Program, ProgramError (..), ProgramMonad (..),
                                        StatementLine, runProgramWithInitialCC)

import           Control.Monad.Cont    (runContT)
import           Control.Monad.Except  (runExceptT)
import           Control.Monad.Reader  (runReaderT)
import qualified Data.ByteString.Char8 (putStrLn)
import qualified Data.Map.Strict       as Map
import           Ether.State           (runStateT')

runProgramIO :: forall a . (Show a, Integral a) => Program a -> IO ()
runProgramIO program = do
    let withCC = runProgramMonad $ runProgramWithInitialCC program
    let withEvalContext = runStateT' @(EvalContext a) withCC Map.empty
    let withStatementLine = runStateT' @StatementLine withEvalContext 1
    let withExcept = runExceptT withStatementLine
    let withMonadCont = runContT withExcept pure
    let withReader = runReaderT withMonadCont (pure ())
    result <- withReader
    case result of
        Left err -> do
            putStrLn $ "Error at statement " ++ show (programErrorLine err) ++ ":"
            Data.ByteString.Char8.putStrLn $ programErrorDescription err
        Right _ -> pure ()
