module Core.ProgramIO
    ( runProgramIO
    , Program
    ) where

import           Prelude               hiding (break)

import           Core.Expr             (EvalContext)
import           Core.Program          (Program, ProgramError (..), StatementLine,
                                        runProgram)

import           Control.Monad.Cont    (callCC, runContT)
import           Control.Monad.Except  (runExceptT)
import qualified Data.ByteString.Char8 (putStrLn)
import qualified Data.Map.Strict       as Map
import           Ether.State           (runStateT')

runProgramIO :: forall a . (Show a, Integral a) => Program a -> IO ()
runProgramIO program = do
    let withCC = callCC $ \break -> runProgram (break ()) program
    let withStatementLine = runStateT' @StatementLine withCC 1
    let withEvalContext = runStateT' @(EvalContext a) withStatementLine Map.empty
    let withExcept = runExceptT withEvalContext
    let withMonadCont = runContT withExcept pure
    result <- withMonadCont
    case result of
        Left err -> do
            putStrLn $ "Error at statement " ++ show (programErrorLine err) ++ ":"
            Data.ByteString.Char8.putStrLn $ programErrorDescription err
        Right _ -> pure ()
