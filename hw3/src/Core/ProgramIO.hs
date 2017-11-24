{-# LANGUAGE ScopedTypeVariables #-}
module Core.ProgramIO
    ( runProgramIO
    , Program
    ) where

import           Core.Expr             (EvalContext)
import           Core.Program          (Program, ProgramError (..), StatementLine,
                                        runProgram)

import           Control.Monad.Except  (runExceptT)
import qualified Data.ByteString.Char8 (putStrLn)
import qualified Data.Map.Strict       as Map
import           Ether.State           (runStateT')

runProgramIO :: forall a . Integral a => Program a -> IO ()
runProgramIO program = do
    let withStatementLine = runStateT' @StatementLine (runProgram program) 1
    let withEvalContext = runStateT' @(EvalContext a) withStatementLine Map.empty
    result <- runExceptT withEvalContext
    case result of
        Left err -> do
            putStrLn $ "Error at statement " ++ show (programErrorLine err) ++ ":"
            Data.ByteString.Char8.putStrLn $ programErrorDescription err
        Right _ -> pure ()
