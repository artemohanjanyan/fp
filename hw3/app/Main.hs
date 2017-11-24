module Main (main) where

import           Core.ProgramIO        (runProgramIO)
import           Parser.ProgramParser  (programParser)

import           System.Environment    (getArgs)

import           Data.ByteString       (readFile)

import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> putStrLn "specify input file"
        filePath:_ -> parseAndRun filePath

parseAndRun :: String -> IO ()
parseAndRun filePath = do
    fileContents <- Data.ByteString.readFile filePath
    case parse programParser filePath fileContents of
        Left err      -> putStr $ parseErrorPretty err
        Right program -> runProgramIO @Int program
