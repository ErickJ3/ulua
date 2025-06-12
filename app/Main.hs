module Main where

import Evaluator
import Parser
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    _ -> putStrLn "usage: cabal run ulua -- <file.ulua>"

runFile :: String -> IO ()
runFile filename = do
  parseResult <- parseFile filename
  case parseResult of
    Left err -> putStrLn $ errorBundlePretty err
    Right block -> do
      evalOutcome <- runEval (evalBlock block)
      case evalOutcome of
        Left evalErr -> putStrLn $ "Execution error: " ++ formatError evalErr
        Right ((), finalEnv) -> do
          putStrLn "Execution completed successfully."
          putStrLn "Final environment:"
          print finalEnv

formatError :: EvalError -> String
formatError (VarNotFound var) = "Variable not found: " ++ var
formatError (TypeMismatch msg) = "Type mismatch: " ++ msg
formatError DivisionByZero = "Division by zero!"