module Main where

import Control.Monad (forever)
import Evaluator
import Evaluator.Types (displayError, displayObservable)
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import TypeChecker

main :: IO ()
main = forever $ do
  putStr "weed> "
  hFlush stdout
  input <- getLine
  if input == "exit" then exitSuccess else repl input

repl :: String -> IO ()
repl input = do
  let toks = scanTokens input
  let surface = parseSurface toks

  case lower surface of
    Left e -> putStrLn $ "Lowering error: " ++ show e
    Right coreu -> case typeCheck coreu of
      Left e -> putStrLn $ "Type error: " ++ show e
      Right coret -> do
        ev <- evaluate coret
        case ev of
          Left err -> putStrLn $ "Evaluation error: " ++ displayError err
          Right v -> putStrLn $ displayObservable v
