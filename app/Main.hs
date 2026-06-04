module Main where

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import Evaluator
import Evaluator.Types (displayError, displayObservable)
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import TypeChecker

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "weed> "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          liftIO $ repl input
          loop

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
