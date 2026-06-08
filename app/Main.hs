module Main where

import Evaluator
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import PrettyPrint
import System.Console.Haskeline
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
    Left e -> putTextLn $ "Lowering error: " <> show e
    Right coreu -> case typeCheck coreu of
      Left e -> putTextLn $ "Type error: " <> show e
      Right coret -> do
        ev <- evaluate coret
        case ev of
          Left err -> putTextLn $ "Evaluation error: " <> prettyPrint err
          Right v -> putTextLn $ prettyPrint v
