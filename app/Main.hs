module Main where

import Evaluator
import Formatting.ANSI
import Parser.Lexer
import Parser.Lowerer
import Parser.SurfaceParser
import Formatting.Pretty (prettyPrint)
import System.Console.Haskeline
import TypeChecker

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      let weedText = ansiFormatString Green Normal "weed> "
      minput <- getInputLine $ toString weedText
      case minput of
        Nothing -> pass
        Just "exit" -> pass
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
