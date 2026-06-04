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
  let coreu = case lower surface of
        (Right cue) -> cue
        (Left e) -> error $ show e
  let coret = case typeCheck coreu of
        (Right cte) -> cte
        (Left e) -> error $ show e

  ev <- evaluate coret
  case ev of
    (Right v) -> putStrLn $ displayObservable v
    (Left err) -> error $ displayError err
