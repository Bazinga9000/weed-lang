module Main where

import Formatting.ANSI
import Pipeline
import System.Console.Haskeline

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
          interpreted <- liftIO $ interpret input
          putTextLn $ case interpreted of
            Left err -> ansiFormatString Red Normal err
            Right (res, _) -> res -- the repl doesn't autosum, here we assume you're doing it manually
          loop
