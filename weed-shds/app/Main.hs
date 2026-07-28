module Main where

import Eris
import Commands

main :: IO ()
main = runEris $ Eris {
  botName = "Super Helpful Dice Simulator",
  commands = [ping, roll],
  initState = pass
  }
