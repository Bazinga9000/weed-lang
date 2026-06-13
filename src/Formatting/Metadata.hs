module Formatting.Metadata where

import Evaluator.Metadata
import Formatting.ANSI

colorMetadata :: NumberMetadata -> Maybe ANSIForegroundColor
colorMetadata m
  | hasMark (m ^. critLevel) && hasMark (m ^. failLevel) = Just Yellow
  | hasMark (m ^. critLevel) = Just Green
  | hasMark (m ^. failLevel) = Just Red
  | hasMark (m ^. extraDice) = Just Blue
  | otherwise = Nothing

markMetadata :: NumberMetadata -> Text
markMetadata m = critSuccessMark <> critFailMark <> extraMark
  where
    critSuccessMark = mkMark ("☆", "★") (m ^. critLevel)
    critFailMark = mkMark ("†", "‡") (m ^. failLevel)
    extraMark = mkMark ("!", "!") (m ^. extraDice)
