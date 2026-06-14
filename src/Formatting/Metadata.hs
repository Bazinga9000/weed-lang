module Formatting.Metadata where

import Control.Lens
import Evaluator.Metadata
import Formatting.ANSI

colorMetadata :: NumberMetadata -> Maybe ANSIForegroundColor
colorMetadata m
  | m ^. dropped = Just Gray
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

formatWithMetadata :: NumberMetadata -> Text -> Text
formatWithMetadata md inText = case colorMetadata md of
  Nothing -> marked
  Just c -> ansiFormatString c Normal marked
  where
    marked = inText <> markMetadata md
