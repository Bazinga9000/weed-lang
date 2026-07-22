module Formatting.ANSI (ANSIForegroundColor (..), ANSIWeight (..), ansiFormatString, clearANSI, overrideANSI) where
import Data.Text qualified as T

--- right now, only supports basic color codes,
--- since Discord is an intended frontend, and Discord only
--- supports bare ANSI, foreground colors only.

--- rolling my own ANSI instead of doing it through a fancy library is
--- also for Discord reasons

ansiEscape :: Text
ansiEscape = one '\ESC'

ansiReset :: Text
ansiReset = ansiEscape <> "[0m"

data ANSIForegroundColor
  = Gray
  | Red
  | Green
  | Yellow
  | Blue
  | Pink
  | Cyan
  | White
  deriving (Show, Eq, Ord)

ansiFCValue :: ANSIForegroundColor -> Int
ansiFCValue Gray = 30
ansiFCValue Red = 31
ansiFCValue Green = 32
ansiFCValue Yellow = 33
ansiFCValue Blue = 34
ansiFCValue Pink = 35
ansiFCValue Cyan = 36
ansiFCValue White = 37

-- todo: maybe make this less jank
data ANSIWeight = Normal | Bold | Underline | BoldUnderline deriving (Show, Eq, Ord)

ansiWCode :: ANSIWeight -> Text
ansiWCode Normal = "0"
ansiWCode Bold = "1"
ansiWCode Underline = "4"
ansiWCode BoldUnderline = "1;4"

mkANSIString :: ANSIForegroundColor -> ANSIWeight -> Text
mkANSIString fg w = ansiEscape <> "[" <> ansiWCode w <> ";" <> (show . ansiFCValue) fg <> "m"

ansiFormatString :: ANSIForegroundColor -> ANSIWeight -> Text -> Text
ansiFormatString fg w t = mkANSIString fg w <> t <> ansiReset

clearANSI :: Text -> Text
clearANSI txt =
  let (before, after) = T.break (== '\ESC') txt
  in if T.null after
     then before
     else before <> clearANSI (dropANSISequence after)
  where
    dropANSISequence :: Text -> Text
    dropANSISequence t
      | "\ESC[" `T.isPrefixOf` t =
          -- drop \ESC[" then drop until we hit the terminating char.
          -- CSI terminators are in the ASCII range 0x40-0x7E ('@' to '~').
          let (_, remainder) = T.break (\c -> c >= '\x40' && c <= '\x7e') (T.drop 2 t)
          in T.drop 1 remainder -- drop the terminator
      | otherwise = T.drop 1 t -- solitary \ESC, just drop it

overrideANSI :: ANSIForegroundColor -> ANSIWeight -> Text -> Text
overrideANSI f w t = ansiFormatString f w $ clearANSI t
