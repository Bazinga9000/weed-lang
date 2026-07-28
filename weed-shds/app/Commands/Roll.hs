module Commands.Roll (roll) where

import Data.Text qualified as T
import Discord.Interactions
import Discord.Types
import Formatting.ANSI
import Pipeline
import System.Timeout
import Eris

unwrapTimeout :: (IsString a) => Maybe (Either a b) -> Either a b
unwrapTimeout Nothing = Left $ fromString "Roll timed out (10 second limit)"
unwrapTimeout (Just a) = a

fetchUserName :: MemberOrUser -> Text
fetchUserName (MemberOrUser (Left gm)) = case memberNick gm of
  Just name -> name
  Nothing -> maybe "?????" userName (memberUser gm)
fetchUserName (MemberOrUser (Right u)) = userName u

clamp :: Text -> Maybe Text
clamp s = if T.length s >= 900 then Nothing else Just s

clampResult :: (Text, Maybe Text) -> (Maybe Text, Maybe Text)
clampResult (r, as) = (clamp r, as >>= clamp)

roll :: SlashCommand s
roll = buildCommand "roll" "Roll dice, backed by WEED." $
  rollHandler <$> arg @Text "die" "The dice expression to roll"

rollHandler :: Text -> Interaction -> ErisHandler s ()
rollHandler input intr = do
  interpreted <- unwrapTimeout <$> liftIO (timeout 1_000_000_000 $ interpret input)
  let preamble = unwords [fetchUserName (interactionUser intr) <> "'s", "roll", "`" <> input <> "`"]
  let out =
        unlines
          ( case clampResult <$> interpreted of
              Left err ->
                [ preamble <> " failed with error:",
                  "```ansi",
                  ansiFormatString Red Normal err,
                  "```"
                ]
              Right (Nothing, Nothing) ->
                [ preamble <> " failed with error:",
                  "```ansi",
                  ansiFormatString Red Normal "Discord Message would be too long",
                  "```"
                ]
              Right (Just res, Nothing) ->
                [ preamble <> " rolled:",
                  "```ansi",
                  res,
                  "```"
                ]
              Right (Nothing, Just autosum) ->
                [ preamble <> " rolled:",
                  "### " <> autosum
                ]
              Right (Just res, Just autosum) ->
                [ preamble <> " rolled:",
                  "### " <> autosum,
                  "```ansi",
                  res,
                  "```"
                ]
          )
  say intr out
