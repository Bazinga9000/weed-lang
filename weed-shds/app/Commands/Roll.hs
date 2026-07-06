module Commands.Roll (roll) where

import Commands.Core
import Data.Text qualified as T
import Discord
import Discord.Interactions
import Discord.Requests qualified as R
import Discord.Types
import Formatting
import Formatting.ANSI
import Pipeline
import System.Timeout

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

roll :: SlashCommand
roll =
  SlashCommand
    { name = "roll",
      registration =
        createChatInput "roll" "Roll dice, backed by WEED." >>= \case
          c@CreateApplicationCommandChatInput {} ->
            Just $
              c
                { createOptions =
                    Just $
                      OptionsValues
                        [ OptionValueString
                            { optionValueName = "die",
                              optionValueLocalizedName = Nothing,
                              optionValueDescription = "The dice expression to roll",
                              optionValueLocalizedDescription = Nothing,
                              optionValueRequired = True,
                              optionValueStringChoices = Left False,
                              optionValueStringMinLen = Nothing,
                              optionValueStringMaxLen = Nothing
                            }
                        ]
                }
          _ -> error "impossible, createChatInput always makes a ChatInput",
      handler = \intr -> \case
        Just (OptionsDataValues [OptionDataValueString "die" (Right input)]) ->
          do
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

            void . restCall $
              R.CreateInteractionResponse
                (interactionId intr)
                (interactionToken intr)
                (interactionResponseBasic out)
        e -> bad $ "roll recieved bad option values: " <> show e
    }
