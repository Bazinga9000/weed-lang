module Eris.Commands (arg, buildCommand) where

import Discord
import Discord.Interactions
import Discord.Requests qualified as R
import Eris.Commands.Core
import Eris.Commands.Arguments
import Relude.Unsafe (fromJust)

-- | Extracts the name from any Discord option payload
getOptionName :: OptionDataValue -> Text
getOptionName = \case
  OptionDataValueString n _      -> n
  OptionDataValueInteger n _     -> n
  OptionDataValueBoolean n _     -> n
  OptionDataValueUser n _        -> n
  OptionDataValueChannel n _     -> n
  OptionDataValueRole n _        -> n
  OptionDataValueMentionable n _ -> n
  OptionDataValueNumber n _      -> n
  OptionDataValueAttachment n _  -> n

-- | Defines a single argument for a command
arg :: forall a. DiscordOption a => Text -> Text -> CommandDef a
arg name desc = CommandDef [opt] (OptParser parser)
  where
    opt = mkOption (Proxy @a) name desc
    parser intr vals = parseOption intr (find (\v -> getOptionName v == name) vals)

-- | Safely attaches options to a ChatInput command
addOptions :: [OptionValue] -> CreateApplicationCommand -> CreateApplicationCommand
addOptions [] c = c
addOptions opts c@CreateApplicationCommandChatInput{} =
  c { createOptions = Just (OptionsValues opts) }
addOptions _ c = c

sendParseError :: Interaction -> Text -> DiscordHandler ()
sendParseError intr msg = void . restCall $
  R.CreateInteractionResponse
    (interactionId intr)
    (interactionToken intr)
    (interactionResponseBasic $ "Command Error: " <> msg)

-- | Applicative builder for commands
buildCommand :: Text -> Text -> CommandDef (Interaction -> DiscordHandler ()) -> SlashCommand
buildCommand name desc (CommandDef opts (OptParser parser)) = SlashCommand
  { name = name
  , registration = Just $ addOptions opts (fromJust $ createChatInput name desc) --TODO: avoid the unsafe fromJust here
  , handler = \intr mOpts -> do
      let vals = case mOpts of
            Just (OptionsDataValues vs) -> vs
            _ -> []
      case parser intr vals of
        Left err -> sendParseError intr err
        Right handlerFunc -> handlerFunc intr
  }
