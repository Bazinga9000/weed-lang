module Eris.Commands.Core where
import Discord.Interactions
import Eris.Handler

-- | The full representation of a slash command
data SlashCommand s = SlashCommand
  { scName :: Text,
    scRegistration :: Maybe CreateApplicationCommand,
    scHandler :: Interaction -> Maybe OptionsData -> ErisHandler s ()
  }

name :: SlashCommand s -> Text
name = scName

registration :: SlashCommand s -> Maybe CreateApplicationCommand
registration = scRegistration

handler :: SlashCommand s -> Interaction -> Maybe OptionsData -> ErisHandler s ()
handler = scHandler

-- | A parser that extracts values from Discord's interaction data.
newtype OptParser a = OptParser { runOptParser :: Interaction -> [OptionDataValue] -> Either Text a }
  deriving Functor

instance Applicative OptParser where
  pure x = OptParser (\_ _ -> Right x)
  OptParser pf <*> OptParser px = OptParser $ \intr vals -> do
    f <- pf intr vals
    x <- px intr vals
    return (f x)

-- | Pairs the required Discord registration options with the parser.
data CommandDef a = CommandDef
  { cmdOptions :: [OptionValue]
  , cmdParser  :: OptParser a
  } deriving Functor

instance Applicative CommandDef where
  pure x = CommandDef [] (pure x)
  CommandDef opts1 p1 <*> CommandDef opts2 p2 =
    CommandDef (opts1 <> opts2) (p1 <*> p2)
