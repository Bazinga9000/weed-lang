module Eris.Commands.Core where
import Discord
import Discord.Interactions

-- | The full representation of a slash command
data SlashCommand = SlashCommand
  { name :: Text,
    registration :: Maybe CreateApplicationCommand,
    handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

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
