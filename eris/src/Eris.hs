module Eris (Eris(..), runEris) where

import Eris.Commands
import Eris.Token
import Eris.Handler
import Discord
import Discord.Interactions
import Discord.Types


data Eris s = Eris {
  botName :: Text,
  commands :: [SlashCommand s],
  initState :: IO s
  }

runEris :: Eris a -> IO ()
runEris eris = do
  echo "Fetching Token..."
  token <- fetchToken

  echo "Initializing State..."
  stateTVar <- (initState eris) >>= newTVarIO

  echo "Starting Bot..."

  botTerminationError <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = onDiscordEvent eris stateTVar,
          discordGatewayIntent = def {gatewayIntentMessageContent = False},
          discordOnLog = \s -> putTextLn s >> putTextLn ""
        }

  bad "A fatal error occurred: "
  bad botTerminationError
  exitFailure

onDiscordEvent :: Eris s -> TVar s -> Event -> DiscordHandler ()
onDiscordEvent eris tvar = \case
  Ready _ user _ _ _ _ (PartialApplication appId _) -> onReady eris appId user
  InteractionCreate intr -> onInteractionCreate eris tvar intr
  _ -> pass

onReady :: Eris s -> ApplicationId -> User -> DiscordHandler ()
onReady eris appId user = do
  good $ botName eris <> " - Bot ready!"
  echo $ "App ID: " <> show appId
  echo $ "User: " <> userName user
  updateCommandRegistrations appId (commands eris)

onInteractionCreate :: Eris s -> TVar s -> Interaction -> DiscordHandler ()
onInteractionCreate eris tvar cmd = case cmd of
  InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {}
    } ->
      let inputName = applicationCommandDataName input
       in case find (\c -> inputName == name c) (commands eris) of
            Just found ->
              runErisHandler tvar $ handler found cmd (optionsData input)
            Nothing ->
              bad $ "Got unknown slash command " <> inputName <> " (registrations out of date?)"
  _ ->
    pass -- Unexpected/unsupported interaction type
