module Main where

import Commands
import Discord
import Discord.Interactions
import Discord.Types
import Formatting
import System.Directory

main :: IO ()
main = do
  token <- fetchTokenOrError

  echo "Starting bot..."
  botTerminationError <-
    runDiscord $
      def
        { discordToken = token,
          discordOnEvent = onDiscordEvent,
          discordGatewayIntent = def {gatewayIntentMessageContent = False},
          discordOnLog = \s -> putTextLn s >> putTextLn ""
        }

  bad "A fatal error occurred: "
  bad botTerminationError
  exitFailure

commands :: [SlashCommand]
commands = [ping, roll]

onDiscordEvent :: Event -> DiscordHandler ()
onDiscordEvent = \case
  Ready _ user _ _ _ _ (PartialApplication appId _) -> onReady appId user
  InteractionCreate intr -> onInteractionCreate intr
  _ -> pass

onReady :: ApplicationId -> User -> DiscordHandler ()
onReady appId user = do
  good "WEED-SHDS - Bot ready!"
  echo $ "App ID: " <> show appId
  echo $ "User: " <> userName user
  updateCommandRegistrations appId commands

onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {}
    } ->
      let inputName = applicationCommandDataName input
       in case find (\c -> inputName == name c) commands of
            Just found ->
              handler found cmd (optionsData input)
            Nothing ->
              bad $ "Got unknown slash command " <> inputName <> " (registrations out of date?)"
  _ ->
    pass -- Unexpected/unsupported interaction type

fetchTokenOrError :: IO Text
fetchTokenOrError = do
  mTok <- fetchToken
  case mTok of
    Just t -> return t
    Nothing -> do
      bad "No Token Found. Resolve this by:"
      bad "    (a) Setting TOKEN_FILE to a path to a file containing your token"
      bad "    (b) Setting TOKEN to your raw token"
      exitFailure

fetchToken :: IO (Maybe Text)
fetchToken = do
  tokenFile <- lookupEnv "TOKEN_FILE"
  case tokenFile of
    Nothing -> fetchTokenText
    Just filePath -> do
      e <- doesFileExist filePath
      if not e
        then fetchTokenText
        else do
          s <- decodeUtf8 <$> readFileBS filePath
          return $ Just s

fetchTokenText :: IO (Maybe Text)
fetchTokenText = do
  rawToken <- lookupEnv "TOKEN"
  case rawToken of
    Nothing -> return Nothing
    Just s -> return . Just . fromString $ s
