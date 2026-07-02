module Main where

import Discord
import Discord.Interactions
import Discord.Requests qualified as R
import Discord.Types
import Formatting.ANSI
import System.Directory
import System.Environment

echo :: (MonadIO m) => Text -> m ()
echo = liftIO . putTextLn

bad :: (MonadIO m) => Text -> m ()
bad = echo . ansiFormatString Red Normal

good :: (MonadIO m) => Text -> m ()
good = echo . ansiFormatString Green Normal

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

data SlashCommand = SlashCommand
  { name :: Text,
    registration :: Maybe CreateApplicationCommand,
    handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

commands :: [SlashCommand]
commands = [ping]

ping :: SlashCommand
ping =
  SlashCommand
    { name = "ping",
      registration = createChatInput "ping" "responds pong",
      handler = \intr _options ->
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic "pong")
    }

onDiscordEvent :: Event -> DiscordHandler ()
onDiscordEvent = \case
  Ready _ user _ _ _ _ (PartialApplication appId _) -> onReady appId user
  InteractionCreate intr -> onInteractionCreate intr
  _ -> pure ()

onReady :: ApplicationId -> User -> DiscordHandler ()
onReady appId user = do
  good $ "WEED-SHDS - Bot ready!"
  good $ "App ID: " <> show appId
  good $ "User: " <> userName user
  echo "Registering commands..."
  regs <- mapM register commands
  if Nothing `elem` regs
    then pure ()
    else do
      good $ "All commands (" <> show (length regs) <> ") registered!"
      echo "Unregistering outdated commands..."
      unregisterOutdatedCmds $ catMaybes regs
      good "Done unregistering outdated commands..."
  where
    register cmd = do
      resp <- tryRegistering cmd
      case resp of
        Left err -> do
          bad $ "Failed to register " <> name cmd <> ": " <> show err
          return Nothing
        Right cmd' -> do
          good $ "Registered " <> name cmd
          return $ Just cmd'

    tryRegistering cmd = case registration cmd of
      Just reg -> restCall $ R.CreateGlobalApplicationCommand appId reg
      Nothing -> pure . Left $ RestCallErrorCode 0 "" ""

    unregisterOutdatedCmds validCmds = do
      registered <- restCall $ R.GetGlobalApplicationCommands appId
      case registered of
        Left err ->
          bad $ "Failed to get registered slash commands: " <> show err
        Right cmds ->
          let validIds = map applicationCommandId validCmds
              outdatedIds =
                filter (`notElem` validIds)
                  . map applicationCommandId
                  $ cmds
           in forM_ outdatedIds $
                restCall . R.DeleteGlobalApplicationCommand appId

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
    pure () -- Unexpected/unsupported interaction type

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
  tokenFile <- System.Environment.lookupEnv "TOKEN_FILE"
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
  rawToken <- System.Environment.lookupEnv "TOKEN"
  case rawToken of
    Nothing -> return Nothing
    Just s -> return . Just . fromString $ s
