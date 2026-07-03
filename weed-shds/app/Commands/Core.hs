module Commands.Core (SlashCommand (..), updateCommandRegistrations) where

import Discord
import Discord.Interactions
import Discord.Requests qualified as R
import Discord.Types
import Formatting

data SlashCommand = SlashCommand
  { name :: Text,
    registration :: Maybe CreateApplicationCommand,
    handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

-- the full flow for updating a bot's application commands
updateCommandRegistrations :: ApplicationId -> [SlashCommand] -> DiscordHandler ()
updateCommandRegistrations appId commands = do
  echo "Registering commands..."
  regs <- mapM (register appId) commands
  let badCount = length $ filter isNothing regs
  let total = length regs
  let goodCount = total - badCount
  echo $ "(" <> show goodCount <> "/" <> show total <> ") command" <> (if goodCount == 1 then "" else "s") <> " registered"
  if badCount /= 0
    then pure ()
    else do
      echo "Unregistering outdated commands..."
      unregisterOutdatedCmds appId $ catMaybes regs
      good "Done unregistering outdated commands..."

-- register an application command.
-- Outputs status info on the registration to stdout
register :: ApplicationId -> SlashCommand -> DiscordHandler (Maybe ApplicationCommand)
register appId cmd = do
  resp <- tryRegistering cmd
  case resp of
    Left err -> do
      bad $ "  - Failed to register " <> name cmd <> ": " <> show err
      return Nothing
    Right cmd' -> do
      good $ "  - Registered " <> name cmd
      return $ Just cmd'
  where
    tryRegistering c = case registration c of
      Just reg -> restCall $ R.CreateGlobalApplicationCommand appId reg
      Nothing -> pure . Left $ RestCallErrorCode 0 "" ""

-- unregister any commands not present in the given list of commands
unregisterOutdatedCmds :: ApplicationId -> [ApplicationCommand] -> DiscordHandler ()
unregisterOutdatedCmds appId validCmds = do
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
