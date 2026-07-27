module Eris.Commands.Arguments where

import Discord.Interactions
import Discord.Types
import Data.Scientific (Scientific, toRealFloat)
import Data.Map qualified as M

-- | Typeclass for those types which can be parsed as Discord arguments
class DiscordOption a where
  mkOption    :: Proxy a -> Text -> Text -> OptionValue
  parseOption :: Interaction -> Maybe OptionDataValue -> Either Text a

instance DiscordOption Text where
  mkOption _ name desc = OptionValueString
    name Nothing desc Nothing True (Left False) Nothing Nothing

  parseOption _ (Just (OptionDataValueString _ (Right t))) = Right t
  parseOption _ (Just _) = Left "Type mismatch: Expected a string."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption Integer where
  mkOption _ name desc = OptionValueInteger
    name Nothing desc Nothing True (Left False) Nothing Nothing

  parseOption _ (Just (OptionDataValueInteger _ (Right i))) = Right i
  parseOption _ (Just _) = Left "Type mismatch: Expected an integer."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption Bool where
  mkOption _ name desc = OptionValueBoolean
    name Nothing desc Nothing True

  parseOption _ (Just (OptionDataValueBoolean _ b)) = Right b
  parseOption _ (Just _) = Left "Type mismatch: Expected a boolean."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption UserId where
  mkOption _ name desc = OptionValueUser
    name Nothing desc Nothing True

  parseOption _ (Just (OptionDataValueUser _ u)) = Right u
  parseOption _ (Just _) = Left "Type mismatch: Expected a user id."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption User where
  mkOption _ name desc = OptionValueUser
    name Nothing desc Nothing True

  parseOption intr (Just (OptionDataValueUser _ u)) = fetch intr resolvedDataUsers u
  parseOption _ (Just _) = Left "Type mismatch: Expected a user id."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption ChannelId where
  mkOption _ name desc = OptionValueChannel
    name Nothing desc Nothing True Nothing

  parseOption _ (Just (OptionDataValueChannel _ c)) = Right c
  parseOption _ (Just _) = Left "Type mismatch: Expected a channel id."
  parseOption _ Nothing  = Left "Missing required argument."


instance DiscordOption RoleId where
  mkOption _ name desc = OptionValueRole
    name Nothing desc Nothing True

  parseOption _ (Just (OptionDataValueRole _ r)) = Right r
  parseOption _ (Just _) = Left "Type mismatch: Expected a channel id."
  parseOption _ Nothing  = Left "Missing required argument."


instance DiscordOption Role where
  mkOption _ name desc = OptionValueUser
    name Nothing desc Nothing True

  parseOption intr (Just (OptionDataValueRole _ r)) = fetch intr resolvedDataRoles r
  parseOption _ (Just _) = Left "Type mismatch: Expected a role id."
  parseOption _ Nothing  = Left "Missing required argument."

mkNumberOption :: Text -> Text -> OptionValue
mkNumberOption name desc = OptionValueNumber
  name Nothing desc Nothing True (Left False) Nothing Nothing

instance DiscordOption Scientific where
  mkOption _ = mkNumberOption

  parseOption _ (Just (OptionDataValueNumber _ (Right s))) = Right s
  parseOption _ (Just _) = Left "Type mismatch: Expected a number."
  parseOption _ Nothing  = Left "Missing required argument."


instance DiscordOption Rational where
  mkOption _ = mkNumberOption

  parseOption _ (Just (OptionDataValueNumber _ (Right s))) = Right (toRational s)
  parseOption _ (Just _) = Left "Type mismatch: Expected a number."
  parseOption _ Nothing  = Left "Missing required argument."


instance DiscordOption Double where
  mkOption _ = mkNumberOption

  parseOption _ (Just (OptionDataValueNumber _ (Right s))) = Right (toRealFloat s)
  parseOption _ (Just _) = Left "Type mismatch: Expected a number."
  parseOption _ Nothing  = Left "Missing required argument."

-- todo: figure out what to do with Mentionable and Attachment, since they both return Snowflake
-- probably just a matter of resolvedData

-- todo: use resolvedData to implement DiscordOption for the non-ID types


instance DiscordOption Attachment where
  mkOption _ name desc = OptionValueUser
    name Nothing desc Nothing True

  parseOption intr (Just (OptionDataValueAttachment _ a)) = fetch intr resolvedDataAttachments (DiscordId a)
  parseOption _ (Just _) = Left "Type mismatch: Expected an attachment id."
  parseOption _ Nothing  = Left "Missing required argument."

instance DiscordOption a => DiscordOption (Maybe a) where
  mkOption _ name desc = mkOptional (mkOption (Proxy @a) name desc) where
    mkOptional :: OptionValue -> OptionValue
    mkOptional (OptionValueString a b c d _ f g h) = OptionValueString a b c d False f g h
    mkOptional (OptionValueInteger a b c d _ f g h) = OptionValueInteger a b c d False f g h
    mkOptional (OptionValueBoolean a b c d _) = OptionValueBoolean a b c d False
    mkOptional (OptionValueUser a b c d _) = OptionValueUser a b c d False
    mkOptional (OptionValueChannel a b c d _ f) = OptionValueChannel a b c d False f
    mkOptional (OptionValueRole a b c d _) = OptionValueRole a b c d False
    mkOptional (OptionValueMentionable a b c d _) = OptionValueMentionable a b c d False
    mkOptional (OptionValueNumber a b c d _ f g h) = OptionValueNumber a b c d False f g h
    mkOptional (OptionValueAttachment a b c d _) = OptionValueAttachment a b c d False

  parseOption intr (Just odv) = Just <$> parseOption intr (Just odv)
  parseOption _ Nothing = return Nothing



fetch :: Interaction -> (ResolvedData -> Maybe (Map Snowflake a)) -> DiscordId b -> Either Text a
fetch intr mapGet discordId = do
    userMap <- mapGet <$> getResolvedData intr
    let resolvedU = do
         m <- userMap
         M.lookup (unId discordId) m

    case resolvedU of
      Just datum -> Right datum
      Nothing -> Left $ "Could not find " <> show discordId <> " in ResolvedData"

getResolvedData :: Interaction -> Either Text ResolvedData
getResolvedData = \case
  (InteractionApplicationCommand _ _ appCmdData _ _ _ _ _ _ _ _) -> case appCmdData of
    (ApplicationCommandDataUser _ _ rd _) -> unwrapResolvedData rd
    (ApplicationCommandDataMessage _ _ rd _) -> unwrapResolvedData rd
    (ApplicationCommandDataChatInput _ _ rd _) -> unwrapResolvedData rd
  _ -> Left "getResolvedData got wrong Interaction type, expected InteractionApplicationCommand"
  where
    unwrapResolvedData Nothing = Left "Command had no ResolvedData"
    unwrapResolvedData (Just r) = Right r
