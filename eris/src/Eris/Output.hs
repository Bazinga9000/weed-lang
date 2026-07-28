module Eris.Output where

import Discord
import Discord.Interactions
import Discord.Requests qualified as R
import Eris.Handler

-- Send a single message in response to a command, with no fanfare
say :: Interaction -> Text -> ErisHandler s ()
say intr msg = void . liftDiscord . restCall $
  R.CreateInteractionResponse
    (interactionId intr)
    (interactionToken intr)
    (interactionResponseBasic msg)
