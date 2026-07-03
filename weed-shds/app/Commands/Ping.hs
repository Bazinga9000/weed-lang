module Commands.Ping where

import Commands.Core
import Discord
import Discord.Interactions
import Discord.Requests qualified as R

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
