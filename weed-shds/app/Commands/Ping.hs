module Commands.Ping where

import Eris
import Discord.Interactions

ping :: SlashCommand s
ping = buildCommand "ping" "responds pong" $ pure pingHandler

pingHandler :: Interaction -> ErisHandler s ()
pingHandler intr = say intr "pong"
