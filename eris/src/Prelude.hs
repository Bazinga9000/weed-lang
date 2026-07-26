module Prelude
  ( module Relude,
    module Relude.Extra.Bifunctor,
    echo, good, bad
  )
where

import Relude
import Relude.Extra.Bifunctor

echo :: (MonadIO m) => Text -> m ()
echo = liftIO . putTextLn

bad :: (MonadIO m) => Text -> m ()
bad t = echo $ "\ESC[31;0m" <> t <> "\ESC[0m"

good :: (MonadIO m) => Text -> m ()
good t = echo $ "\ESC[32;0m" <> t <> "\ESC[0m"
