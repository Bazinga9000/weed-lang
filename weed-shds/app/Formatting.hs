module Formatting where

import Formatting.ANSI

echo :: (MonadIO m) => Text -> m ()
echo = liftIO . putTextLn

bad :: (MonadIO m) => Text -> m ()
bad = echo . ansiFormatString Red Normal

good :: (MonadIO m) => Text -> m ()
good = echo . ansiFormatString Green Normal
