module Eris.Handler (ErisHandler(..), runErisHandler, liftDiscord, MonadBotState(..)) where

import Discord
import Control.Monad.Reader (mapReaderT)


-- discord-haskell prefers using TVars or similar to handle state throughout commands
-- however, this is a bit unergonomic, so Eris abstracts this
-- into a standard state monad and handles the TVar internally
--
-- NB: ErisHandler provides a MonadState instance for compatability. However,
-- as this is backed by a TVar, sequences of get/put are
-- not transactional. For guaranteed atomic operations, use the MonadBotState
-- instance instead.
newtype ErisHandler s a = ErisHandler
  { unErisHandler :: ReaderT (TVar s) DiscordHandler a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

runErisHandler :: TVar s -> ErisHandler s a -> DiscordHandler a
runErisHandler tv (ErisHandler m) = runReaderT m tv

liftDiscord :: DiscordHandler a -> ErisHandler s a
liftDiscord = ErisHandler . lift

instance MonadReader DiscordHandle (ErisHandler s) where
    ask =
        ErisHandler $ lift ask

    local f (ErisHandler m) =
        ErisHandler $
            mapReaderT (local f) m



stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar tv f = do
  s <- readTVar tv
  let (a, s') = f s
  writeTVar tv s'
  pure a

instance MonadState s (ErisHandler s) where
    get = ErisHandler $ do
        tv <- ask
        liftIO $ readTVarIO tv

    put s = ErisHandler $ do
        tv <- ask
        liftIO . atomically $
            writeTVar tv s

    state f = ErisHandler $ do
        tv <- ask
        liftIO . atomically $
            stateTVar tv f

class Monad m => MonadBotState s m where
    getBotState    :: m s
    putBotState    :: s -> m ()
    modifyBotState :: (s -> s) -> m ()

    stateBot       :: (s -> (a, s)) -> m a


instance MonadBotState s (ErisHandler s) where
  getBotState = get
  putBotState = put

  modifyBotState f = ErisHandler $ do
      tv <- ask
      liftIO . atomically $
          modifyTVar' tv f

  stateBot f = ErisHandler $ do
      tv <- ask
      liftIO . atomically $
          stateTVar tv f
