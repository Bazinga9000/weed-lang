module Eris.Token (fetchToken) where
import System.Directory

fetchToken :: IO Text
fetchToken = do
  mTok <- fetchTokenMaybe
  case mTok of
    Just t -> return t
    Nothing -> do
      bad "No Token Found. Resolve this by:"
      bad "    (a) Setting TOKEN_FILE to a path to a file containing your token"
      bad "    (b) Setting TOKEN to your raw token"
      exitFailure

fetchTokenMaybe :: IO (Maybe Text)
fetchTokenMaybe = do
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
