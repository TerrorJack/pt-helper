{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Transmission where

import ClassyPrelude
import Data.Default.Class
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

type Response = HTTP.Response LByteString

data TransmissionException
    = TransmissionNoToken !Response
    deriving (Show, Typeable)

instance Exception TransmissionException

data Session = Session {
    sessionManager :: !HTTP.Manager,
    sessionDefaultRequest :: !HTTP.Request
}

data Config = Config {
    host :: !ByteString,
    secure :: !Bool,
    port :: !Int,
    path :: !ByteString
}

refreshToken :: MonadThrow m => Response -> HTTP.Request -> m HTTP.Request
refreshToken resp req =
    case lookup tokname (HTTP.responseHeaders resp) of
        Just tok -> pure req { HTTP.requestHeaders = [(tokname, tok)] }
        Nothing -> throwM $ TransmissionNoToken resp
    where tokname = "X-Transmission-Session-Id"

initSession :: MonadIO m => Config -> m Session
initSession Config {..} = liftIO $ do
    let req = def {
        HTTP.method = "POST",
        HTTP.host = host,
        HTTP.secure = secure,
        HTTP.port = port,
        HTTP.path = path,
        HTTP.checkStatus = \_ _ _ -> Nothing
    }
    mgr <- HTTP.newManager $ if secure then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings
    resp <- HTTP.httpLbs req mgr
    req' <- refreshToken resp req
    pure Session {
        sessionManager = mgr,
        sessionDefaultRequest = req'
    }
