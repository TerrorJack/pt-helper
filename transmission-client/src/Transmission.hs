{-# LANGUAGE OverloadedStrings #-}

module Transmission where

import ClassyPrelude
import Data.Default.Class
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

data TransmissionException
    = TransmissionNoToken
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

initSession :: MonadIO m => Config -> m Session
initSession cfg = liftIO $ do
    let req = def {
        HTTP.method = "POST",
        HTTP.host = host cfg,
        HTTP.secure = secure cfg,
        HTTP.port = port cfg,
        HTTP.path = path cfg,
        HTTP.checkStatus = \_ _ _ -> Nothing
    }
    mgr <- HTTP.newManager $ if secure cfg then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings
    resp <- HTTP.httpLbs req mgr
    let tokname = "X-Transmission-Session-Id"
    case lookup tokname (HTTP.responseHeaders resp) of
        Just tok -> pure Session {
            sessionManager = mgr,
            sessionDefaultRequest = req {
                HTTP.requestHeaders = [(tokname, tok)]
            }
        }
        Nothing -> throwIO TransmissionNoToken
