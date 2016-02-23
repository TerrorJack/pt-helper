{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Transmission (
    Response,
    TransmissionException(..),
    Session,
    Config(..),
    initSession
) where

import ClassyPrelude
import qualified Data.Aeson as JSON
import Data.Default.Class
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP

type Response = HTTP.Response LByteString

data TransmissionException
    = TransmissionHTTPException !HTTP.HttpException
    | TransmissionParseError !Response
    | TransmissionOtherError !Response
    deriving (Show, Typeable)

instance Exception TransmissionException

data Session = Session {
    sessionManager :: !HTTP.Manager,
    sessionDefaultRequest :: !(IORef HTTP.Request)
}

data Config = Config {
    host :: !ByteString,
    secure :: !Bool,
    port :: !Int,
    path :: !ByteString,
    timeout :: !Int
}

instance Default Config where
    def = Config {
        host = "127.0.0.1",
        secure = False,
        port = 9091,
        path = "/transmission/rpc",
        timeout = 10000
    }

sendRequest :: (MonadIO m, JSON.ToJSON req, JSON.FromJSON resp) => Session -> req -> m resp
sendRequest s@Session {..} reqjson = liftIO $ catch sendreq handler where
    sendreq = do
        defreq <- readIORef sessionDefaultRequest
        let req = defreq { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode reqjson }
        resp <- HTTP.httpLbs req sessionManager
        case HTTP.statusCode $ HTTP.responseStatus resp of
            200 -> case JSON.decode $ HTTP.responseBody resp of
                Just result -> pure result
                Nothing -> throwM $ TransmissionParseError resp
            409 -> case lookup tokname (HTTP.responseHeaders resp) of
                Just tok -> do
                    let defreq' = defreq { HTTP.requestHeaders = [(tokname, tok)] }
                    writeIORef sessionDefaultRequest defreq'
                    sendRequest s reqjson
                Nothing -> throwM $ TransmissionParseError resp
            _ -> throwM $ TransmissionOtherError resp
    handler e = throwM $ TransmissionHTTPException e
    tokname = "X-Transmission-Session-Id"

initSession :: MonadIO m => Config -> m Session
initSession Config {..} = liftIO $ do
    mgr <- HTTP.newManager $ if secure then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings
    let req = def {
        HTTP.method = "POST",
        HTTP.host = host,
        HTTP.secure = secure,
        HTTP.port = port,
        HTTP.path = path,
        HTTP.checkStatus = \_ _ _ -> Nothing,
        HTTP.cookieJar = Nothing,
        HTTP.redirectCount = 0,
        HTTP.responseTimeout = Just timeout
    }
    reqref <- newIORef req
    let s = Session {
        sessionManager = mgr,
        sessionDefaultRequest = reqref
    }
    (_ :: JSON.Value) <- sendRequest s ()
    pure s
