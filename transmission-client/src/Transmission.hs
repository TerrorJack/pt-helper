{-|
    This module implements a wrapper for Transmission RPC protocol, as specified in <https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>.
-}

module Transmission (
    -- * Session
    Config(..),
    Session,
    initSession,
    -- * Miscellaneous types
    Response,
    TransmissionException(..),
    -- * Utilities
    sendRequest
) where

import ClassyPrelude
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import Data.Default.Class
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP

-- | Not needed in normal cases. May be useful when a 'TransmissionException' is raised and you want to inspect the raw response.
type Response = HTTP.Response LByteString

-- | All I/O actions in this module may raise a 'TransmissionException'.
data TransmissionException
    = TransmissionHTTPException !HTTP.HttpException -- ^ Raised by I/O actions in "Network.HTTP.Client".
    | TransmissionParseError !Response -- ^ Raised when a response with status code 200 returns, but fails to parse.
    | TransmissionOtherError !Response -- ^ Raised when a response with non-200 status code returns.
    deriving (Show, Typeable)

instance Exception TransmissionException

-- | Use the 'Default' class to get a default 'Config'.
data Config = Config {
    host :: !ByteString, -- ^ Host of RPC server. Defaults to @\"127.0.0.1\"@
    secure :: !Bool, -- ^ Whether or not to use HTTPS. Defaults to 'False'
    port :: !Int,  -- ^ Port of RPC server. Defaults to @9091@
    path :: !ByteString, -- ^ Path of RPC service. Defaults to @\"\/transmission\/rpc\"@
    timeout :: !Int -- ^ Timeout in milliseconds. Defaults to @10000@
}

instance Default Config where
    def = Config {
        host = "127.0.0.1",
        secure = False,
        port = 9091,
        path = "/transmission/rpc",
        timeout = 10000
    }

-- | Use 'initSession' to initiate a 'Session' from your 'Config', then use the 'Session' for other actions.
data Session = Session {
    sessionManager :: !HTTP.Manager,
    sessionDefaultRequest :: !(IORef HTTP.Request)
}

-- | The internal action for performing requests. Transparently handles acquiring/refreshing CSRF tokens.
sendRequest :: (MonadReader Session m, MonadBaseControl IO m, MonadThrow m, JSON.ToJSON req, JSON.FromJSON resp) => req -> m resp
sendRequest reqjson = catch sendreq handler where
    sendreq = do
        Session {..} <- ask
        defreq <- readIORef sessionDefaultRequest
        let req = defreq { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode reqjson }
        resp <- liftBase $ HTTP.httpLbs req sessionManager
        case HTTP.statusCode $ HTTP.responseStatus resp of
            200 -> case JSON.decode $ HTTP.responseBody resp of
                Just result -> pure result
                Nothing -> throwM $ TransmissionParseError resp
            409 -> case lookup tokname (HTTP.responseHeaders resp) of
                Just tok -> do
                    let defreq' = defreq { HTTP.requestHeaders = [(tokname, tok)] }
                    writeIORef sessionDefaultRequest defreq'
                    sendRequest reqjson
                Nothing -> throwM $ TransmissionParseError resp
            _ -> throwM $ TransmissionOtherError resp
    handler e = throwM $ TransmissionHTTPException e
    tokname = "X-Transmission-Session-Id"

-- | Initiate a 'Session'.
initSession :: MonadBase IO m => Config -> m Session
initSession Config {..} = liftBase $ do
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
    (_ :: JSON.Value) <- runReaderT (sendRequest ()) s
    pure s

-- | Represents a @.torrent@ file to be added.
data Torrent
    = TorrentURL !Text -- ^ Can be filename/URL/magnet link
    | TorrentFile !ByteString -- ^ Raw content of the @.torrent@ file

-- | Request for @torrent-add@ method.
data TorrentAddRequest = TorrentAddRequest {
    torrent :: !Torrent
}

instance JSON.ToJSON TorrentAddRequest where
    toJSON (TorrentAddRequest (TorrentURL url)) = JSON.Object [("method",JSON.String "torrent-add"),("arguments",JSON.Object [("filename",JSON.String url)])]
    toJSON (TorrentAddRequest (TorrentFile file)) = JSON.Object [("method",JSON.String "torrent-add"),("arguments",JSON.Object [("metainfo",JSON.String $ decodeUtf8 $ Base64.encode file)])]
