{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Wai where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai as Wai
import Network.Wai.Parse as Wai

-- import Network.Wai.Parse
import Web.Hyperbole.Route
import Web.UI
import Web.UI.Render (renderLazyByteString)

data Wai :: Effect where
  ResHeader :: HeaderName -> ByteString -> Wai m ()
  ResBody :: MimeType -> L.ByteString -> Wai m ()
  ReqBody :: Wai m L.ByteString
  ResError :: WaiError -> Wai m a

type instance DispatchOf Wai = 'Dynamic

data Resp = Resp
  { status :: Status
  , headers :: [(HeaderName, ByteString)]
  , mimeType :: MimeType
  , body :: L.ByteString
  , reqBody :: L.ByteString
  }

data MimeType
  = Html
  | Text

-- TODO: support file uploads
-- formData :: Wai :> es => Eff es [Param]
-- formData = do
--   rb <- gets reqBody
--   Wai.parseRequestBodyEx opts backend req
--
-- notFound :: Wai :> es => Eff es ()
-- notFound = do

-- runWaiResp
--   :: Eff (Wai : es) ()
--   -> Eff es Resp
-- runWaiResp = reinterpret (execState empty) $ \_ -> \case
--   AddHeader k v -> do
--     modify $ \r -> r{headers = (k, v) : r.headers}
--   SetResponseBody body -> do
--     modify $ \r -> r{body = body}
--  where
--   empty :: Resp
--   empty = Resp [] ""

-- sendResponse :: Wai :> es => Resp -> Eff es ResponseReceived
-- sendResponse resp = send $ Respond resp

-- responseBody :: Wai :> es => ResponseBody -> Eff es ()
-- responseBody bs = send $ ResBody bs

runWai
  :: IOE :> es
  => Request
  -> Eff (Wai : es) a
  -> Eff es (Either WaiError Resp)
runWai req = reinterpret (runErrorNoCallStack @WaiError . execState @Resp emptyResponse) $ \_ -> \case
  ReqBody -> do
    cacheReqBody
    gets reqBody
  ResHeader k v ->
    modify $ \r -> r{headers = (k, v) : r.headers}
  ResBody mt bd ->
    modify $ \r -> r{body = bd, mimeType = mt, status = status200}
  ResError e -> do
    throwError e
 where
  cacheReqBody :: forall es. (IOE :> es, State Resp :> es) => Eff es ()
  cacheReqBody = do
    r <- get
    when (L.null r.reqBody) $ do
      rb <- liftIO $ Wai.consumeRequestBodyLazy req
      put $ r{reqBody = rb}

application :: (Route route) => (route -> Eff [Wai, IOE] ()) -> Application
application actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case matchRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [contentType Text] "Not Found"
    Just rt -> do
      res <- runEff . runWai request $ do
        actions rt
      case res of
        Left err -> respond $ responseError err
        Right resp -> do
          let headers = contentType resp.mimeType : resp.headers
          liftIO $ respond $ responseLBS status200 headers resp.body
 where
  contentType :: MimeType -> (HeaderName, ByteString)
  contentType Html = ("Content-Type", "text/html")
  contentType Text = ("Content-Type", "text/plain")

  responseError NotFound =
    responseLBS status404 [contentType Text] "Not Found"

view :: Wai :> es => View () -> Eff es ()
view vw = do
  let bd = renderLazyByteString vw
  send $ ResHeader "Content-Type" "text/html"
  send $ ResBody Html bd

emptyResponse :: Resp
emptyResponse = Resp status404 [] Text "Not Found" ""

data WaiError
  = NotFound

notFound :: Wai :> es => Eff es a
notFound = send $ ResError NotFound
