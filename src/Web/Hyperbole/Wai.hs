{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Wai where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (Status, status200, status301, status400, status404, status500)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai as Wai
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData)
import Web.Hyperbole.Embed (cssResetEmbed, htmxScriptEmbed)
import Web.Hyperbole.Route
import Web.UI
import Web.UI.Render (renderLazyByteString)

data Wai :: Effect where
  ResHeader :: HeaderName -> ByteString -> Wai m ()
  ResBody :: MimeType -> L.ByteString -> Wai m ()
  ResStatus :: Status -> Wai m ()
  ReqBody :: Wai m L.ByteString
  Interrupt :: Interrupt -> Wai m a

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

formData :: (Wai :> es) => Eff es Form
formData = do
  bd <- send ReqBody
  let ef = urlDecodeForm bd
  either (send . Interrupt . ParseError) pure ef

parseFormData :: (Wai :> es, FromForm a) => Eff es a
parseFormData = do
  f <- formData
  either (send . Interrupt . ParseError) pure $ fromForm f

formParam :: (Wai :> es, FromHttpApiData a) => Text -> Eff es a
formParam k = do
  f <- formData
  either (send . Interrupt . ParseError) pure $ parseUnique k f

runWai
  :: (IOE :> es)
  => Request
  -> Eff (Wai : es) a
  -> Eff es (Either Interrupt Resp)
runWai req = reinterpret (runErrorNoCallStack @Interrupt . execState @Resp emptyResponse) $ \_ -> \case
  ReqBody -> do
    cacheReqBody
    gets reqBody
  ResHeader k v ->
    modify $ \r -> r{headers = (k, v) : r.headers}
  ResStatus s ->
    modify $ \r -> r{status = s}
  ResBody mt bd ->
    modify $ \r -> r{body = bd, mimeType = mt, status = status200}
  Interrupt e -> do
    throwError e
 where
  cacheReqBody :: forall es. (IOE :> es, State Resp :> es) => Eff es ()
  cacheReqBody = do
    r <- get
    when (L.null r.reqBody) $ do
      rb <- liftIO $ Wai.consumeRequestBodyLazy req
      put $ r{reqBody = rb}

application :: (PageRoute route) => (route -> Eff [Wai, IOE] ()) -> Application
application actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case findRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [contentType Text] "Not Found"
    Just rt -> do
      res <- runEff . runWai request $ actions rt
      case res of
        Left err -> respond $ interrupt err
        Right resp -> do
          let headers = contentType resp.mimeType : resp.headers
              respBody = addDocument (requestMethod request) resp.body
          liftIO $ respond $ responseLBS status200 headers respBody
 where
  findRoute [] = Just defRoute
  findRoute ps = matchRoute ps

  addDocument "GET" bd =
    [i|<html>
    <head>
      <title>This is a title!</title>
      <script type="text/javascript">#{htmxScriptEmbed}</script>
      <style type type="text/css">#{cssResetEmbed}</style>
    </head>
    <body>#{bd}</body>
  </html> |]
  addDocument _ bd = bd

  contentType :: MimeType -> (HeaderName, ByteString)
  contentType Html = ("Content-Type", "text/html; charset=utf-8")
  contentType Text = ("Content-Type", "text/plain; charset=utf-8")

  interrupt NotFound =
    responseLBS status404 [contentType Text] "Not Found"
  interrupt (ParseError _) =
    responseLBS status400 [contentType Text] "Bad Request"
  interrupt (Redirect u) =
    responseLBS status301 [("Location", cs $ fromUrl u)] ""

view :: (Wai :> es) => View () -> Eff es ()
view vw = do
  let bd = renderLazyByteString vw
  send $ ResHeader "Content-Type" "text/html"
  send $ ResBody Html bd

emptyResponse :: Resp
emptyResponse = Resp status500 [] Text "Response not set" ""

data Interrupt
  = NotFound
  | Redirect Url
  | ParseError Text

notFound :: (Wai :> es) => Eff es a
notFound = send $ Interrupt NotFound

redirect :: (Wai :> es) => Url -> Eff es ()
redirect u = do
  send $ Interrupt $ Redirect u
