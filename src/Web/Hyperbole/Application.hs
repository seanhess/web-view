module Web.Hyperbole.Application where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.String.Conversions (cs)
import Data.Text
import Effectful
import Network.HTTP.Types (status200, status301, status400, status404)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Web.Hyperbole.Route
import Web.Hyperbole.Sockets
import Web.Hyperbole.Wai
import Web.UI
import Web.UI.Types (Content (Text))

-- | Run both the http and ws application
application :: (PageRoute route) => (L.ByteString -> L.ByteString) -> (route -> Eff [Wai, IOE] ()) -> Application
application toDoc actions =
  websocketsOr connectionOptions (socketApplication talk)
    $ httpApplication toDoc actions

talk :: (Socket :> es) => Eff es ()
talk = do
  msg :: Text <- receiveData

  -- HANDLE ACTION?
  -- SEND VIEW (possibly multiple times). Need to run action...

  sendCommand $ Render [Text $ "Updated: " <> msg]

httpApplication :: (PageRoute route) => (L.ByteString -> L.ByteString) -> (route -> Eff [Wai, IOE] ()) -> Application
httpApplication toDoc actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case findRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [contentType ContentText] "Not Found"
    Just rt -> do
      res <- runEff . runWai request $ actions rt
      case res of
        Left err -> respond $ interrupt err
        Right resp -> do
          let headers = contentType resp.contentType : resp.headers
              respBody = addDocument (requestMethod request) resp.body
          liftIO $ respond $ responseLBS status200 headers respBody
 where
  findRoute [] = Just defRoute
  findRoute ps = matchRoute ps

  -- convert to document if GET. Subsequent POST requests will only include fragments
  addDocument "GET" bd = toDoc bd
  addDocument _ bd = bd

  contentType :: ContentType -> (HeaderName, ByteString)
  contentType ContentHtml = ("Content-Type", "text/html; charset=utf-8")
  contentType ContentText = ("Content-Type", "text/plain; charset=utf-8")

  interrupt NotFound =
    responseLBS status404 [contentType ContentText] "Not Found"
  interrupt (ParseError _) =
    responseLBS status400 [contentType ContentText] "Bad Request"
  interrupt (Redirect u) =
    responseLBS status301 [("Location", cs $ fromUrl u)] ""
