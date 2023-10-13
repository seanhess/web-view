{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Wai where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
import Network.HTTP.Types (Status, status200, status500)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai as Wai
import Web.FormUrlEncoded
import Web.HttpApiData (FromHttpApiData)
import Web.UI
import Web.UI.Render (renderLazyByteString)

data Wai :: Effect where
  ResHeader :: HeaderName -> ByteString -> Wai m ()
  ResBody :: ContentType -> L.ByteString -> Wai m ()
  ResStatus :: Status -> Wai m ()
  ReqBody :: Wai m L.ByteString
  Interrupt :: Interrupt -> Wai m a

type instance DispatchOf Wai = 'Dynamic

data Resp = Resp
  { status :: Status
  , headers :: [(HeaderName, ByteString)]
  , contentType :: ContentType
  , body :: L.ByteString
  , reqBody :: L.ByteString
  }

data ContentType
  = ContentHtml
  | ContentText

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
  ResHeader k v -> modify
    $ \r -> r{headers = (k, v) : r.headers}
  ResStatus s -> modify
    $ \r -> r{status = s}
  ResBody ct bd -> modify
    $ \r -> r{body = bd, contentType = ct, status = status200}
  Interrupt e -> do
    throwError e
 where
  cacheReqBody :: forall es. (IOE :> es, State Resp :> es) => Eff es ()
  cacheReqBody = do
    r <- get
    when (L.null r.reqBody) $ do
      rb <- liftIO $ Wai.consumeRequestBodyLazy req
      put $ r{reqBody = rb}

view :: (Wai :> es) => Eff '[View] () -> Eff es ()
view vw = do
  let bd = runPureEff $ renderLazyByteString vw
  send $ ResHeader "Content-Type" "text/html"
  send $ ResBody ContentHtml bd

emptyResponse :: Resp
emptyResponse = Resp status500 [] ContentText "Response not set" ""

data Interrupt
  = NotFound
  | Redirect Url
  | ParseError Text

notFound :: (Wai :> es) => Eff es a
notFound = send $ Interrupt NotFound

redirect :: (Wai :> es) => Url -> Eff es ()
redirect u = do
  send $ Interrupt $ Redirect u
