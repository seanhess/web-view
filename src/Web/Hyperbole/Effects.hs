{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Effects where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai

data Wai :: Effect where
  ResHeader :: HeaderName -> ByteString -> Wai m ()
  ResBody :: L.ByteString -> Wai m ()
  Respond :: Wai m ResponseReceived
  ReqBody :: Wai m L.ByteString

type instance DispatchOf Wai = 'Dynamic

data Resp = Resp
  { headers :: [(HeaderName, ByteString)]
  , body :: L.ByteString
  }

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

runWai
  :: IOE :> es
  => Request
  -> (Response -> IO ResponseReceived)
  -> Eff (Wai : es) a
  -> Eff es a
runWai req respond = reinterpret (evalState emptyResponse) $ \_ -> \case
  ReqBody -> liftIO $ lazyRequestBody req
  ResHeader k v -> addHeader k v
  ResBody bd -> setResponseBody bd
  Respond -> do
    (resp :: Resp) <- get
    liftIO $ respond $ responseLBS status200 resp.headers resp.body
 where
  addHeader :: (State Resp :> es) => HeaderName -> ByteString -> Eff es ()
  addHeader k v = modify $ \r -> r{headers = (k, v) : r.headers}

  setResponseBody :: (State Resp :> es) => L.ByteString -> Eff es ()
  setResponseBody bd = modify $ \r -> r{body = bd}

-- runWai'
--   :: (IOE :> es)
--   => (Response -> IO ResponseReceived)
--   -> Eff (State Resp : Wai : es) ()
--   -> Eff es ResponseReceived
-- runWai' respond build = do
--   let wai = evalState (Resp [] "") build
--   runWai respond wai

emptyResponse :: Resp
emptyResponse = Resp [] ""
