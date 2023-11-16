{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Effect where

import Control.Monad (join)
import Data.ByteString
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Wai (ContentType (..), Wai (..))
import Effectful.Wai qualified as Wai
import Network.HTTP.Types (Query)
import Network.Wai
import Web.FormUrlEncoded (Form)
import Web.FormUrlEncoded qualified as Form
import Web.Hyperbole.HyperView
import Web.UI


-- you can't automatically derive FromHttpApiData. I don't like it!

data Hyperbole :: Effect where
  RespondView :: View () () -> Hyperbole m ()
  GetEvent :: (HyperView id action) => Hyperbole m (Maybe (Event id action))
  GetForm :: Hyperbole m Form
  HyperError :: HyperError -> Hyperbole m a


type instance DispatchOf Hyperbole = 'Dynamic


data Event id act = Event id act


runHyperbole
  :: (Wai :> es)
  => Eff (Hyperbole : es) a
  -> Eff es a
runHyperbole = interpret $ \_ -> \case
  RespondView vw -> do
    let bd = renderLazyByteString () vw
    send $ ResHeader "Content-Type" "text/html"
    send $ ResBody ContentHtml bd
    Wai.continue
  GetEvent -> do
    q <- fmap queryString <$> send $ Wai.Request
    pure $ do
      Event ti ta <- lookupEvent q
      vid <- parseParam ti
      act <- parseParam ta
      pure $ Event vid act
  GetForm -> Wai.formData
  HyperError NotFound -> send $ Interrupt Wai.NotFound
  HyperError (ParseError e) -> send $ Interrupt $ Wai.ParseError e
 where
  lookupParam :: ByteString -> Query -> Maybe Text
  lookupParam p q =
    fmap cs <$> join $ lookup p q

  lookupEvent :: Query -> Maybe (Event Text Text)
  lookupEvent q =
    Event
      <$> lookupParam "id" q
      <*> lookupParam "action" q


formData :: (Hyperbole :> es) => Eff es Form
formData = send GetForm


-- | Read a required form parameter
param :: (Hyperbole :> es, Param a) => Text -> Form -> Eff es a
param p f = do
  -- param is required
  either (send . HyperError . ParseError) pure $ do
    t <- Form.lookupUnique p f
    maybe (Left [i|could not parseParam: '#{t}'|]) pure $ parseParam t


notFound :: (Hyperbole :> es) => Eff es a
notFound = send (HyperError NotFound)


data HyperError
  = NotFound
  | ParseError Text


load :: (Hyperbole :> es) => Eff es (View () ()) -> Eff es ()
load pg = do
  vw <- pg
  send $ RespondView vw


hyper :: forall id action es. (Hyperbole :> es, HyperView id action) => (id -> action -> Eff es (View id ())) -> Eff es ()
hyper run = do
  -- Get an event matching our type. If it doesn't match, skip to the next handler
  mev <- send GetEvent
  case mev of
    Just (Event vid act) -> do
      vw <- run vid act
      send $ RespondView $ viewId vid vw
    _ -> pure ()
