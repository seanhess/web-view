{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Hyperbole.Page where

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
import Web.Hyperbole.LiveView
import Web.UI


-- you can't automatically derive FromHttpApiData. I don't like it!

data Page :: Effect where
  RespondView :: View () () -> Page m ()
  GetEvent :: (Param act, Param id, LiveView id) => Page m (Maybe (Event id act))
  GetForm :: Page m Form
  PageError :: PageError -> Page m a


type instance DispatchOf Page = 'Dynamic


data Event id act = Event id act


runPageWai
  :: (Wai :> es)
  => Eff (Page : es) a
  -> Eff es a
runPageWai = interpret $ \_ -> \case
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
  PageError NotFound -> send $ Interrupt Wai.NotFound
  PageError (ParseError e) -> send $ Interrupt $ Wai.ParseError e
 where
  lookupParam :: ByteString -> Query -> Maybe Text
  lookupParam p q =
    fmap cs <$> join $ lookup p q

  lookupEvent :: Query -> Maybe (Event Text Text)
  lookupEvent q =
    Event
      <$> lookupParam "id" q
      <*> lookupParam "action" q


formData :: (Page :> es) => Eff es Form
formData = send GetForm


param :: (Page :> es, Param a) => Text -> Form -> Eff es a
param p f = do
  -- param is required
  either (send . PageError . ParseError) pure $ do
    t <- Form.lookupUnique p f
    maybe (Left [i|could not parseParam: '#{t}'|]) pure $ parseParam t


view :: (Page :> es) => View () () -> Eff es ()
view = send . RespondView


notFound :: (Page :> es) => Eff es a
notFound = send (PageError NotFound)


data PageError
  = NotFound
  | ParseError Text


pageLoad :: (Page :> es) => Eff es (View () ()) -> Eff es ()
pageLoad pg = do
  vw <- pg
  view vw


pageAction :: forall id es. (Page :> es, LiveView id, Param id, Param (Action id), Show id) => (id -> Action id -> Eff es (View id ())) -> Eff es ()
pageAction handle = do
  -- this continues if it doesn't match!
  mev <- send GetEvent
  case mev of
    Just (Event vid act) -> do
      vw <- handle vid act
      view $ addContext vid vw
    _ -> pure ()
