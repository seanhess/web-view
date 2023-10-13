{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Page where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Web.FormUrlEncoded (parseMaybe)
import Web.HttpApiData (FromHttpApiData)
import Web.Hyperbole.Route
import Web.Hyperbole.Wai (Interrupt (..), Wai (..), notFound)
import Web.Hyperbole.Wai qualified as Wai
import Web.UI

data Page :: Effect where
  RespondView :: View () -> Page m ()
  GetAction :: (PageRoute action) => Page m (Maybe action)
  GetParam :: (FromHttpApiData a) => Text -> Page m a
  PageError :: PageError -> Page m a

type instance DispatchOf Page = 'Dynamic

runPageWai
  :: (Wai :> es)
  => Eff (Page : es) a
  -> Eff es a
runPageWai = interpret $ \_ -> \case
  RespondView vw ->
    Wai.view vw
  GetAction -> do
    f <- Wai.formData
    case parseMaybe "action" f of
      -- action was missing, return Nothing
      Right Nothing -> pure Nothing
      -- action was present, but failed to parse
      Left e -> send $ Interrupt (ParseError e)
      -- action was parsed to text
      Right (Just t) -> do
        -- attempt to parse action into a local action
        maybe (send $ Interrupt (ParseError "Bad Action")) pure $ matchRoute $ pathSegments t
  GetParam p ->
    Wai.formParam p
  PageError MissingInfo ->
    notFound

param :: (Page :> es, FromHttpApiData a) => Text -> Eff es a
param = send . GetParam

respondView :: (Page :> es) => View () -> Eff es ()
respondView = send . RespondView

missingInfo :: (Page :> es) => Eff es a
missingInfo = send (PageError MissingInfo)

data PageError
  = MissingInfo
