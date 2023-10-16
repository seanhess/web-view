{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Page where

import Data.Bifunctor
import Data.String.Conversions
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Text.Read
import Web.FormUrlEncoded (parseUnique)
import Web.HttpApiData (FromHttpApiData)
import Web.Hyperbole.Wai (Interrupt (..), Wai (..), notFound)
import Web.Hyperbole.Wai qualified as Wai
import Web.UI


-- import Text.Read
-- import Web.Hyperbole.Route

data Page :: Effect where
  RespondView :: View () -> Page m ()
  GetAction :: (Read a) => Page m a
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
    case parseAction f of
      Left e -> send $ Interrupt (ParseError e)
      Right a -> pure a
  GetParam p ->
    Wai.formParam p
  PageError MissingInfo ->
    notFound
 where
  parseAction f = do
    t <- parseUnique "action" f
    readEitherText t

  readEitherText :: (Read a) => Text -> Either Text a
  readEitherText = first cs . readEither . cs


param :: (Page :> es, FromHttpApiData a) => Text -> Eff es a
param = send . GetParam


respondView :: (Page :> es) => View () -> Eff es ()
respondView = send . RespondView


missingInfo :: (Page :> es) => Eff es a
missingInfo = send (PageError MissingInfo)


data PageError
  = MissingInfo
