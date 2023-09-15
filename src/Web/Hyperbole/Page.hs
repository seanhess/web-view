{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Page where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Web.Scotty (Param, Parsable (..))

data PageError
  = ParamError Text Text
  deriving (Show, Exception)

data Page :: Effect where
  GetParam :: Parsable a => Text -> Page m a

-- RequestBody :: Page m ByteString

type instance DispatchOf Page = 'Dynamic

runPage
  :: (Error PageError :> es, IOE :> es)
  => [Param]
  -> Eff (Page : es) a
  -> Eff es a
runPage ps =
  interpret $ \_ -> \case
    GetParam k -> getParam k
 where
  -- RequestBody -> pure body

  getParam :: (Parsable a, Error PageError :> es) => Text -> Eff es a
  getParam k = do
    sval <- maybe (throwError $ ParamError "Missing" k) pure $ lookupParam k
    either (throwError . ParamError k . L.toStrict) pure $ parseParam sval

  lookupParam :: Text -> Maybe L.Text
  lookupParam k = do
    lookup (L.fromStrict k) ps

param :: (Page :> es, Parsable a) => Text -> Eff es a
param n = send $ GetParam n
