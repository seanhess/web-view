{-# LANGUAGE LambdaCase #-}

module Web.Hyperbole.Page where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Text.Read (readMaybe)

data PageError
  = ParamError Text Text
  deriving (Show, Exception)

data Page :: Effect where
  GetParam :: FromParam a => Text -> Page m a
  GetParams :: Page m [(Text, Text)]

-- RequestBody :: Page m ByteString

type instance DispatchOf Page = 'Dynamic

runPage
  :: (Error PageError :> es, IOE :> es)
  => [(Text, Text)]
  -> Eff (Page : es) a
  -> Eff es a
runPage ps =
  interpret $ \_ -> \case
    GetParam k -> getParam k
    GetParams -> pure ps
 where
  getParam :: (FromParam a, Error PageError :> es) => Text -> Eff es a
  getParam k = do
    let sval = filterParams k
    either (throwError . ParamError k) pure $ fromParam sval

  -- combines into comma-separated lists if multiple
  filterParams :: Text -> Text
  filterParams k = do
    T.intercalate "," $ map snd $ filter ((k ==) . fst) ps

param :: (Page :> es, FromParam a) => Text -> Eff es a
param n = send $ GetParam n

params :: (Page :> es) => Eff es [(Text, Text)]
params = send GetParams

class FromParam a where
  fromParam :: Text -> Either Text a

readEither :: Read a => Text -> Either Text a
readEither inp =
  case readMaybe (unpack inp) of
    Nothing -> Left $ "could not parse '" <> inp <> "'"
    Just a -> pure a

instance FromParam String where
  fromParam inp = pure $ unpack inp

instance FromParam Text where
  fromParam = pure

instance FromParam Integer where
  fromParam inp =
    first ("Expected Integer:" <>) $ readEither inp

instance FromParam Int where
  fromParam inp =
    first ("Expected Int:" <>) $ readEither inp

instance (FromParam a, Read a) => FromParam [a] where
  fromParam "" = pure []
  fromParam is = first ("Expected List: " <>) $ mapM readEither $ T.splitOn "," is
