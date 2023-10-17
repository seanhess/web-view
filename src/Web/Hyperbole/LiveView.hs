{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.LiveView where

import Data.Text


-- import Data.String.Conversions
-- import Data.Text
-- import Web.HttpApiData (FromHttpApiData (..))

-- keep it simple for now


class LiveView id where
  type Action id

-- parseId :: Text -> Maybe id
-- default parseId :: (Read id) => Text -> Maybe id
-- parseId = readMaybe . cs
--
--
-- idAtt :: id -> Text
-- default idAtt :: (Show id) => id -> Text
-- idAtt = cs . show
--
--
-- parseAction :: Text -> Maybe (Action id)
-- default parseAction :: (Read (Action id)) => Text -> Maybe (Action id)
-- parseAction = readMaybe . cs
--
--
-- actionAtt :: Action id -> Text
-- default actionAtt :: (Show (Action id)) => Action id -> Text
-- actionAtt = cs . show
