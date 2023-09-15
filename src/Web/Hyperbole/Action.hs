{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Action where

import Data.Default (Default)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)

class Default a => PageAction a where
  actionName :: a -> Text
  default actionName :: Show a => a -> Text
  actionName = pack . show

  fromName :: Text -> Maybe a
  default fromName :: Read a => Text -> Maybe a
  fromName = readMaybe . unpack
