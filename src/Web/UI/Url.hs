{-# LANGUAGE DefaultSignatures #-}
module Web.UI.Url where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.String (IsString(..))

type Segment = Text
newtype Url = Url [Segment]
  deriving newtype (Show)

-- what if you want a relative url?
instance IsString Url where
  fromString s = Url [cleanSegment $ pack s]

fromUrl :: Url -> Text
fromUrl (Url ss) = "/" <> T.intercalate "/" ss

class ToSegment a where
  segment :: a -> Segment
  default segment :: Show a => a -> Segment
  segment = pack . show

instance ToSegment Int
instance ToSegment Integer
instance ToSegment Text where
  segment = id
instance ToSegment String where
  segment = pack
instance ToSegment L.Text where
  segment = L.toStrict

-- instance ToSegment Url where
--   segment (Url t) = t
--
cleanSegment :: Segment -> Segment
cleanSegment = T.dropWhileEnd (== '/') . T.dropWhile (== '/')

(//) :: Url -> Segment -> Url
(Url ss) // t = Url $ ss <> [cleanSegment t]
