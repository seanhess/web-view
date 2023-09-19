{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Htmx where

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Web.Htmx
import Web.UI
import Web.UI.Types (Attribute)

-- import Web.Hyperbole.Action

hxTarget :: HxTarget -> Mod Attribute
hxTarget t = att "hx-target" (toAtt t)

hxSwap :: HxSwap -> Mod Attribute
hxSwap t = att "hx-swap" (toAtt t)

hxGet :: Url -> Mod Attribute
hxGet = att "hx-get" . fromUrl

hxPost :: Url -> Mod Attribute
hxPost = att "hx-post" . fromUrl

hxPut :: Url -> Mod Attribute
hxPut = att "hx-put" . fromUrl

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

-- action :: PageAction action => action -> Mod Attribute
-- action act = hxPost $ actionUrl act
--
-- actionUrl :: PageAction action => action -> Url
-- actionUrl a =
--   Url $ "?action=" <> actionName a
