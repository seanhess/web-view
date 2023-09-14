module Boop where

import Data.Text (Text)
import Data.Text qualified as T
import Htmx
import Web.UI
import Web.UI.Types (Attribute)

-- TODO: modifiers to swap: innerHTML swap:1s, innerHTML settle:1s

hxTarget :: HxTarget -> Mod Attribute
hxTarget t = att "hx-target" (toAtt t)

hxSwap :: HxSwap -> Mod Attribute
hxSwap t = att "hx-swap" (toAtt t)

hxGet :: Url -> Mod Attribute
hxGet (Url u) = att "hx-get" u

hxPost :: Url -> Mod Attribute
hxPost (Url u) = att "hx-post" u

hxPut :: Url -> Mod Attribute
hxPut (Url u) = att "hx-put" u

newtype Url = Url Text

(</>) :: Url -> Text -> Url
(Url u) </> t = Url $ T.dropWhileEnd (== '/') u <> "/" <> t
