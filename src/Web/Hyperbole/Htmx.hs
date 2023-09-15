module Web.Hyperbole.Htmx where

import Data.Text (Text)
import Data.Text qualified as T
import Web.Htmx
import Web.Hyperbole.Action
import Web.UI
import Web.UI.Types (Attribute)

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

action :: PageAction action => action -> Mod Attribute
action act = hxPost $ actionUrl act

actionUrl :: PageAction action => action -> Url
actionUrl a =
  Url $ "?action=" <> actionName a
