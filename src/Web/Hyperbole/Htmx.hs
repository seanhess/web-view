module Web.Hyperbole.Htmx where

import Data.String.Conversions (cs)
import Effectful
import Web.Htmx
import Web.Hyperbole.Route
import Web.UI
import Web.UI.Types


-- import Web.UI.Url

target :: Mod
target = hxTarget This . hxSwap InnerHTML


-- import Web.Hyperbole.Action
--
hxInclude :: Selector -> Mod
hxInclude s = att "hx-include" (toAtt s)


hxTarget :: HxTarget -> Mod
hxTarget t = att "hx-target" (toAtt t)


hxSwap :: HxSwap -> Mod
hxSwap t = att "hx-swap" (toAtt t)


hxGet :: Url -> Mod
hxGet = att "hx-get" . fromUrl


hxPost :: Url -> Mod
hxPost = att "hx-post" . fromUrl


hxPut :: Url -> Mod
hxPut = att "hx-put" . fromUrl


hxIndicator :: Selector -> Mod
hxIndicator s = att "hx-indicator" (toAtt s)


swapTarget :: HxSwap -> View' c () -> View' c ()
swapTarget t = tag "div" (hxSwap t . hxTarget This)


action :: (Show a) => a -> Mod
-- action a = hxPost (routeUrl a)
action a = att "data-action" (cs $ show a)
