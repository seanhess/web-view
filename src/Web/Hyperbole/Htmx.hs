module Web.Hyperbole.Htmx where

import Web.Htmx
import Web.Hyperbole.Route
import Web.UI

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

swapTarget :: HxSwap -> View () -> View ()
swapTarget t = tag "div" (hxSwap t . hxTarget This)

action :: (PageRoute a) => a -> Mod
action a = hxPost (routeUrl a)

