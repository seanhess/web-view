{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Htmx where

import Web.Htmx
import Web.UI
import Web.UI.Url


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


-- action :: PageAction action => action -> Mod Attribute
-- action act = hxPost $ actionUrl act
--
-- actionUrl :: PageAction action => action -> Url
-- actionUrl a =
--   Url $ "?action=" <> actionName a
