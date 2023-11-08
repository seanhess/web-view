module Web.UI.Layout where

import Data.Function
import Data.Text
import Web.UI.Element
import Web.UI.Style
import Web.UI.Types


-- | Sets the root layout so filling columns makes sense
rootLayout :: Mod
rootLayout =
  flexCol
    . addClass
      ( cls "layout"
          -- [ ("white-space", "pre")
          & prop @Text "width" "100vw"
          & prop @Text "height" "100vh"
          -- not sure if this property is necessary, copied from older code
          & prop @Text "min-height" "100vh"
          & prop @Text "z-index" "0"
      )


layout :: Mod -> View c () -> View c ()
layout f = el (rootLayout . f)


-- | You can make a fixed layout by using layout and putting "scroll" on a child-element
scroll :: Mod
scroll = addClass $ cls "scroll" & prop @Text "overflow" "auto"


data Nav = Nav


nav :: Mod -> View c () -> View c ()
nav f = tag "nav" (f . flexCol)
