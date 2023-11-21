module Web.View.Layout where

import Data.Function
import Data.Text
import Web.View.Element
import Web.View.Style
import Web.View.Types


{- | Use `root` on the top-level tag in your document to allow columns to fill the view

> document = col (root . pad 10) myContent
-}
root :: Mod
root =
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


{- | Same as 'root' but as an Element

> document = layout (pad 10) myContent
-}
layout :: Mod -> View c () -> View c ()
layout f = el (root . f)


{- | Make a fixed layout by using layout and putting "scroll" on a child-element

> document = row root $ do
>   nav id $ "Left Sidebar"
>   col scroll $ "Main Content"
-}
scroll :: Mod
scroll = addClass $ cls "scroll" & prop @Text "overflow" "auto"


-- | A Nav element
nav :: Mod -> View c () -> View c ()
nav f = tag "nav" (f . flexCol)
