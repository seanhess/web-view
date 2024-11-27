module Web.View.Layout where

import Data.Function
import Data.Text
import Web.View.Element
import Web.View.Style
import Web.View.Types
import Web.View.View (View, tag)


{- | We can intuitively create layouts with combindations of 'row', 'col', 'grow', and 'space'

Wrap main content in 'layout' to allow the view to consume vertical screen space

@
holygrail :: 'View' c ()
holygrail = 'layout' id $ do
  'row' section "Top Bar"
  'row' 'grow' $ do
    'col' section "Left Sidebar"
    'col' (section . 'grow') "Main Content"
    'col' section "Right Sidebar"
  'row' section "Bottom Bar"
  where section = 'border' 1
@
-}
layout :: Mod c -> View c () -> View c ()
layout f = el (root . f)


{- | As `layout` but as a 'Mod'

> holygrail = col root $ do
>   ...
-}
root :: Mod c
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


{- | Lay out children in a column.

> col grow $ do
>    el_ "Top"
>    space
>    el_ "Bottom"
-}
col :: Mod c -> View c () -> View c ()
col f = el (flexCol . f)


{- | Lay out children in a row

> row id $ do
>    el_ "Left"
>    space
>    el_ "Right"
-}
row :: Mod c -> View c () -> View c ()
row f = el (flexRow . f)


{- | Grow to fill the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'

> row id $ do
>  el grow none
>  el_ "Right"
-}
grow :: Mod c
grow = addClass $ cls "grow" & prop @Int "flex-grow" 1


{- | Space that fills the available space in the parent 'Web.View.Layout.row' or 'Web.View.Layout.col'.


> row id $ do
>  space
>  el_ "Right"

This is equivalent to an empty element with 'grow'

> space = el grow none
-}
space :: View c ()
space = el grow none


-- | Allow items to become smaller than their contents. This is not the opposite of grow!
collapse :: Mod c
collapse = addClass $ cls "collapse" & prop @Int "min-width" 0


{- | Make a fixed 'layout' by putting 'scroll' on a child-element

> document = row root $ do
>   nav (width 300) "Sidebar"
>   col (grow . scroll) "Main Content"
-}
scroll :: Mod c
scroll = addClass $ cls "scroll" & prop @Text "overflow" "auto"


-- | A Nav element
nav :: Mod c -> View c () -> View c ()
nav f = tag "nav" (f . flexCol)


-- | A stack container puts its contents on top of each other. Each child has the full width?
stack :: Mod c -> View c () -> View c ()
stack f =
  tag "div" (f . container . absChildren)
 where
  container =
    addClass $
      cls "stack"
        & prop @Text "position" "relative"
        & prop @Text "display" "grid"
  absChildren =
    addClass $
      Class absSelector mempty
        & prop @Text "position" "relative"
        & prop @Text "grid-area" "1 / 1"
  absSelector = (selector "abs-childs"){child = Just AllChildren}
