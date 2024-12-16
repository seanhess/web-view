{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Web.View.Style where

import Data.Function ((&))
import Data.Map qualified as M
import Data.Text (Text)
import Web.View.Types


{- HLINT "HLint: shadows the existing binding" -}

-- * Styles


-- | Set to a specific width
width :: Length -> Mod c
width n =
  addClass $
    cls ("w" -. n)
      & prop "width" n
      & prop @Int "flex-shrink" 0


-- | Set to a specific height
height :: Length -> Mod c
height n =
  addClass $
    cls ("h" -. n)
      & prop "height" n
      & prop @Int "flex-shrink" 0


-- | Allow width to grow to contents but not shrink any smaller than value
minWidth :: Length -> Mod c
minWidth n =
  addClass $
    cls ("mw" -. n)
      & prop "min-width" n


-- | Allow height to grow to contents but not shrink any smaller than value
minHeight :: Length -> Mod c
minHeight n =
  addClass $
    cls ("mh" -. n)
      & prop "min-height" n


{- | Space surrounding the children of the element

To create even spacing around and between all elements:

> col (pad 10 . gap 10) $ do
>   el_ "one"
>   el_ "two"
>   el_ "three"
-}
pad :: Sides Length -> Mod c
pad (All n) =
  addClass $
    cls ("pad" -. n)
      & prop "padding" n
pad (Y n) =
  addClass $
    cls ("pady" -. n)
      & prop "padding-top" n
      & prop "padding-bottom" n
pad (X n) =
  addClass $
    cls ("padx" -. n)
      & prop "padding-left" n
      & prop "padding-right" n
pad (XY x y) =
  addClass $
    cls ("pad" -. x -. y)
      & prop "padding-left" x
      & prop "padding-right" x
      & prop "padding-top" y
      & prop "padding-bottom" y
pad (TRBL t r b l) =
  addClass $
    cls ("pad" -. t -. r -. b -. l)
      & prop "padding-top" t
      & prop "padding-right" r
      & prop "padding-bottom" b
      & prop "padding-left" l


-- | The space between child elements. See 'pad'
gap :: Length -> Mod c
gap n = addClass $ cls ("gap" -. n) & prop "gap" n


fontSize :: Length -> Mod c
fontSize n = addClass $ cls ("fs" -. n) & prop "font-size" n


-- fontFamily :: Text -> Mod c
-- fontFamily t = cls1 $ Class ("font" -. n) [("font-family", pxRem n)]

-- | Set container to be a row. Favor 'Web.View.Layout.row' when possible
flexRow :: Mod c
flexRow =
  addClass $
    cls "row"
      & prop @Text "display" "flex"
      & prop @Text "flex-direction" "row"


-- | Set container to be a column. Favor 'Web.View.Layout.col' when possible
flexCol :: Mod c
flexCol =
  addClass $
    cls "col"
      & prop @Text "display" "flex"
      & prop @Text "flex-direction" "column"


-- | Adds a basic drop shadow to an element
shadow :: Mod c
shadow =
  addClass $
    cls "shadow"
      & prop @Text "box-shadow" "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)"


-- | Round the corners of the element
rounded :: Length -> Mod c
rounded n = addClass $ cls ("rnd" -. n) & prop "border-radius" n


-- | Set the background color. See 'Web.View.Types.ToColor'
bg :: (ToColor clr) => clr -> Mod ctx
bg c =
  addClass $
    cls ("bg" -. colorName c)
      & prop "background-color" (colorValue c)


-- | Set the text color. See 'Web.View.Types.ToColor'
color :: (ToColor clr) => clr -> Mod ctx
color c = addClass $ cls ("clr" -. colorName c) & prop "color" (colorValue c)


bold :: Mod c
bold = addClass $ cls "bold" & prop @Text "font-weight" "bold"


-- | Hide an element. See 'parent' and 'media'
hide :: Mod c
hide =
  addClass $
    cls "hide"
      & prop @Text "display" "none"


opacity :: Float -> Mod c
opacity n =
  addClass $
    cls ("opacity" -. n)
      & prop "opacity" n


{- | Set a border around the element

> el (border 1) "all sides"
> el (border (X 1)) "only left and right"
-}
border :: Sides PxRem -> Mod c
border (All p) =
  addClass $
    cls ("brd" -. p)
      & prop "border-width" p
      & prop @Text "border-style" "solid"
border (Y p) =
  addClass $
    cls ("brdy" -. p)
      & prop "border-top-width" p
      & prop "border-bottom-width" p
border (X p) =
  addClass $
    cls ("brdx" -. p)
      & prop "border-left-width" p
      & prop "border-right-width" p
border (XY x y) =
  addClass $
    cls ("brd" -. x -. y)
      & prop "border-right-width" x
      & prop "border-left-width" x
      & prop "border-top-width" y
      & prop "border-bottom-width" y
border (TRBL t r b l) =
  addClass $
    cls ("brd" -. t -. r -. b -. l)
      & prop "border-top-width" t
      & prop "border-right-width" r
      & prop "border-bottom-width" b
      & prop "border-left-width" l


-- | Set a border color. See 'Web.View.Types.ToColor'
borderColor :: (ToColor clr) => clr -> Mod ctx
borderColor c =
  addClass $
    cls ("brdc" -. colorName c)
      & prop "border-color" (colorValue c)


{- | Use a button-like cursor when hovering over the element

Button-like elements:

> btn = pointer . bg Primary . hover (bg PrimaryLight)
>
> options = row id $ do
>   el btn "Login"
>   el btn "Sign Up"
-}
pointer :: Mod c
pointer = addClass $ cls "pointer" & prop @Text "cursor" "pointer"


-- | Cut off the contents of the element
truncate :: Mod c
truncate =
  addClass $
    cls "truncate"
      & prop @Text "white-space" "nowrap"
      & prop @Text "overflow" "hidden"
      & prop @Text "text-overflow" "ellipsis"


{- | Animate changes to the given property

> el (transition 100 (Height 400)) "Tall"
> el (transition 100 (Height 100)) "Small"
-}
transition :: Ms -> TransitionProperty -> Mod c
transition ms = \case
  (Height n) -> trans "height" n
  (Width n) -> trans "width" n
  (BgColor c) -> trans "background-color" c
  (Color c) -> trans "color" c
 where
  trans p val =
    addClass $
      cls ("t" -. val -. p -. ms)
        & prop "transition-duration" ms
        & prop "transition-property" p
        & prop p val


-- You MUST set the height/width manually when you attempt to transition it
data TransitionProperty
  = Width PxRem
  | Height PxRem
  | BgColor HexColor
  | Color HexColor
  deriving (Show)


textAlign :: Align -> Mod c
textAlign a =
  addClass $
    cls ("ta" -. a)
      & prop "text-align" a


-- * Selector Modifiers


{- | Apply when hovering over an element

> el (bg Primary . hover (bg PrimaryLight)) "Hover"
-}
hover :: Mod c -> Mod c
hover = applyPseudo Hover


-- | Apply when the mouse is pressed down on an element
active :: Mod c -> Mod c
active = applyPseudo Active


-- | Apply to even-numbered children
even :: Mod c -> Mod c
even = applyPseudo Even


-- | Apply to odd-numbered children
odd :: Mod c -> Mod c
odd = applyPseudo Odd


{- | Apply when the Media matches the current window. This allows for responsive designs

> el (width 100 . media (MinWidth 800) (width 400))
>   "Big if window > 800"
-}
media :: Media -> Mod c -> Mod c
media m = mapModClass $ \c ->
  c
    { selector = addMedia c.selector
    }
 where
  addMedia :: Selector -> Selector
  addMedia Selector{..} = Selector{media = Just m, ..}


{- | Apply when the element is somewhere inside an anscestor.

For example, the HTMX library applies an "htmx-request" class to the body when a request is pending. We can use this to create a loading indicator

> el (pad 10) $ do
>   el (parent "htmx-request" flexRow . hide) "Loading..."
>   el (parent "htmx-request" hide . flexRow) "Normal Content"
-}
parent :: Text -> Mod c -> Mod c
parent p = mapModClass $ \c ->
  c
    { selector = addAncestor c.selector
    }
 where
  addAncestor :: Selector -> Selector
  addAncestor Selector{..} = Selector{ancestor = Just p, ..}


-- Add a pseudo-class like Hover to your style
applyPseudo :: Pseudo -> Mod c -> Mod c
applyPseudo ps = mapModClass $ \c ->
  c
    { selector = addToSelector c.selector
    }
 where
  addToSelector :: Selector -> Selector
  addToSelector Selector{..} = Selector{pseudo = Just ps, ..}


mapModClass :: (Class -> Class) -> Mod c -> Mod c
mapModClass fc fm as =
  -- apply the function to all classes added by the mod
  -- ignore
  let as' = fm $ Attributes [] []
   in as'
        { classes = as.classes <> map fc as'.classes
        , other = as.other <> as'.other
        }


-- * Creating New Styles


{- | Add a single class

> width :: PxRem -> Mod
> width n =
>   addClass
>     $ cls ("w" -. n)
>     & prop "width" n
>     & prop @Int "flex-shrink" 0
-}
addClass :: Class -> Mod c
addClass c attributes =
  Attributes
    { classes = c : attributes.classes
    , other = attributes.other
    }


-- | Construct a class from a ClassName
cls :: ClassName -> Class
cls n = Class (selector n) []


{- | Construct a mod from a ClassName with no CSS properties. Convenience for situations where external CSS classes need to be referenced.

> el (extClass "btn" . extClass "btn-primary") "Click me!"
-}
extClass :: ClassName -> Mod c
extClass = addClass . cls


-- | Add a property to a class
prop :: (ToStyleValue val) => Name -> val -> Class -> Class
prop n v c =
  c{properties = M.insert n (toStyleValue v) c.properties}


-- | Hyphneate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
(ClassName n) -. a = ClassName $ n <> "-" <> toClassName a


infixl 6 -.
