{-# LANGUAGE LambdaCase #-}

module Web.UI.Style where

import Data.Function ((&))
import Data.Map qualified as M
import Data.Text (Text)
import Web.UI.Types


-- | Hyphneate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
(ClassName n) -. a = ClassName $ n <> "-" <> toClassName a


infixl 6 -.


-- | Add class attributes. If they already exist, will combine with spaces
modClasses :: [Class] -> Mod
modClasses cx t =
  t{classes = cx : t.classes}


-- | Add a single class attribute.
addClass :: Class -> Mod
addClass c = modClasses [c]


cls :: ClassName -> Class
cls n = Class (selector n) []


prop :: (ToStyleValue val) => Name -> val -> Class -> Class
prop n v c =
  c{properties = M.insert n (toStyleValue v) c.properties}


width :: PxRem -> Mod
width n =
  addClass
    $ cls ("w" -. n)
    & prop "width" n
    & prop @Int "flex-shrink" 0


minWidth :: PxRem -> Mod
minWidth n =
  addClass
    $ cls ("mw" -. n)
    & prop "min-width" n


height :: PxRem -> Mod
height n =
  addClass
    $ cls ("h" -. n)
    & prop "height" n
    & prop @Int "flex-shrink" 0


pad :: Sides PxRem -> Mod
pad (All n) =
  addClass
    $ cls ("pad" -. n)
    & prop "padding" n
pad (Y n) =
  addClass
    $ cls ("pady" -. n)
    & prop "padding-top" n
    & prop "padding-bottom" n
pad (X n) =
  addClass
    $ cls ("padx" -. n)
    & prop "padding-left" n
    & prop "padding-right" n
pad (XY x y) =
  addClass
    $ cls ("pad" -. x -. y)
    & prop "padding-left" x
    & prop "padding-right" x
    & prop "padding-top" y
    & prop "padding-bottom" y
pad (TRBL t r b l) =
  addClass
    $ cls ("pad" -. t -. r -. b -. l)
    & prop "padding-top" t
    & prop "padding-right" r
    & prop "padding-bottom" b
    & prop "padding-left" l


gap :: PxRem -> Mod
gap n = addClass $ cls ("gap" -. n) & prop "gap" n


grow :: Mod
grow = addClass $ cls "grow" & prop @Int "flex-grow" 1


-- | Allow items to become smaller than their contents. This is not the opposite of grow!
collapse :: Mod
collapse = addClass $ cls "collapse" & prop @Int "min-width" 0


fontSize :: PxRem -> Mod
fontSize n = addClass $ cls ("fs" -. n) & prop "font-size" n


-- fontFamily :: Text -> Mod
-- fontFamily t = cls1 $ Class ("font" -. n) [("font-family", pxRem n)]

flexRow :: Mod
flexRow =
  addClass
    $ cls "row"
    & prop @Text "display" "flex"
    & prop @Text "flex-direction" "row"


flexCol :: Mod
flexCol =
  addClass
    $ cls "col"
    & prop @Text "display" "flex"
    & prop @Text "flex-direction" "column"


shadow :: Mod
shadow =
  addClass
    $ cls "shadow"
    & prop @Text "box-shadow" "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)"


bg :: (ToColor c) => c -> Mod
bg c =
  addClass
    $ cls ("bg" -. colorName c)
    & prop "background-color" (colorValue c)


color :: (ToColor c) => c -> Mod
color c = addClass $ cls ("clr" -. colorName c) & prop "color" (colorValue c)


bold :: Mod
bold = addClass $ cls "bold" & prop @Text "font-weight" "bold"


rounded :: PxRem -> Mod
rounded n = addClass $ cls ("rnd" -. n) & prop "border-radius" n


data Display
  = None
  | Block
  deriving (Show, Eq, ToClassName, ToStyleValue)


-- should probably do something higher level than this
display :: Display -> Mod
display d =
  addClass
    $ cls ("dsp" -. d)
    & prop "display" d


border :: Sides PxRem -> Mod
border (All p) =
  addClass
    $ cls ("brd" -. p)
    & prop "border" p
    & prop @Text "border-style" "solid"
border (Y p) =
  addClass
    $ cls ("brdy" -. p)
    & prop "border-top-width" p
    & prop "border-bottom-width" p
border (X p) =
  addClass
    $ cls ("brdx" -. p)
    & prop "border-left-width" p
    & prop "border-right-width" p
border (XY x y) =
  addClass
    $ cls ("brd" -. x -. y)
    & prop "border-right-width" x
    & prop "border-left-width" x
    & prop "border-top-width" y
    & prop "border-bottom-width" y
border (TRBL t r b l) =
  addClass
    $ cls ("brd" -. t -. r -. b -. l)
    & prop "border-top-width" t
    & prop "border-right-width" r
    & prop "border-bottom-width" b
    & prop "border-left-width" l


borderColor :: (ToColor c) => c -> Mod
borderColor c =
  addClass
    $ cls ("brdc" -. colorName c)
    & prop "border-color" (colorValue c)


pointer :: Mod
pointer = addClass $ cls "pointer" & prop @Text "cursor" "pointer"


hover :: Mod -> Mod
hover f = Hover |: f


active :: Mod -> Mod
active f = Active |: f


even :: Mod -> Mod
even f = Even |: f


odd :: Mod -> Mod
odd f = Odd |: f


-- Add a pseudo-class like Hover to your style
(|:) :: Pseudo -> Mod -> Mod
(|:) ps = modClassMod $ \c ->
  c
    { selector = selectorAddPseudo ps c.selector
    }


infixr 9 |:


media :: Media -> Mod -> Mod
media m = modClassMod $ \c ->
  c
    { selector = selectorAddMedia m c.selector
    }


parent :: Text -> Mod -> Mod
parent p = modClassMod $ \c ->
  c
    { selector = selectorAddParent p c.selector
    }


modClassMod :: (Class -> Class) -> Mod -> Mod
modClassMod fc fm el =
  -- apply the function to all classes added by the mod
  -- ignore
  let el' = fm $ Element "none" [] [] []
   in el
        { classes = el.classes <> (map fc <$> el'.classes)
        , attributes = el.attributes <> el'.attributes
        , children = el.children <> el'.children
        }


rgb :: Int -> Int -> Int -> StyleValue
rgb rd gr bl = StyleValue $ mconcat [show rd, " ", show gr, " ", show bl]




truncate :: Mod
truncate =
  addClass
    $ cls "truncate"
    & prop @Text "white-space" "nowrap"
    & prop @Text "overflow" "hidden"
    & prop @Text "text-overflow" "ellipsis"


-- You MUST set the height/width manually when you attempt to transition it
data TransitionProperty
  = Width PxRem
  | Height PxRem
  deriving (Show)


-- what does it need? To know it's calculated height, right?
transition :: Ms -> TransitionProperty -> Mod
transition ms = \case
  (Height n) -> trans "height" n
  (Width n) -> trans "width" n
 where
  trans p px =
    addClass
      $ cls ("t" -. px -. p -. ms)
      & prop "transition-duration" ms
      & prop "transition-property" p
      & prop p px


data Align
  = Center
  deriving (Show, ToClassName, ToStyleValue)


textAlign :: Align -> Mod
textAlign a =
  addClass
    $ cls ("ta" -. a)
    & prop "text-align" a
