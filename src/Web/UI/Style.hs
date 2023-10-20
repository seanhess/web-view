{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.UI.Style where

import Data.Map qualified as M
import Data.Text (Text, pack, toLower)
import Data.Text qualified as T
import Web.UI.Types


-- Px, converted to Rem
type PxRem = Int


class ToClassName a where
  toClassName :: a -> Text
  default toClassName :: (Show a) => a -> Text
  toClassName = T.toLower . T.pack . show


instance ToClassName Int


instance ToClassName Text where
  toClassName = id


instance {-# OVERLAPS #-} (ToColor a) => ToClassName a where
  toClassName = colorName


-- | Hyphneate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
(ClassName n) -. a = ClassName $ n <> "-" <> toClassName a


infixl 6 -.


-- | Add class attributes. If they already exist, will combine with spaces
modClasses :: [Class] -> Mod
modClasses cx t =
  t{classes = cx : t.classes}


-- | Add a single class attribute.
cls1 :: ClassName -> [(Name, StyleValue)] -> Mod
cls1 cn ss = modClasses [Class (selector cn) (M.fromList ss)]


width :: PxRem -> Mod
width n = cls1 ("w" -. n) [("width", pxRem n), ("flex-shrink", "0")]


minWidth :: PxRem -> Mod
minWidth n = cls1 ("mw" -. n) [("min-width", pxRem n)]


height :: PxRem -> Mod
height n = cls1 ("h" -. n) [("height", pxRem n), ("flex-shrink", "0")]


pad :: Sides PxRem -> Mod
pad (All n) = cls1 ("pad" -. n) [("padding", pxRem n)]
pad (Y n) = cls1 ("pady" -. n) [("padding-top", pxRem n), ("padding-bottom", pxRem n)]
pad (X n) = cls1 ("padx" -. n) [("padding-left", pxRem n), ("padding-right", pxRem n)]
pad (XY x y) = cls1 ("pad" -. x -. y) [("padding-left", pxRem x), ("padding-right", pxRem x), ("padding-top", pxRem y), ("padding-bottom", pxRem y)]
pad (TRBL t r b l) = cls1 ("pad" -. t -. r -. b -. l) [("padding-top", pxRem t), ("padding-right", pxRem r), ("padding-bottom", pxRem b), ("padding-left", pxRem l)]


gap :: PxRem -> Mod
gap n = cls1 ("gap" -. n) [("gap", pxRem n)]


grow :: Mod
grow = cls1 "grow" [("flex-grow", "1")]


-- | Allow items to become smaller than their contents. This is not the opposite of grow!
collapse :: Mod
collapse = cls1 "collapse" [("min-width", "0")]


fontSize :: PxRem -> Mod
fontSize n = cls1 ("fs" -. n) [("font-size", pxRem n)]


-- fontFamily :: Text -> Mod
-- fontFamily t = cls1 $ Class ("font" -. n) [("font-family", pxRem n)]

flexRow :: Mod
flexRow = cls1 "row" [("display", "flex"), ("flex-direction", "row")]


flexCol :: Mod
flexCol = cls1 "col" [("display", "flex"), ("flex-direction", "column")]


shadow :: Mod
shadow =
  cls1
    "shadow"
    [("box-shadow", "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")]


bg :: (ToColor c) => c -> Mod
bg c = cls1 ("bg" -. colorName c) [("background-color", Hex $ colorValue c)]


color :: (ToColor c) => c -> Mod
color c = cls1 ("clr" -. colorName c) [("color", Hex $ colorValue c)]


bold :: Mod
bold = cls1 "bold" [("font-weight", "bold")]


rounded :: PxRem -> Mod
rounded n = cls1 ("rnd" -. n) [("border-radius", pxRem n)]


data Display
  = None
  | Block
  deriving (Show, Eq, ToClassName)


-- should probably do something higher level than this
display :: Display -> Mod
display d = cls1 ("dsp" -. d) [("display", val d)]
 where
  val None = "none"
  val Block = "block"


border :: Sides PxRem -> Mod
border (All p) = cls1 ("brd" -. p) [("border", pxRem p), ("border-style", "solid")]
border (Y p) =
  cls1
    ("brdy" -. p)
    [ ("border-top-width", pxRem p)
    , ("border-bottom-width", pxRem p)
    ]
border (X p) =
  cls1
    ("brdx" -. p)
    [ ("border-left-width", pxRem p)
    , ("border-right-width", pxRem p)
    ]
border (XY x y) =
  cls1
    ("brd" -. x -. y)
    [ ("border-top-width", pxRem y)
    , ("border-right-width", pxRem x)
    , ("border-bottom-width", pxRem y)
    , ("border-left-width", pxRem x)
    ]
border (TRBL t r b l) =
  cls1
    ("brd" -. t -. r -. b -. l)
    [ ("border-top-width", pxRem t)
    , ("border-right-width", pxRem r)
    , ("border-bottom-width", pxRem b)
    , ("border-left-width", pxRem l)
    ]


borderColor :: (ToColor c) => c -> Mod
borderColor c =
  cls1
    ("brdc" -. colorName c)
    [("border-color", Hex $ colorValue c)]


pointer :: Mod
pointer = cls1 "pointer" [("cursor", "pointer")]


hover :: Pseudo
hover = Hover


active :: Pseudo
active = Active


-- Add a pseudo-class like Hover to your style
(|:) :: Pseudo -> Mod -> Mod
(|:) ps = modLastClasses $ \c ->
  c
    { selector = selectorAddPseudo ps c.selector
    }


infixr 9 |:


parent :: Text -> Mod -> Mod
parent p = modLastClasses $ \c ->
  c
    { selector = selectorAddParent p c.selector
    }


modLastClasses :: (Class -> Class) -> Mod -> Mod
modLastClasses fc fm el =
  let el' = fm el
   in case el'.classes of
        [] -> el'
        (new : cx) ->
          -- this is a bit of a hack
          -- we know that the last class added is the one to be modified
          el'{classes = map fc new : cx}


pxRem :: PxRem -> StyleValue
pxRem 0 = Px 0
pxRem 1 = Px 1
pxRem n = Rem (fromIntegral n / 16.0)


rgb :: Int -> Int -> Int -> StyleValue
rgb rd gr bl = RGB $ mconcat [pack (show rd), " ", pack (show gr), " ", pack (show bl)]


class ToColor a where
  colorValue :: a -> HexColor
  colorName :: a -> Text
  default colorName :: (Show a) => a -> Text
  colorName = toLower . pack . show


instance ToColor HexColor where
  colorValue c = c
  colorName (HexColor a) = T.dropWhile (== '#') a


-- | Sets the root layout so filling columns makes sense
rootLayout :: Mod
rootLayout =
  cls1
    "layout"
    -- [ ("white-space", "pre")
    [ ("width", "100%")
    , ("height", "100%")
    , ("min-height", "100%")
    , ("z-index", "0")
    ]
    . flexCol


truncate :: Mod
truncate =
  cls1
    "truncate"
    [ ("white-space", "nowrap")
    , ("overflow", "hidden")
    , ("text-overflow", "ellipsis")
    ]


type Seconds = Float


data TransitionProperty
  = Width
  | Height
  deriving (Show)


transition :: TransitionProperty -> Float -> Mod
transition p n =
  cls1
    "tt"
    [ ("transition-duration", Value (pack (show n) <> "s"))
    , ("transition-property", Value $ T.toLower $ pack $ show p)
    ]
