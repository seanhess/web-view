{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.UI.Style where

import Data.Text (Text, pack, toLower)
import Data.Text qualified as T
import Web.UI.Types

-- Px, converted to Rem
type PxRem = Int

class ToClassName a where
  toClassName :: a -> Text

instance ToClassName Int where
  toClassName = T.pack . show

instance ToClassName Text where
  toClassName = id

instance {-# OVERLAPS #-} (ToColor a) => ToClassName a where
  toClassName = colorName

-- | Hyphneate classnames
(-.) :: (ToClassName a) => ClassName -> a -> ClassName
(ClassName ps n) -. a = ClassName ps $ n <> "-" <> toClassName a

infixl 6 -.

-- | Add class attributes. If they already exist, will combine with spaces
cls :: [Class] -> Mod
cls cx t =
  t{classes = cx : t.classes}

-- | Add a single class attribute.
cls1 :: Class -> Mod
cls1 c = cls [c]

width :: PxRem -> Mod
width n =
  cls1
    $ Class
      ("w" -. n)
      [("width", pxRem n), ("flex-shrink", "0")]

pad :: PxRem -> Mod
pad n =
  cls1
    $ Class
      ("pad" -. n)
      [("padding", pxRem n)]

padY :: PxRem -> Mod
padY n =
  cls1
    $ Class
      ("pady" -. n)
      [ ("padding-top", pxRem n)
      , ("padding-bottom", pxRem n)
      ]

padX :: PxRem -> Mod
padX n =
  cls1
    $ Class
      ("padx" -. n)
      [ ("padding-left", pxRem n)
      , ("padding-right", pxRem n)
      ]

gap :: PxRem -> Mod
gap n =
  cls1
    $ Class
      ("gap" -. n)
      [("gap", pxRem n)]

grow :: Mod
grow =
  cls1
    $ Class
      "grow"
      [("flex-grow", "1")]

fontSize :: PxRem -> Mod
fontSize n = cls1 $ Class ("fs" -. n) [("font-size", pxRem n)]

-- fontFamily :: Text -> Mod
-- fontFamily t = cls1 $ Class ("font" -. n) [("font-family", pxRem n)]

flexRow :: Mod
flexRow = cls1 $ Class "row" [("display", "flex"), ("flex-direction", "row")]

flexCol :: Mod
flexCol = cls1 $ Class "col" [("display", "flex"), ("flex-direction", "column")]

shadow :: Mod
shadow = cls1 $ Class "shadow" [("box-shadow", "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")]

bg :: (ToColor c) => c -> Mod
bg c =
  cls1
    $ Class
      ("bg" -. colorName c)
      [ ("background-color", Hex $ colorValue c)
      ]

color :: (ToColor c) => c -> Mod
color c =
  cls1
    $ Class
      ("clr" -. colorName c)
      [("color", Hex $ colorValue c)]

bold :: Mod
bold = cls1 $ Class "bold" [("font-weight", "bold")]

rounded :: PxRem -> Mod
rounded n = cls1 $ Class ("rnd" -. n) [("border-radius", pxRem n)]

border :: PxRem -> Mod
border p =
  cls1
    $ Class
      ("brd" -. p)
      [ ("border", pxRem p)
      , ("border-style", "solid")
      ]

borderY :: PxRem -> Mod
borderY p =
  cls1
    $ Class
      ("brdy" -. p)
      [ ("border-top", pxRem p)
      , ("border-bottom", pxRem p)
      , ("border-style", "solid")
      ]

borderX :: PxRem -> Mod
borderX p =
  cls1
    $ Class
      ("brdx" -. p)
      [ ("border-left", pxRem p)
      , ("border-right", pxRem p)
      , ("border-style", "solid")
      ]

border' :: TRBL PxRem -> Mod
border' s =
  cls1
    $ Class
      ("brdtrbl" -. s.top -. s.right -. s.bottom -. s.left)
      [ ("border-top", pxRem s.top)
      , ("border-right", pxRem s.right)
      , ("border-bottom", pxRem s.bottom)
      , ("border-left", pxRem s.left)
      , ("border-style", "solid")
      ]

borderR :: PxRem -> Mod
borderR p =
  cls1
    $ Class
      ("brdr" -. p)
      [ ("border-right", pxRem p)
      , ("border-style", "solid")
      ]

borderColor :: (ToColor c) => c -> Mod
borderColor c =
  cls1
    $ Class
      ("brdc" -. colorName c)
      [("border-color", Hex $ colorValue c)]

pointer :: Mod
pointer = cls1 $ Class "pointer" [("cursor", "pointer")]

hover :: Pseudo
hover = Hover

active :: Pseudo
active = Active

-- Add a pseudo-class like Hover to your style
(|:) :: Pseudo -> Mod -> Mod
(|:) ps f t =
  let t' = f t
   in case t'.classes of
        [] -> t'
        (new : cx) ->
          -- this is a bit of a hack
          -- we know that the last class added is the one to be modified
          t'{classes = map addPseudo new : cx}
 where
  addPseudo (Class (ClassName _ n) v) =
    Class (ClassName (Just ps) n) v

infixr 9 |:

pxRem :: PxRem -> StyleValue
pxRem 0 = Px 0
pxRem 1 = Px 1
pxRem n = Rem (fromIntegral n / 16.0)

rgb :: Int -> Int -> Int -> StyleValue
rgb rd gr bl = RGB $ mconcat [show rd, " ", show gr, " ", show bl]

class ToColor a where
  colorValue :: a -> HexColor
  colorName :: a -> Text
  default colorName :: (Show a) => a -> Text
  colorName = toLower . pack . show

instance ToColor HexColor where
  colorValue c = c
  colorName (HexColor a) = T.dropWhile (== '#') a

-- prefix :: Text -> Mod -> Mod
-- prefix p f t =
--   let t' = f t
--    in case t'.classes of
--         [] -> t'
--         (new : cx) ->
--           -- this is a bit of a hack
--           -- we know that the last class added is the one to be modified
--           t'{classes = map addPrefix new : cx}
--  where
--   addPrefix (Class (ClassName ps n) v) =
--     Class (ClassName ps (Just p) n) v
