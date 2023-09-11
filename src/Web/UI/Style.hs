module Web.UI.Style where

import Data.Text (Text, pack)
import Web.UI.Types

-- Px, converted to Rem
type PxRem = Int

-- | Hyphneate classnames
(-.) :: Show a => Text -> a -> ClassName
n -. a = ClassName Nothing $ n <> "-" <> pack (show a)

-- | Add class attributes. If they already exist, will combine with spaces
cls :: [Class] -> Mod Class
cls cx t =
  t{classes = cx : t.classes}

-- | Add a single class attribute.
cls1 :: Class -> Mod Class
cls1 c = cls [c]

pad :: PxRem -> Mod Class
pad n =
  cls1 $
    Class
      ("pad" -. n)
      [("padding", pxRem n)]

padY :: PxRem -> Mod Class
padY n =
  cls1 $
    Class
      ("pady" -. n)
      [ ("padding-top", pxRem n)
      , ("padding-bottom", pxRem n)
      ]

padX :: PxRem -> Mod Class
padX n =
  cls1 $
    Class
      ("padx" -. n)
      [ ("padding-left", pxRem n)
      , ("padding-right", pxRem n)
      ]

gap :: PxRem -> Mod Class
gap n =
  cls1 $
    Class
      ("gap" -. n)
      [("gap", pxRem n)]

grow :: Mod Class
grow =
  cls1 $
    Class
      "grow"
      [("flex-grow", "1")]

flexRow :: Mod Class
flexRow = cls1 $ Class "row" [("display", "flex"), ("flex-direction", "row")]

flexCol :: Mod Class
flexCol = cls1 $ Class "col" [("display", "flex"), ("flex-direction", "column")]

shadow :: Mod Class
shadow = cls1 $ Class "shadow" [("box-shadow", "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")]

bg :: ToColor c => c -> Mod Class
bg c =
  cls1 $
    Class
      (ClassName Nothing $ "bg-" <> colorName c)
      [ ("background-color", colorValue c)
      ]

color :: ToColor c => c -> Mod Class
color c =
  cls1 $
    Class
      (ClassName Nothing $ "clr-" <> colorName c)
      [("color", colorValue c)]

bold :: Mod Class
bold = cls1 $ Class "bold" [("font-weight", "bold")]

border :: PxRem -> Mod Class
border p =
  cls1 $
    Class
      ("border" -. p)
      [ ("border", pxRem p)
      , ("border-style", "solid")
      ]

pointer :: Mod Class
pointer = cls1 $ Class "pointer" [("cursor", "pointer")]

hover :: Pseudo
hover = Hover

-- Add a pseudo-class like Hover to your style
(|:) :: Pseudo -> Mod Class -> Mod Class
(|:) p f t =
  let t' = f t
   in case t'.classes of
        [] -> t'
        (new : cx) ->
          -- this is a bit of a hack
          -- we know that the last class added is the one to be modified
          t'{classes = map prefixClass new : cx}
 where
  prefixClass (Class (ClassName _ n) v) =
    Class (ClassName (Just p) n) v

infixr 9 |:

pxRem :: PxRem -> Style
pxRem 0 = Style Px "0"
pxRem 1 = Style Px "1"
pxRem n = Style Rem (show (fromIntegral n / 16.0 :: Float))

rgb :: Int -> Int -> Int -> Style
rgb rd gr bl = Style RGB $ mconcat [show rd, " ", show gr, " ", show bl]

class ToColor a where
  colorValue :: a -> Style
  colorName :: a -> Text

newtype HexColor = HexColor String

instance ToColor HexColor where
  colorValue (HexColor a) = Style Hex a
  colorName (HexColor a) = pack a
