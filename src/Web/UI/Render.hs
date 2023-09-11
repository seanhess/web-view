{-# LANGUAGE OverloadedLists #-}

module Web.UI.Render where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Web.UI.Types

type Indent = Int

htmlTag :: (Indent -> [Text] -> [Text]) -> Indent -> Element -> [Text]
htmlTag indent' i tag =
  case tag.children of
    [] ->
      -- autoClosing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [open <> htmlAtts (flatAttributes tag) <> ">" <> close]
    [Text t] ->
      [open <> htmlAtts (flatAttributes tag) <> ">" <> t <> close]
    _ ->
      mconcat
        [ [open <> htmlAtts (flatAttributes tag) <> ">"]
        , indent' (i + 1) $ htmlChildren tag.children
        , [close]
        ]
 where
  open = "<" <> tag.name
  close = "</" <> tag.name <> ">"

  htmlContent :: Content -> [Text]
  htmlContent (Node t) = htmlTag indent' i t
  htmlContent (Text t) = [t]

  htmlChildren :: [Content] -> [Text]
  htmlChildren cts =
    mconcat $
      fmap htmlContent cts

  htmlAtts :: FlatAttributes -> Text
  htmlAtts (FlatAttributes []) = ""
  htmlAtts (FlatAttributes as) =
    " "
      <> T.intercalate " " (map htmlAtt $ M.toList as)
   where
    htmlAtt (k, v) =
      k <> "=" <> "'" <> v <> "'"

indent :: Indent -> [Text] -> [Text]
indent i' = fmap ind
 where
  ind :: Text -> Text
  ind t = T.replicate (2 * i') " " <> t

noIndent :: Indent -> [Text] -> [Text]
noIndent _ ts = ts

renderText :: View Document () -> Text
renderText u =
  case viewContents u of
    [Node d] -> mconcat $ htmlTag noIndent 0 d
    _ -> error "Should not be possible to create document with multiple tags. Use document function."

renderLazyText :: View Document () -> L.Text
renderLazyText = L.fromStrict . renderText

showView :: View a () -> Text
showView v = T.unlines $ mconcat $ map showContent $ viewContents v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag indent 0 t
showContent (Text t) = [t]

renderCSS :: Map ClassName ClassProps -> [Text]
renderCSS m = map renderClass $ toClasses m
 where
  toClasses = map toClass . M.toList
  toClass (n, p) = Class n p

  renderClass :: Class -> Text
  renderClass (Class n p) =
    "." <> classNameSelector n <> " " <> "{" <> T.intercalate "; " (map renderProp $ M.toList p) <> "}"

  renderProp :: (Text, Style) -> Text
  renderProp (p, cv) = p <> ":" <> renderStyle cv

renderStyle :: Style -> Text
renderStyle (Style v u) = pack $ unitsValue v u

flatAttributes :: Element -> FlatAttributes
flatAttributes t =
  FlatAttributes $
    addClass (mconcat t.classes) t.attributes
 where
  addClass [] atts = atts
  addClass cx atts = M.insert "class" (classAttValue cx) atts

  classAttValue :: [Class] -> Text
  classAttValue cx =
    T.intercalate " " $ map className cx

-- | Attributes that include classes
newtype FlatAttributes = FlatAttributes Attributes
