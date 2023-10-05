{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.UI.Render where

import Data.ByteString.Lazy qualified as BL
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Web.UI.Element (insertContents, style)

-- import Debug.Trace
import Web.UI.Types

type Indent = Int

htmlTag :: ([Text] -> [Text]) -> Element -> [Text]
htmlTag ind tag =
  case tag.children of
    [] ->
      -- autoClosing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [open <> htmlAtts (flatAttributes tag) <> ">" <> close]
    -- single text node
    [Text t] ->
      -- SINGLE text node, just display it indented
      [open <> htmlAtts (flatAttributes tag) <> ">" <> L.fromStrict t <> close]
    _ ->
      mconcat
        [ [open <> htmlAtts (flatAttributes tag) <> ">"]
        , ind $ htmlChildren tag.children
        , [close]
        ]
 where
  open = "<" <> L.fromStrict tag.name
  close = "</" <> L.fromStrict tag.name <> ">"

  htmlContent :: Content -> [Text]
  htmlContent (Node t) = htmlTag ind t
  htmlContent (Text t) = [L.fromStrict t]

  htmlChildren :: [Content] -> [Text]
  htmlChildren cts =
    mconcat
      $ fmap htmlContent cts

  htmlAtts :: FlatAttributes -> Text
  htmlAtts (FlatAttributes []) = ""
  htmlAtts (FlatAttributes as) =
    " "
      <> L.intercalate " " (map htmlAtt $ M.toList as)
   where
    htmlAtt (k, v) =
      L.fromStrict $ k <> "=" <> "'" <> v <> "'"

indentation :: Text
indentation = "  "

indentAll :: [Text] -> [Text]
indentAll = fmap indent

indent :: Text -> Text
indent t = indentation <> t

noIndent :: Indent -> [Text] -> [Text]
noIndent _ ts = ts

renderLazyText :: View () -> Text
renderLazyText u = L.intercalate "\n" content
 where
  -- T.intercalate "\n" (content <> style css)
  content = map renderContent $ (.contents) $ runView $ do
    insertContents $ (.contents) $ runView styles
    u

  css :: [T.Text]
  css = renderCSS $ (.classStyles) $ runView u

  styles :: View ()
  styles = style (T.intercalate "\n" css)

renderText :: View () -> T.Text
renderText = L.toStrict . renderLazyText

renderLazyByteString :: View () -> BL.ByteString
renderLazyByteString = L.encodeUtf8 . renderLazyText

renderContent :: Content -> Text
renderContent (Node d) = L.unlines $ htmlTag indentAll d
renderContent (Text t) = L.fromStrict t

renderCSS :: Map ClassName (Map Name StyleValue) -> [T.Text]
renderCSS m = map renderClass $ toClasses m
 where
  toClasses = map toClass . M.toList
  toClass (n, p) = Class n p

  renderClass :: Class -> T.Text
  renderClass (Class n p) =
    "." <> classNameSelector n <> " " <> "{" <> T.intercalate "; " (map renderProp $ M.toList p) <> "}"

  renderProp :: (T.Text, StyleValue) -> T.Text
  renderProp (p, cv) = p <> ":" <> renderStyle cv

renderStyle :: StyleValue -> T.Text
renderStyle v = T.pack $ show v

flatAttributes :: Element -> FlatAttributes
flatAttributes t =
  FlatAttributes
    $ addClass (mconcat t.classes) t.attributes
 where
  addClass [] atts = atts
  addClass cx atts = M.insert "class" (classAttValue cx) atts

  classAttValue :: [Class] -> T.Text
  classAttValue cx =
    T.intercalate " " $ map className cx

-- | Attributes that include classes
newtype FlatAttributes = FlatAttributes Attributes

showView :: View () -> Text
showView v = L.unlines $ mconcat $ map showContent $ (.contents) $ runView v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag indentAll t
showContent (Text t) = [L.fromStrict t]
