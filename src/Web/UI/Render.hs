{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.UI.Render where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
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
      [open <> htmlAtts (flatAttributes tag) <> ">" <> t <> close]
    _ ->
      mconcat
        [ [open <> htmlAtts (flatAttributes tag) <> ">"]
        , ind $ htmlChildren tag.children
        , [close]
        ]
 where
  open = "<" <> tag.name
  close = "</" <> tag.name <> ">"

  htmlContent :: Content -> [Text]
  htmlContent (Node t) = htmlTag ind t
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

indentation :: Text
indentation = "  "

indentAll :: [Text] -> [Text]
indentAll = fmap indent

indent :: Text -> Text
indent t = indentation <> t

noIndent :: Indent -> [Text] -> [Text]
noIndent _ ts = ts

renderText :: View a () -> Text
renderText u = T.intercalate "\n" content
 where
  -- T.intercalate "\n" (content <> style css)
  content = map renderContent $ (.contents) $ runView $ do
    u
    insertContents $ (.contents) $ runView styles
  css = renderCSS $ (.classStyles) $ runView u
  styles = style (T.intercalate "\n" css)

renderLazyText :: View a () -> L.Text
renderLazyText = L.fromStrict . renderText

renderContent :: Content -> Text
renderContent (Node d) = T.unlines $ htmlTag indentAll d
renderContent (Text t) = t

showView :: View a () -> Text
showView v = T.unlines $ mconcat $ map showContent $ (.contents) $ runView v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag indentAll t
showContent (Text t) = [t]

renderCSS :: Map ClassName (Map Name StyleValue) -> [Text]
renderCSS m = map renderClass $ toClasses m
 where
  toClasses = map toClass . M.toList
  toClass (n, p) = Class n p

  renderClass :: Class -> Text
  renderClass (Class n p) =
    "." <> classNameSelector n <> " " <> "{" <> T.intercalate "; " (map renderProp $ M.toList p) <> "}"

  renderProp :: (Text, StyleValue) -> Text
  renderProp (p, cv) = p <> ":" <> renderStyle cv

renderStyle :: StyleValue -> Text
renderStyle v = pack $ show v

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
