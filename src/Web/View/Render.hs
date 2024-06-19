{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Web.View.Render where

import Data.ByteString.Lazy qualified as BL
import Data.Foldable (forM_, toList)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack, toLower, unlines, unwords)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as LE
import Web.View.Types
import Web.View.View (View, ViewState (..), runView, viewInsertContents)
import HTMLEntities.Text qualified as HE
import Prelude hiding (unlines, unwords)

{- | Renders a 'View' as HTML with embedded CSS class definitions

>>> renderText $ el bold "Hello"
<style type='text/css'>.bold { font-weight:bold }</style>
<div class='bold'>Hello</div>
-}
renderText :: View () () -> Text
renderText = renderText' ()


renderLazyText :: View () () -> L.Text
renderLazyText = L.fromStrict . renderText


renderLazyByteString :: View () () -> BL.ByteString
renderLazyByteString = LE.encodeUtf8 . renderLazyText


{- | Render with the specified view context

> renderText' () $ el bold "Hello"
-}
renderText' :: c -> View c () -> Text
renderText' c u = intercalate "\n" content
 where
  -- T.intercalate "\n" (content <> style css)
  content :: [Text]
  content = map (unlines . renderContent indent) . (.contents) $ runView c addCss

  addCss = do
    forM_ styleElement $ viewInsertContents . pure
    u

  css :: Maybe (NonEmpty Text)
  css = nonEmpty $ renderCSS $ (.css) $ runView c u

  styleElement :: Maybe Content
  styleElement = case css of
    Just css' -> Just $ Node $ Element "style" (Attributes [] [("type", "text/css")]) [Text $ intercalate "\n" $ toList css']
    Nothing -> Nothing


renderContent :: (Text -> Text) -> Content -> [Text]
renderContent ind (Node t) = renderTag ind t
renderContent _ (Text t) = [HE.text t]
renderContent _ (Raw t) = [t]


renderTag :: (Text -> Text) -> Element -> [Text]
renderTag ind tag =
  case tag.children of
    [] ->
      -- auto closing creates a bug in chrome. An auto-closed div
      -- absorbs the next children
      [open <> htmlAtts (flatAttributes tag) <> ">" <> close]
    -- single text node
    [Text t] ->
      -- SINGLE text node, just display it indented
      [open <> htmlAtts (flatAttributes tag) <> ">" <> HE.text t <> close]
    _ ->
      mconcat
        [ [open <> htmlAtts (flatAttributes tag) <> ">"]
        , ind <$> htmlChildren tag.children
        , [close]
        ]
 where
  open = "<" <> tag.name
  close = "</" <> tag.name <> ">"

  htmlChildren :: [Content] -> [Text]
  htmlChildren cts =
    mconcat $
      fmap (renderContent ind) cts

  htmlAtts :: FlatAttributes -> Text
  htmlAtts (FlatAttributes []) = ""
  htmlAtts (FlatAttributes as) =
    " "
      <> unwords (map htmlAtt $ M.toList as)
   where
    htmlAtt (k, v) =
      k <> "=" <> "'" <> HE.text v <> "'"


renderCSS :: CSS -> [Text]
renderCSS = mapMaybe renderClass . M.elems
 where
  renderClass :: Class -> Maybe Text
  renderClass c | M.null c.properties = Nothing
  renderClass c =
    let sel = selectorText c.selector
        props = intercalate "; " (map renderProp $ M.toList c.properties)
     in Just $ [i|#{sel} { #{props} }|] & addMedia c.selector.media

  addMedia Nothing css = css
  addMedia (Just m) css =
    let mc = mediaCriteria m
     in [i|@media #{mc} { #{css} }|]

  mediaCriteria :: Media -> Text
  mediaCriteria (MinWidth n) = [i|(min-width: #{n}px)|]
  mediaCriteria (MaxWidth n) = [i|(max-width: #{n}px)|]

  renderProp :: (Text, StyleValue) -> Text
  renderProp (p, cv) = p <> ":" <> renderStyle cv

  renderStyle :: StyleValue -> Text
  renderStyle (StyleValue v) = pack v


indent :: Text -> Text
indent t = "  " <> t


-- | The css selector for this style
selectorText :: Selector -> Text
selectorText s =
  parent s.parent <> "." <> addPseudo s.pseudo (classNameElementText s.media s.parent Nothing s.className)
 where
  parent Nothing = ""
  parent (Just p) = "." <> p <> " "

  addPseudo Nothing c = c
  addPseudo (Just p) c =
    pseudoText p <> "\\:" <> c <> ":" <> pseudoSuffix p

  pseudoSuffix :: Pseudo -> Text
  pseudoSuffix Even = "nth-child(even)"
  pseudoSuffix Odd = "nth-child(odd)"
  pseudoSuffix p = pseudoText p


-- | The class name as it appears in the element
classNameElementText :: Maybe Media -> Maybe Text -> Maybe Pseudo -> ClassName -> Text
classNameElementText mm mp mps c =
  addMedia mm . addPseudo mps . addParent mp $ c.text
 where
  addParent Nothing cn = cn
  addParent (Just p) cn = p <> "-" <> cn

  addPseudo :: Maybe Pseudo -> Text -> Text
  addPseudo Nothing cn = cn
  addPseudo (Just p) cn =
    pseudoText p <> ":" <> cn

  addMedia :: Maybe Media -> Text -> Text
  addMedia Nothing cn = cn
  addMedia (Just (MinWidth n)) cn =
    [i|mmnw#{n}-#{cn}|]
  addMedia (Just (MaxWidth n)) cn =
    [i|mmxw#{n}-#{cn}|]


pseudoText :: Pseudo -> Text
pseudoText p = toLower $ pack $ show p


-- | The 'Web.View.Types.Attributes' for an element, inclusive of class.
flatAttributes :: Element -> FlatAttributes
flatAttributes t =
  FlatAttributes $
    addClass t.attributes.classes t.attributes.other
 where
  addClass [] atts = atts
  addClass cx atts = M.insert "class" (classAttValue cx) atts

  classAttValue :: [Class] -> Text
  classAttValue cx =
    unwords $ fmap (\c -> classNameElementText c.selector.media c.selector.parent c.selector.pseudo c.selector.className) cx

-- showView :: c -> View c () -> Text
-- showView c v =
--   let st = runView c v
--    in unlines $ mconcat $ map renderContent st.contents
