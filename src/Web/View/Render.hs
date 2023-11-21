{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.View.Render where

import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack, toLower, unlines, unwords)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as LE
import Web.View.View (View, ViewState (..), runView, viewInsertContents)
import Prelude hiding (unlines, unwords)

-- import Debug.Trace
import Web.View.Types


renderText :: c -> View c () -> Text
renderText c u = intercalate "\n" content
 where
  -- T.intercalate "\n" (content <> style css)
  content :: [Text]
  content = map (unlines . renderContent) . (.contents) $ runView c addCss

  addCss = do
    viewInsertContents [styleElement]
    u

  css :: [Text]
  css = renderCSS $ (.css) $ runView c u

  styleElement :: Content
  styleElement =
    Node $ Element "style" [] [("type", "text/css")] [Text $ intercalate "\n" css]

  renderContent :: Content -> [Text]
  renderContent (Node t) = renderTag indent t
  renderContent (Text t) = [t]
  renderContent (Raw t) = [t]


renderLazyText :: c -> View c () -> L.Text
renderLazyText c = L.fromStrict . renderText c


renderLazyByteString :: c -> View c () -> BL.ByteString
renderLazyByteString c = LE.encodeUtf8 . renderLazyText c


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
      [open <> htmlAtts (flatAttributes tag) <> ">" <> t <> close]
    _ ->
      mconcat
        [ [open <> htmlAtts (flatAttributes tag) <> ">"]
        , ind <$> htmlChildren tag.children
        , [close]
        ]
 where
  open = "<" <> tag.name
  close = "</" <> tag.name <> ">"

  htmlContent :: Content -> [Text]
  htmlContent (Node t) = renderTag ind t
  htmlContent (Text t) = [t]
  htmlContent (Raw t) = [t]

  htmlChildren :: [Content] -> [Text]
  htmlChildren cts =
    mconcat
      $ fmap htmlContent cts

  htmlAtts :: FlatAttributes -> Text
  htmlAtts (FlatAttributes []) = ""
  htmlAtts (FlatAttributes as) =
    " "
      <> unwords (map htmlAtt $ M.toList as)
   where
    htmlAtt (k, v) =
      k <> "=" <> "'" <> v <> "'"


renderCSS :: CSS -> [Text]
renderCSS = map renderClass . M.elems
 where
  renderClass :: Class -> Text
  renderClass c =
    let sel = selectorText c.selector
        props = intercalate "; " (map renderProp $ M.toList c.properties)
     in [i|#{sel} { #{props} }|] & addMedia c.selector.media

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
  FlatAttributes
    $ addClass (mconcat t.classes) t.attributes
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
