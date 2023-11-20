{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.View.Render where

import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack, unlines, unwords)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as LE
import Web.View.Element (insertContents)
import Prelude hiding (unlines, unwords)

-- import Debug.Trace
import Web.View.Types


htmlTag :: (Text -> Text) -> Element -> [Text]
htmlTag ind tag =
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
  htmlContent (Node t) = htmlTag ind t
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


indent :: Text -> Text
indent t = "  " <> t


renderText :: forall c. c -> View c () -> Text
renderText c u = intercalate "\n" content
 where
  -- T.intercalate "\n" (content <> style css)
  content :: [Text]
  content = map (unlines . renderContent) $ (.contents) $ runView c addCss

  addCss :: View c ()
  addCss = do
    insertContents [styleElement]
    u

  css :: [Text]
  css = renderCSS $ (.css) $ runView c u

  styleElement :: Content
  styleElement =
    Node $ Element "style" [] [("type", "text/css")] [Text $ intercalate "\n" css]


renderLazyText :: c -> View c () -> L.Text
renderLazyText c = L.fromStrict . renderText c


renderLazyByteString :: c -> View c () -> BL.ByteString
renderLazyByteString c = LE.encodeUtf8 . renderLazyText c


renderContent :: Content -> [Text]
renderContent (Node t) = htmlTag indent t
renderContent (Text t) = [t]
renderContent (Raw t) = [t]


renderCSS :: Map Selector Class -> [Text]
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


showView :: c -> View c () -> Text
showView c v =
  let st = runView c v
   in unlines $ mconcat $ map renderContent st.contents
