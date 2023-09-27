module Web.UI.Element where

import Control.Monad.State.Strict (modify)
import Control.Monad.Writer.Strict
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Web.UI.Style
import Web.UI.Types
import Web.UI.Url

tag :: Text -> Mod -> View () -> View ()
tag nm f ct = do
  let st = runView ct
  let elm = f $ Element nm [] [] st.contents
  addContent $ Node elm
  addClasses $ classList $ st.classStyles
  addClasses $ mconcat $ elm.classes

addClasses :: [Class] -> View ()
addClasses clss = do
  modify $ \vs ->
    vs
      { classStyles = foldr addClsDef vs.classStyles clss
      }
 where
  addClsDef :: Class -> Map ClassName (Map Name StyleValue) -> Map ClassName (Map Name StyleValue)
  addClsDef c = M.insert c.className c.classProperties

addContent :: Content -> View ()
addContent ct = do
  modify $ \vs ->
    vs
      { contents = vs.contents <> [ct]
      }

-- Inserts into first child
insertContents :: [Content] -> View ()
insertContents cs = do
  modify $ \vs -> vs{contents = insert vs.contents}
 where
  insert [Node e] = [Node $ insertEl e]
  insert cnt = cnt <> cs
  insertEl e = e{children = e.children <> cs}

classList :: Map ClassName (Map Name StyleValue) -> [Class]
classList m = map (uncurry Class) $ M.toList m

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> Mod
att n v t = t{attributes = M.insert n v t.attributes}

-- | A basic element
el :: Mod -> View () -> View ()
el = tag "div"

-- | A basic element, with no modifiers
el_ :: View () -> View ()
el_ = tag "div" id

button :: Mod -> View () -> View ()
button = tag "button"

-- | Convert from text directly to view. You should not have to use this. Use `text` instead
data Head

data Base
data Doc

text :: Text -> View ()
text t = addContent $ Text t

none :: View ()
none = pure ()

meta :: Mod -> View ()
meta f = tag "meta" f ("" :: View ())

title :: Text -> View ()
title = tag "title" id . text

head :: View () -> View ()
head = tag "head" id

html :: View () -> View ()
html = tag "html" id

body :: View () -> View ()
body = tag "body" id

row :: Mod -> View () -> View ()
row f = el (flexRow . f)

row_ :: View () -> View ()
row_ = row id

col :: Mod -> View () -> View ()
col f = el (flexCol . f)

col_ :: View () -> View ()
col_ = col id

space :: View ()
space = el grow $ pure ()

label :: Mod -> View () -> View ()
label = tag "label"

form :: Mod -> View () -> View ()
form f = tag "form" (f . flexCol)

input :: Mod -> View ()
input m = tag "input" (m . att "type" "text") none

name :: Text -> Mod
name = att "name"

value :: Text -> Mod
value = att "value"

script :: Text -> View ()
-- script (Code code) = tag "script" (att "type" "text/javascript") $ fromText code
script src = tag "script" (att "type" "text/javascript" . att "src" src) none

style :: Text -> View ()
style cnt = tag "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")

stylesheet :: Text -> View ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none

table :: Mod -> [dt] -> Writer [TableColumn dt] () -> View ()
table f dts wcs = do
  let cols = execWriter wcs
  tag "table" (f . borderCollapse) $ do
    tag "thead" id $ do
      tag "tr" id $ do
        forM_ cols $ \(TableColumn mtd h _) -> do
          tag "th" mtd h
    tag "tbody" id $ do
      forM_ dts $ \dt -> do
        tag "tr" id $ do
          forM_ cols $ \(TableColumn mtd _ view) -> do
            tag "td" mtd $ view dt
 where
  borderCollapse :: Mod
  borderCollapse = cls1 $ Class "brd-cl" [("border-collapse", "collapse")]

tcol :: Mod -> View () -> (dt -> View ()) -> Writer [TableColumn dt] ()
tcol f hd view = tell [TableColumn f hd view]

data TableColumn dt = TableColumn Mod (View ()) (dt -> View ())

link :: Url -> Mod -> View () -> View ()
link u f = tag "a" (f . att "href" (fromUrl u))
