module Web.UI.Element where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Effectful
import Effectful.State.Dynamic
import Web.UI.Style
import Web.UI.Types

-- import Web.UI.Url

type Children es = Eff (View : es) ()

-- we want to get the content
-- but we aren't IN that monad. It's independent of us
-- there must be a better way to do this.
tag :: (View :> es) => Text -> Mod -> Children es -> Eff es ()
tag nm f ct = do
  st <- runView ct
  let elm = f $ Element nm [] [] st.contents
  addContent $ Node elm
  addClasses $ classList $ st.classStyles
  addClasses $ mconcat $ elm.classes

addClasses :: (View :> es) => [Class] -> Eff es ()
addClasses clss = do
  modify $ \vs ->
    vs
      { classStyles = foldr addClsDef vs.classStyles clss
      }
 where
  addClsDef :: Class -> Map ClassName (Map Name StyleValue) -> Map ClassName (Map Name StyleValue)
  addClsDef c = M.insert c.className c.classProperties

addContent :: (View :> es) => Content -> Eff es ()
addContent ct = do
  modify $ \vs ->
    vs
      { contents = vs.contents <> [ct]
      }

-- Inserts into first child
insertContents :: (View :> es) => [Content] -> Eff es ()
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
el :: (View :> es) => Mod -> Children es -> Eff es ()
el = tag "div"

-- | A basic element, with no modifiers
el_ :: (View :> es) => Eff (View : es) () -> Eff es ()
el_ = tag "div" id

button :: (View :> es) => Mod -> Children es -> Eff es ()
button = tag "button"

-- | Convert from text directly to view. You should not have to use this. Use `text` instead
data Head

data Base
data Doc

text :: (View :> es) => Text -> Eff es ()
text t = addContent $ Text t

none :: Eff es ()
none = pure ()

meta :: (View :> es) => Mod -> Eff es ()
meta f = tag "meta" f none

title :: (View :> es) => Text -> Eff es ()
title = tag "title" id . text

head :: (View :> es) => Children es -> Eff es ()
head = tag "head" id

html :: (View :> es) => Children es -> Eff es ()
html = tag "html" id

body :: (View :> es) => Children es -> Eff es ()
body = tag "body" id

row :: (View :> es) => Mod -> Children es -> Eff es ()
row f = el (flexRow . f)

row_ :: (View :> es) => Children es -> Eff es ()
row_ = row id

col :: (View :> es) => Mod -> Children es -> Eff es ()
col f = el (flexCol . f)

col_ :: (View :> es) => Children es -> Eff es ()
col_ = col id

space :: (View :> es) => Eff es ()
space = el grow $ pure ()

label :: (View :> es) => Mod -> Children es -> Eff es ()
label = tag "label"

form :: (View :> es) => Mod -> Children es -> Eff es ()
form f = tag "form" (f . flexCol)

input :: (View :> es) => Mod -> Eff es ()
input m = tag "input" (m . att "type" "text") none

name :: Text -> Mod
name = att "name"

value :: Text -> Mod
value = att "value"

script :: (View :> es) => Text -> Eff es ()
-- script (Code code) = tag "script" (att "type" "text/javascript") $ fromText code
script src = tag "script" (att "type" "text/javascript" . att "src" src) none

style :: (View :> es) => Text -> Eff es ()
style cnt = tag "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")

stylesheet :: (View :> es) => Text -> Eff es ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none

-- table :: Mod -> [dt] -> Writer [TableColumn dt] () -> Eff es ()
-- table f dts wcs = do
--   let cols = execWriter wcs
--   tag "table" (f . borderCollapse) $ do
--     tag "thead" id $ do
--       tag "tr" id $ do
--         forM_ cols $ \tc -> do
--           tc.headCell.fromCell
--     tag "tbody" id $ do
--       forM_ dts $ \dt -> do
--         tag "tr" id $ do
--           forM_ cols $ \tc -> do
--             (tc.dataCell dt).fromCell
--  where
--   borderCollapse :: Mod
--   borderCollapse = cls1 $ Class "brd-cl" [("border-collapse", "collapse")]
--
-- tcol :: Cell Head () -> (dt -> Cell Data ()) -> Writer [TableColumn dt] ()
-- tcol hd view = tell [TableColumn hd view]
--
-- data TableColumn dt = TableColumn
--   { headCell :: Cell Head ()
--   , dataCell :: dt -> Cell Data ()
--   }
--
-- data Data
-- newtype Cell t a = Cell {fromCell :: Eff es a}
--
-- th :: Mod -> Eff es () -> Cell Head ()
-- th f c = Cell $ tag "th" f c
--
-- td :: Mod -> Eff es () -> Cell Data ()
-- td f c = Cell $ tag "td" f c
--
-- link :: Url -> Mod -> Eff es () -> Eff es ()
-- link u f = tag "a" (f . att "href" (fromUrl u))
