module Web.UI.Element where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Web.UI.Style
import Web.UI.Types
import Web.UI.Url


tag :: Text -> Mod -> View c () -> View c ()
tag nm f ct = do
  ctx <- context
  let st = runView ctx ct
  let elm = f $ Element nm [] [] st.contents
  addContent $ Node elm
  addClasses $ M.elems st.css
  addClasses $ mconcat elm.classes


addClasses :: [Class] -> View c ()
addClasses clss = do
  modCss $ \cm -> foldr addClsDef cm clss
 where
  addClsDef :: Class -> Map Selector Class -> Map Selector Class
  addClsDef c = M.insert c.selector c


addContent :: Content -> View c ()
addContent ct =
  modContents (<> [ct])


-- Inserts into first child
insertContents :: [Content] -> View c ()
insertContents cs = modContents insert
 where
  insert [Node e] = [Node $ insertEl e]
  insert cnt = cnt <> cs
  insertEl e = e{children = e.children <> cs}


-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> Mod
att n v (Element en ec ea ecs) = Element en ec (M.insert n v ea) ecs


-- | A basic element
el :: Mod -> View c () -> View c ()
el = tag "div"


-- | A basic element, with no modifiers
el_ :: View c () -> View c ()
el_ = tag "div" id


button :: Mod -> View c () -> View c ()
button = tag "button"


-- | Convert from text directly to view. You should not have to use this. Use `text` instead
data Head


data Base
data Doc


text :: Text -> View c ()
text t = addContent $ Text t


none :: View c ()
none = pure ()


meta :: Mod -> View c ()
meta f = tag "meta" f none


title :: Text -> View c ()
title = tag "title" id . text


head :: View c () -> View c ()
head = tag "head" id


html :: View c () -> View c ()
html = tag "html" id


body :: View c () -> View c ()
body = tag "body" id


row :: Mod -> View c () -> View c ()
row f = el (flexRow . f)


row_ :: View c () -> View c ()
row_ = row id


col :: Mod -> View c () -> View c ()
col f = el (flexCol . f)


col_ :: View c () -> View c ()
col_ = col id


space :: View c ()
space = el grow $ pure ()


label :: Mod -> View c () -> View c ()
label = tag "label"


form :: Mod -> View c () -> View c ()
form f = tag "form" (f . flexCol)


input :: Mod -> View c ()
input m = tag "input" (m . att "type" "text") none


name :: Text -> Mod
name = att "name"


value :: Text -> Mod
value = att "value"


script :: Text -> View c ()
-- script (Code code) = tag "script" (att "type" "text/javascript") $ fromText code
script src = tag "script" (att "type" "text/javascript" . att "src" src) none


style :: Text -> View c ()
style cnt = tag "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")


stylesheet :: Text -> View c ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none


-- table :: Mod -> [dt] -> Writer [TableColumn dt] () -> View c ()
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

link :: Url -> Mod -> View c () -> View c ()
link u f = tag "a" (f . att "href" (fromUrl u))
