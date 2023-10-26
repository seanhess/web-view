module Web.UI.Element where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Writer.Static.Local
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
data Head = Head


data Base
data Doc


text :: Text -> View c ()
text t = addContent $ Text t


raw :: Text -> View c ()
raw t = addContent $ Raw t


none :: View c ()
none = pure ()


meta :: Mod -> View c ()
meta f = tag "meta" f none


title :: Text -> View c ()
title = tag "title" id . text


-- head :: View c () -> View c ()
-- head = tag "head" id

-- html :: View c () -> View c ()
-- html = tag "html" id

-- body :: View c () -> View c ()
-- body = tag "body" id

layout :: Mod -> View c () -> View c ()
layout f = el (rootLayout . f)


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


table :: Mod -> [dt] -> Eff '[Writer [TableColumn dt]] () -> View c ()
table f dts wcs = do
  let cols = runPureEff . execWriter $ wcs
  tag "table" borderCollapse $ do
    tag "thead" id $ do
      tag "tr" f $ do
        forM_ cols $ \tc -> do
          addContext Head tc.headCell
    tag "tbody" id $ do
      forM_ dts $ \dt -> do
        tag "tr" f $ do
          forM_ cols $ \tc -> do
            addContext dt $ tc.dataCell dt
 where
  borderCollapse :: Mod
  borderCollapse = addClass $ cls "brd-cl" & prop @Text "border-collapse" "collapse"


tcol :: forall dt es. (Writer [TableColumn dt] :> es) => View Head () -> (dt -> View dt ()) -> Eff es ()
tcol hd view = do
  tell ([TableColumn hd view] :: [TableColumn dt])


data TableColumn dt = TableColumn
  { headCell :: View Head ()
  , dataCell :: dt -> View dt ()
  }


-- newtype Cell t a = Cell {fromCell :: View () ()}

th :: Mod -> View () () -> View Head ()
th f c = addContext () $ tag "th" f c


td :: Mod -> View () () -> View dt ()
td f c = addContext () $ tag "td" f c


link :: Url -> Mod -> View c () -> View c ()
link u f = tag "a" (f . att "href" (fromUrl u))
