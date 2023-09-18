module Web.UI.Element where

import Control.Monad.State.Strict (modify)
import Control.Monad.Writer.Strict
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Web.UI.Style
import Web.UI.Types

mkElement :: Text -> Mod a -> View a () -> View b ()
mkElement tag f ct = do
  let st = runView ct
  let elm = f $ Element tag [] [] st.contents
  addContent $ Node elm
  addClasses $ classList $ st.classStyles
  addClasses $ mconcat $ elm.classes

addClasses :: [Class] -> View a ()
addClasses clss = do
  modify $ \vs ->
    vs
      { classStyles = foldr addClsDef vs.classStyles clss
      }
 where
  addClsDef :: Class -> Map ClassName (Map Name StyleValue) -> Map ClassName (Map Name StyleValue)
  addClsDef c = M.insert c.className c.classProperties

addContent :: Content -> View a ()
addContent ct = do
  modify $ \vs ->
    vs
      { contents = vs.contents <> [ct]
      }

-- Inserts into first child
insertContents :: [Content] -> View b ()
insertContents cs = do
  modify $ \vs -> vs{contents = insert vs.contents}
 where
  insert [Node e] = [Node $ insertEl e]
  insert cnt = cnt <> cs
  insertEl e = e{children = e.children <> cs}

classList :: Map ClassName (Map Name StyleValue) -> [Class]
classList m = map (uncurry Class) $ M.toList m

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> Mod Attribute
att n v t = t{attributes = M.insert n v t.attributes}

-- | A basic element
el :: Mod a -> View Content () -> View Content ()
el = mkElement "div"

-- | A basic element, with no modifiers
el_ :: View Content () -> View Content ()
el_ = mkElement "div" id

button :: Mod a -> View Content () -> View Content ()
button = mkElement "button"

-- | Convert from text directly to view. You should not have to use this. Use `text` instead
data Head

data Base
data Doc

text :: Text -> View a ()
text t = addContent $ Text t

none :: View a ()
none = pure ()

meta :: Mod a -> View Head ()
meta f = mkElement "meta" f ("" :: View Content ())

title :: Text -> View Head ()
title = mkElement "title" id . text

head :: View Head () -> View Base ()
head = mkElement "head" id

html :: View Base () -> View Doc ()
html = mkElement "html" id

body :: View Content () -> View Base ()
body = mkElement "body" id

row :: Mod a -> View Content () -> View Content ()
row f = el (flexRow . f)

row_ :: View Content () -> View Content ()
row_ = row id

col :: Mod a -> View Content () -> View Content ()
col f = el (flexCol . f)

col_ :: View Content () -> View Content ()
col_ = col id

space :: View Content ()
space = el grow $ pure ()

label :: Mod a -> View Content () -> View Content ()
label = mkElement "label"

form :: Mod a -> View Content () -> View Content ()
form = mkElement "form"

input :: Mod a -> View Content ()
input m = mkElement "input" (m . att "type" "text") none

name :: Text -> Mod Attribute
name = att "name"

value :: Text -> Mod Attribute
value = att "value"

script :: Text -> View b ()
-- script (Code code) = mkElement "script" (att "type" "text/javascript") $ fromText code
script src = mkElement "script" (att "type" "text/javascript" . att "src" src) none

style :: Text -> View b ()
style cnt = mkElement "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")

-- stylesheet :: Text -> View b ()
-- stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none

data Table dt
table :: Mod a -> [dt] -> Writer [Column dt] () -> View (Table dt) ()
table f dts wcs = do
  let cols = execWriter wcs
  mkElement "table" f $ do
    mkElement "thead" id $ do
      mkElement "tr" id $ do
        forM_ cols $ \(Column _ h _) -> do
          h
    mkElement "tbody" id $ do
      forM_ dts $ \dt -> do
        mkElement "tr" id $ do
          forM_ cols $ \(Column md _ view) -> do
            mkElement "td" md $ view dt

tcol :: Mod a -> View Header () -> (dt -> View Content ()) -> Writer [Column dt] ()
tcol f hd view = tell [Column f hd view]

data Column dt = Column (Mod Class) (View Header ()) (dt -> View Content ())

data Header
thead :: Mod a -> View Content () -> View Header ()
thead f = mkElement "th" (f . bold)

thead_ :: View Content () -> View Header ()
thead_ = thead id

data Asdf = Asdf
  { firstName :: Text
  , lastName :: Text
  , email :: Text
  }
usage :: View (Table Asdf) ()
usage =
  table id [Asdf "john" "doe" "john@email.com"] $ do
    tcol id (thead_ "First Name") firstName
    tcol id (thead_ "Last Name") lastName
    tcol id (thead_ "Email") $ \u -> text u.email
 where
  firstName user = text user.firstName
  lastName user = text user.lastName
