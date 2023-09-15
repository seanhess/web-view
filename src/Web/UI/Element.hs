module Web.UI.Element where

import Control.Monad.State.Strict (modify)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Web.UI.Style
import Web.UI.Types

mkElement :: Text -> Mod a -> View a () -> View b ()
mkElement nm f ct = do
  let t = f $ Element nm [] [] (viewContents ct)
  addContent $ Node t
  addClasses $ classList $ viewClasses ct
  addClasses $ mconcat $ t.classes

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
text :: Text -> View a ()
text t = addContent $ Text t

none :: View Content ()
none = pure ()

meta :: Mod a -> View a ()
meta f = mkElement "meta" f ("" :: View Content ())

title :: Text -> View Script ()
title = mkElement "title" id . text

head :: View Script () -> View a ()
head = mkElement "head" id

html :: View a () -> View a ()
html = mkElement "html" id

body :: View Content () -> View Content ()
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
script src = mkElement "script" (att "type" "text/javascript" . att "src" src) none

style :: Text -> View b ()
style cnt = mkElement "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")

-- script (Code code) = mkElement "script" (att "type" "text/javascript") $ fromText code

-- stylesheet :: Text -> View b ()
-- stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none
