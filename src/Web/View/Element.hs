module Web.View.Element where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text)
import Effectful
import Effectful.Writer.Static.Local
import Web.View.Style
import Web.View.Types
import Web.View.View


{- | A basic element

> el (bold . pad 10) "Hello"
-}
el :: Mod -> View c () -> View c ()
el = tag "div"


{- | A basic element, with no modifiers

> el_ "Hello"
-}
el_ :: View c () -> View c ()
el_ = tag "div" id


{- | Add text to a view. Not required for string literals

> el_ $ do
>   "Hello: "
>   text user.name
-}
text :: Text -> View c ()
text t = viewAddContent $ Text t


{- | Embed static, unescaped HTML or SVG. Take care not to use 'raw' with user-generated content.

> spinner = raw "<svg>...</svg>"
-}
raw :: Text -> View c ()
raw t = viewAddContent $ Raw t


{- | Do not any any content

> if isVisible
>  then content
>  else none
-}
none :: View c ()
none = pure ()


pre :: Mod -> Text -> View c ()
pre f t = tag "pre" f (text t)


-- * Inputs


form :: Mod -> View c () -> View c ()
form f = tag "form" (f . flexCol)


input :: Mod -> View c ()
input m = tag "input" (m . att "type" "text") none


name :: Text -> Mod
name = att "name"


value :: Text -> Mod
value = att "value"


label :: Mod -> View c () -> View c ()
label = tag "label"


button :: Mod -> View c () -> View c ()
button = tag "button"


-- * Head and Metadata


script :: Text -> View c ()
script src = tag "script" (att "type" "text/javascript" . att "src" src) none


style :: Text -> View c ()
style cnt = tag "style" (att "type" "text/css") (text $ "\n" <> cnt <> "\n")


stylesheet :: Text -> View c ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none


-- * Tables


table :: Mod -> [dt] -> Eff '[Writer [TableColumn c dt]] () -> View c ()
table f dts wcs = do
  c <- context
  let cols = runPureEff . execWriter $ wcs
  tag "table" borderCollapse $ do
    tag "thead" id $ do
      tag "tr" f $ do
        forM_ cols $ \tc -> do
          addContext (Head c) tc.headCell
    tag "tbody" id $ do
      forM_ dts $ \dt -> do
        tag "tr" f $ do
          forM_ cols $ \tc -> do
            addContext dt $ tc.dataCell dt
 where
  borderCollapse :: Mod
  borderCollapse = addClass $ cls "brd-cl" & prop @Text "border-collapse" "collapse"


tcol :: forall dt c es. (Writer [TableColumn c dt] :> es) => View (Head c) () -> (dt -> View dt ()) -> Eff es ()
tcol hd view = do
  tell ([TableColumn hd view] :: [TableColumn c dt])


newtype Head a = Head a


data TableColumn c dt = TableColumn
  { headCell :: View (Head c) ()
  , dataCell :: dt -> View dt ()
  }


th :: Mod -> View c () -> View (Head c) ()
th f cnt = do
  Head c <- context
  addContext c $ tag "th" f cnt


td :: Mod -> View () () -> View dt ()
td f c = addContext () $ tag "td" f c
