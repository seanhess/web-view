module Web.View.View where

import Data.Map qualified as M
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local as ES
import Web.View.Types


-- * Views


{- | Views are HTML fragments that carry all atomic 'CSS' used by any child element.

> view :: View c ()
> view = col (pad 10 . gap 10) $ do
>   el (color Red) "Hello"
>   el_ "World"

They can also have a context which can be used to create type-safe or context-aware elements that accept specific child views. See 'Web.View.Element.table'
-}
newtype View context a = View {viewState :: Eff [Reader context, State ViewState] a}
  deriving newtype (Functor, Applicative, Monad)


instance IsString (View context ()) where
  fromString s = viewModContents (const [Text (pack s)])


data ViewState = ViewState
  { contents :: [Content]
  , css :: CSS
  }


instance Semigroup ViewState where
  va <> vb = ViewState (va.contents <> vb.contents) (va.css <> vb.css)


-- | Extract the 'ViewState' from a 'View'
runView :: context -> View context () -> ViewState
runView ctx (View ef) =
  runPureEff . execState (ViewState [] []) . runReader ctx $ ef


-- | Get the current context
context :: View context context
context = View ask


-- | Run a view with a specific `context` in a parent 'View' with a different context. This can be used to create type safe view functions, like 'Web.View.Element.table'
addContext :: context -> View context () -> View c ()
addContext ctx vw = do
  -- runs the sub-view in a different context, saving its state
  -- we need to MERGE it
  let st = runView ctx vw
  View $ ES.modify $ \s -> s <> st


viewModContents :: ([Content] -> [Content]) -> View context ()
viewModContents f = View $ do
  ES.modify $ \s -> s{contents = f s.contents}


viewModCss :: (CSS -> CSS) -> View context ()
viewModCss f = View $ do
  ES.modify $ \s -> s{css = f s.css}


viewAddClasses :: [Class] -> View c ()
viewAddClasses clss = do
  viewModCss $ \cm -> foldr addClsDef cm clss
 where
  addClsDef :: Class -> CSS -> CSS
  addClsDef c = M.insert c.selector c


viewAddContent :: Content -> View c ()
viewAddContent ct =
  viewModContents (<> [ct])


-- | Inserts contents into the first child element
viewInsertContents :: [Content] -> View c ()
viewInsertContents cs = viewModContents insert
 where
  insert [Node e] = [Node $ insertEl e]
  insert cnt = cnt <> cs
  insertEl e = e{children = e.children <> cs}


-- * Creating new Elements


{- | Create a new element constructor

> aside :: Mod -> View c () -> View c ()
> aside = tag "aside"
-}
tag :: Text -> Mod -> View c () -> View c ()
tag nm f ct = do
  -- Applies the modifier and merges children into parent
  ctx <- context
  let st = runView ctx ct
  let elm = f $ Element nm [] [] st.contents
  viewAddContent $ Node elm
  viewAddClasses $ M.elems st.css
  viewAddClasses $ mconcat elm.classes


{- | Set an attribute, replacing existing value

> hlink :: Text -> View c () -> View c ()
> hlink url content = tag "a" (att "href" url) content
-}
att :: Name -> AttValue -> Mod
att n v (Element en ec ea ecs) = Element en ec (M.insert n v ea) ecs
