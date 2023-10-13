{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.Kind
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.UI

-- but there's no action associated with it. Maybe a generic "Load"?
-- there's also no hierarchy
data Target
  = Contact Int
  deriving (Show, Eq, Generic, PageRoute)

page :: (Wai :> es, Users :> es) => Maybe Target -> Eff es ()
page = livePage root actions
 where
  root = do
    us <- send LoadUsers
    view $ viewAll us

  actions (Contact uid) act = do
    u <- loadUser uid
    runAction @Contact u act

data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq, Generic, PageAction, PageRoute)

instance Component Contact where
  type Input Contact = User
  type Effects Contact es = (Wai :> es, Users :> es)

  compId u = routeUrl $ Contact u.id

  compView = viewContact

  compAction u View = do
    let tg = compId @Contact u
    view $ runReader tg $ viewContact u
  compAction u Edit = do
    view $ viewEdit u
  compAction u' Save = do
    u <- userFormData u'.id
    send $ SaveUser u
    view $ viewContact u

livePage :: (PageAction act) => Eff es () -> (comp -> act -> Eff es ()) -> Maybe comp -> Eff es ()
livePage = undefined

loadUser :: (Wai :> es, Users :> es) => Int -> Eff es User
loadUser uid = do
  mu <- send (LoadUser uid)
  -- not found! Oh no!
  maybe notFound pure mu

-- hxRequest :: Mod -> Mod
-- hxRequest = prefix "hx-request"

-- I can allow arbitrary effects...
-- for myself
-- just not for everyone :)

viewAll :: (View :> es) => [User] -> Eff es ()
viewAll us = do
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ do
        viewComponent @Contact u

-- add support for context?
viewContact :: (View :> es, Reader Url :> es) => User -> Eff es ()
viewContact u = do
  col (pad 10 . gap 10) $ do
    el_ $ do
      label id (text "First Name:")
      text u.firstName

    el_ $ do
      label id (text "Last Name:")
      text u.lastName

    el_ $ do
      label id (text "Email")
      text u.email

    -- oh, boo, the attribute mods can't be execute in the monad :(
    -- at least not right now :)
    -- form elements should accept them directly
    liveButton Edit (bg Green . hover |: bg GreenLight) (text "Click to Edit")

liveButton :: (View :> es, Reader Url :> es, PageAction action) => action -> Mod -> Children es -> Eff es ()
liveButton a f cd = do
  t <- ask
  tag "button" (att "data-action" (fromAction a) . att "data-target" (fromUrl t) . f) cd

viewEdit :: (View :> es) => User -> Eff es ()
viewEdit u = do
  form (action Save . pad 10 . gap 10) $ do
    label id $ do
      text "First Name"
      input (name "firstName" . value u.firstName)

    label id $ do
      text "Last Name"
      input (name "lastName" . value u.lastName)

    label id $ do
      text "Email"
      input (name "email" . value u.email)

    button id (text "Submit")

    button (action View) (text "Cancel")

userFormData :: (Wai :> es) => Int -> Eff es User
userFormData uid = do
  firstName <- formParam "firstName"
  lastName <- formParam "lastName"
  email <- formParam "email"
  pure $ User uid firstName lastName email True

class PageAction a where
  toAction :: Text -> Maybe a
  fromAction :: a -> Text

class Component act where
  type Input act
  type Effects act (es :: [Effect]) :: Constraint
  compId :: Input act -> Url
  compView :: (View :> es, Reader Url :> es) => Input act -> Eff es ()
  compAction :: (Effects act es) => Input act -> act -> Eff es ()

viewComponent :: forall act es. (Component act, View :> es) => Input act -> Eff es ()
viewComponent inp = do
  let tg = compId @act inp
  el (hxSwap InnerHTML . hxTarget This)
    $ runReader tg (compView @act inp)

runAction :: forall act es. (Component act, Effects act (Reader Url : es)) => Input act -> act -> Eff es ()
runAction inp act =
  let tg = compId @act inp
   in runReader tg $ compAction @act inp act
