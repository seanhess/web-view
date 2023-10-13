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

data Target
  = Contact Int
  | Contacts
  deriving (Show, Eq, Generic, PageRoute)

page :: forall es. (Wai :> es, Users :> es) => Maybe Target -> Eff es ()
page = livePage root actions
 where
  root = do
    us <- usersAll
    view $ viewComponent @Contacts viewAll us

  -- TODO! Unify the types!
  actions :: Target -> Contact -> Eff es ()
  actions (Contact uid) act = do
    u <- userFind uid
    runAction contact u act
  actions Contacts act = do
    us <- usersAll
    runAction contacts us act

contacts :: [User] -> Contacts -> Eff es ()
contacts us Reload =
  liveView $ viewAll us

data Contacts
  = Reload
  deriving (Show, Eq, Read, Generic, PageAction)

instance LiveView Contacts where
  type Input Contacts = [User]
  compId _ = "contacts"

data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq, Generic, PageAction)

instance LiveView Contact where
  type Input Contact = User
  compId u = routeUrl $ Contact u.id

-- -- carry the type info here!
-- contactView :: LiveView User Contact
-- contactView = viewContact

contact :: (Reader Url :> es, Wai :> es, Users :> es) => User -> Contact -> Eff es ()
contact u View = do
  liveView $ viewContact u
contact u Edit = do
  liveView $ viewEdit u
contact u' Save = do
  u <- userFormData u'.id
  send $ SaveUser u
  liveView $ viewContact u

liveView :: (Reader Url :> es, Wai :> es) => View' Url () -> Eff es ()
liveView vw = do
  u <- ask
  view $ addContext u vw

livePage :: (PageAction act) => Eff es () -> (comp -> act -> Eff es ()) -> Maybe comp -> Eff es ()
livePage = undefined

userFind :: (Wai :> es, Users :> es) => Int -> Eff es User
userFind uid = do
  mu <- send (LoadUser uid)
  -- not found! Oh no!
  maybe notFound pure mu

usersAll :: (Users :> es) => Eff es [User]
usersAll = send LoadUsers

userSave :: (Users :> es) => User -> Eff es ()
userSave = send . SaveUser

-- hxRequest :: Mod -> Mod
-- hxRequest = prefix "hx-request"

viewAll :: [User] -> View' Url ()
viewAll us = do
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ do
        viewComponent @Contact viewContact u
        liveButton Reload id "Reload"

-- add support for context?
viewContact :: User -> View' Url ()
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

liveButton :: (PageAction action) => action -> Mod -> View' Url () -> View' Url ()
liveButton a f cd = do
  u <- context
  tag "button" (att "data-action" (fromAction a) . att "data-target" (fromUrl u) . f) cd

viewEdit :: User -> View' Url ()
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

class LiveView act where
  type Input act
  compId :: Input act -> Url

viewComponent :: forall act. (LiveView act) => (Input act -> View' Url ()) -> Input act -> View ()
viewComponent fvw inp = do
  let tg = compId @act inp
  el (hxSwap InnerHTML . hxTarget This)
    $ addContext tg
    $ fvw inp

runAction :: forall act es. (LiveView act) => (Input act -> act -> Eff (Reader Url : es) ()) -> Input act -> act -> Eff es ()
runAction r inp act =
  let u = compId @act inp
   in runReader u $ r inp act
