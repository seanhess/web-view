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

data Action
  = Contact Int Contact
  | Contacts Contacts
  deriving (Show, Eq, Generic, PageRoute)

page :: forall es. (Wai :> es, Users :> es) => Maybe Action -> Eff es ()
page = livePage root actions
 where
  root = do
    us <- usersAll
    view $ viewComponent Contacts viewAll us

  -- TODO! Unify the types!
  actions :: Action -> Eff es ()
  actions (Contact uid act) = do
    u <- userFind uid
    runAction (Contact uid) contact u act
  actions (Contacts act) = do
    us <- usersAll
    runAction Contacts contacts us act

data Contacts
  = Reload
  deriving (Show, Eq, Read, Generic, PageAction, PageRoute)

instance LiveView Contacts Action where
  type Input Contacts = [User]

contacts :: (Reader (Contacts -> Action) :> es, Wai :> es) => [User] -> Contacts -> Eff es ()
contacts us Reload =
  liveView $ viewAll us

viewAll :: [User] -> View' (Contacts -> Action) ()
viewAll us = do
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ do
        viewComponent (Contact u.id) viewContact u
        liveButton Reload id "Reload"

-- fn :: (a -> Action)
data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq, Generic, PageAction, PageRoute)

instance LiveView Contact Action where
  type Input Contact = User

-- compId = Contact

contact :: (Reader (Contact -> Action) :> es, Wai :> es, Users :> es) => User -> Contact -> Eff es ()
contact u View = do
  liveView $ viewContact u
contact u Edit = do
  liveView $ viewEdit u
contact u' Save = do
  u <- userFormData u'.id
  send $ SaveUser u
  liveView $ viewContact u

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

-- add support for context?
viewContact :: User -> View' (Contact -> Action) ()
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

liveButton :: (PageRoute pageAction) => action -> Mod -> View' (action -> pageAction) () -> View' (action -> pageAction) ()
liveButton a f cd = do
  pact <- context
  tag "button" (att "data-action" (fromUrl . routeUrl . pact $ a) . f) cd

viewEdit :: User -> View' (Contact -> Action) ()
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

class LiveView act pageAction where
  type Input act

viewComponent :: forall act pageAction ctx. (LiveView act pageAction, PageRoute pageAction) => (act -> pageAction) -> (Input act -> View' (act -> pageAction) ()) -> Input act -> View' ctx ()
viewComponent toPageAction fvw inp = do
  el (hxSwap InnerHTML . hxTarget This)
    $ addContext toPageAction
    $ fvw inp

runAction :: forall act es pageAction. (LiveView act pageAction, PageRoute pageAction) => (act -> pageAction) -> (Input act -> act -> Eff (Reader (act -> pageAction) : es) ()) -> Input act -> act -> Eff es ()
runAction toPageAction r inp act =
  runReader toPageAction $ r inp act

liveView :: (Reader (act -> pageAction) :> es, Wai :> es) => View' (act -> pageAction) () -> Eff es ()
liveView vw = do
  u <- ask
  view $ addContext u vw

livePage :: Eff es () -> (action -> Eff es ()) -> Maybe action -> Eff es ()
livePage = undefined
