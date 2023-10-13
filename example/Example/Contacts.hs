{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Contacts where

import Control.Monad (forM_)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.Hyperbole.Page
import Web.UI

data Action
  = Contact Int Contact
  | Contacts Contacts
  deriving (Show, Eq, Generic, PageRoute)

page :: forall es. (Page :> es, Users :> es) => Maybe Action -> Eff es ()
page = livePage root actions
 where
  root = do
    us <- usersAll
    respondView $ viewComponent Contacts viewAll us

  actions :: Action -> Eff es ()
  actions (Contact uid act) = do
    u <- userFind uid
    runAction (Contact uid) contact u act
  actions (Contacts act) = do
    us <- usersAll
    runAction Contacts contacts us act

livePage :: (Page :> es) => Eff es () -> (Action -> Eff es ()) -> Maybe Action -> Eff es ()
livePage root _ Nothing = root
livePage _ actions (Just act) = do
  -- TODO: only accept POST
  actions act

data Contacts
  = Reload
  deriving (Show, Eq, Read, Generic, PageRoute)

contacts :: (Reader (Contacts -> Action) :> es, Page :> es) => [User] -> Contacts -> Eff es ()
contacts us Reload =
  liveView $ viewAll us

viewAll :: [User] -> View' (Contacts -> Action) ()
viewAll us = do
  row (pad 10 . gap 10) $ do
    liveButton Reload (bg GrayLight) "Reload"
    forM_ us $ \u -> do
      el (border 1) $ do
        viewComponent (Contact u.id) viewContact u

-- fn :: (a -> Action)
data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq, Generic, PageRoute)

-- compId = Contact

contact :: (Reader (Contact -> Action) :> es, Page :> es, Users :> es) => User -> Contact -> Eff es ()
contact u View = do
  liveView $ viewContact u
contact u Edit = do
  liveView $ viewEdit u
contact u' Save = do
  u <- userFormData u'.id
  send $ SaveUser u
  liveView $ viewContact u

userFind :: (Page :> es, Users :> es) => Int -> Eff es User
userFind uid = do
  mu <- send (LoadUser uid)
  maybe missingInfo pure mu

usersAll :: (Users :> es) => Eff es [User]
usersAll = send LoadUsers

userSave :: (Users :> es) => User -> Eff es ()
userSave = send . SaveUser

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

    liveButton View id (text "Cancel")

userFormData :: (Page :> es) => Int -> Eff es User
userFormData uid = do
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  pure $ User uid firstName lastName email True

viewComponent :: (PageRoute pageAction) => (act -> pageAction) -> (inp -> View' (act -> pageAction) ()) -> inp -> View' ctx ()
viewComponent toPageAction fvw inp = do
  el (hxSwap InnerHTML . hxTarget This)
    $ addContext toPageAction
    $ fvw inp

runAction :: (PageRoute pageAction) => (act -> pageAction) -> (inp -> act -> Eff (Reader (act -> pageAction) : es) ()) -> inp -> act -> Eff es ()
runAction toPageAction r inp act =
  runReader toPageAction $ r inp act

liveView :: (Reader (act -> pageAction) :> es, Page :> es) => View' (act -> pageAction) () -> Eff es ()
liveView vw = do
  u <- ask
  respondView $ addContext u vw
