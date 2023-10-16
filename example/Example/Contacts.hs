{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions (cs)
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.Hyperbole.Page
import Web.UI


data ViewId
  = Contact Int
  | Contacts
  deriving (Show, Eq, Generic, PageRoute)


newtype Component viewId event = Component viewId


page :: forall es. (Page :> es, Users :> es) => Maybe ViewId -> Eff es ()
page = livePage root actions
 where
  root = do
    traceM "ROOT"
    us <- usersAll
    respondView $ viewComponent Contacts viewAll us

  actions :: ViewId -> Eff es ()
  actions (Contact uid) = do
    traceM "CONTACT"
    u <- userFind uid
    runAction (Contact uid) contact u
  actions Contacts = do
    traceM "CONTACTS"
    us <- usersAll
    runAction Contacts contacts us


data Contacts
  = Reload
  deriving (Show, Eq, Read)


contacts :: (Reader (Component ViewId Contact) :> es, Page :> es) => [User] -> Contacts -> Eff es ()
contacts us Reload =
  liveView $ viewAll us


viewAll :: [User] -> View' (Component ViewId Contacts) ()
viewAll us = do
  col (pad 10 . gap 10) $ do
    liveButton Reload (bg GrayLight) "Reload"
    liveButton Edit (bg GrayLight) "Edit #2"
    row (pad 10 . gap 10) $ do
      forM_ us $ \u -> do
        el (border 1) $ do
          viewComponent (Contact u.id) viewContact u


-- fn :: (a -> Action)
data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq)


contact :: (Reader (Component ViewId Contact) :> es, Page :> es, Users :> es) => User -> Contact -> Eff es ()
contact u View = do
  liveView $ viewContact u
contact u Edit = do
  liveView $ viewEdit u
contact u' Save = do
  u <- userFormData u'.id
  send $ SaveUser u
  liveView $ viewContact u


viewContact :: User -> View' (Component ViewId Contact) ()
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


viewEdit :: User -> View' (Component ViewId Contact) ()
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


userFind :: (Page :> es, Users :> es) => Int -> Eff es User
userFind uid = do
  mu <- send (LoadUser uid)
  maybe missingInfo pure mu


usersAll :: (Users :> es) => Eff es [User]
usersAll = send LoadUsers


userSave :: (Users :> es) => User -> Eff es ()
userSave = send . SaveUser


liveButton :: (Show action) => action -> Mod -> View' (Component viewId action) () -> View' (Component viewId action) ()
liveButton a f cd = do
  -- you don't need to put the context in here. We can look it up from the above!
  -- pact <- context
  tag "button" (att "data-action" (cs . show $ a) . f) cd


actionTarget :: (PageRoute viewId) => viewId -> Mod
actionTarget viewId = do
  att "data-target" (fromUrl $ routeUrl viewId)


viewComponent :: (PageRoute viewId) => viewId -> (inp -> View' (Component viewId action) ()) -> inp -> View' ctx ()
viewComponent viewId fvw inp = do
  let tgt = fromUrl $ routeUrl viewId
  el (actionTarget tgt . att "id" tgt)
    $ addContext (Component viewId)
    $ fvw inp


runAction :: (PageRoute viewId, Read act, Page :> es) => viewId -> (inp -> act -> Eff (Reader viewId : es) ()) -> inp -> Eff es ()
runAction viewId r inp = do
  -- TODO: fix Just here
  act <- send GetAction
  runReader viewId $ r inp act


liveView :: (Reader (Component viewId action) :> es, Page :> es) => View' (Component viewId action) () -> Eff es ()
liveView vw = do
  u <- ask
  respondView $ addContext u vw


livePage :: (Page :> es) => Eff es () -> (viewId -> Eff es ()) -> Maybe viewId -> Eff es ()
livePage root _ Nothing = root
livePage _ actions (Just vid) = actions vid
