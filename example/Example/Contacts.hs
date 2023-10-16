{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions (cs)
import Data.Text
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Text.Read
import Web.Hyperbole
import Web.Hyperbole.Page
import Web.UI


-- data ViewId
--   = Contact Int
--   | Contacts
--   deriving (Show, Eq, Generic, PageRoute)

data Contact = Contact Int
  deriving (Show, Read, Eq, Generic, PageRoute)


data Event id action = Event id action


data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, Eq)


data Contacts = Contacts
  deriving (Show, Read, Eq, Generic, PageRoute)


data ContactsAction
  = Reload
  deriving (Show, Read, Eq)


class Component id action where
  parseEvent :: Text -> Text -> Maybe (Event id action)
  default parseEvent :: (Read id, Read action) => Text -> Text -> Maybe (Event id action)
  parseEvent ti ta = do
    i <- readMaybe (cs ti)
    a <- readMaybe (cs ta)
    pure $ Event i a


instance Component Contact ContactAction
instance Component Contacts ContactsAction


-- TODO: put input into Component again?

root :: (Page :> es) => Eff es (View ()) -> Eff es ()
root pg = do
  vw <- pg
  respondView vw


handleAction :: forall viewId action es. (Page :> es, Component viewId action, PageRoute viewId) => (viewId -> action -> Eff es (View' viewId ())) -> Eff es ()
handleAction handle = do
  -- TODO: these shouldn't be required. Just pass if they aren't found
  ti <- param "id"
  ta <- param "action"
  case parseEvent ti ta of
    Nothing -> pure ()
    Just (Event vid act) -> do
      vw <- handle vid act
      respondView $ liveView vid vw


-- newtype Component viewId event = Component viewId

page :: forall es. (Page :> es, Users :> es) => Eff es ()
page = do
  -- I wanted them to be LOCAL. Easy to make sure you're routing it correctly
  -- handleAction @Contacts !!
  handleAction $ \Contacts Reload -> do
    us <- usersAll
    pure $ viewAll us

  handleAction $ \(Contact uid) act -> do
    u <- userFind uid
    contact u (Contact uid) act

  root $ do
    us <- usersAll
    pure $ layout us


-- contacts :: (Reader (Component ViewId Contact) :> es, Page :> es) => [User] -> Contacts -> Eff es ()
-- contacts us Reload =
--   liveView $ viewAll us
--
layout :: [User] -> View ()
layout us = do
  col (pad 10 . gap 10) $ do
    -- liveButton' (Contact 2) Reload (bg GrayLight) "Edit #2"

    row (pad 10 . gap 10) $ do
      liveView Contacts $ viewAll us


viewAll :: [User] -> View' Contacts ()
viewAll us = do
  liveButton Reload (bg GrayLight) "Reload"
  forM_ us $ \u -> do
    el (border 1) $ do
      liveView (Contact u.id) $ viewContact u


-- you don't need the reader, you're passing it in
contact :: (Page :> es, Users :> es) => User -> Contact -> ContactAction -> Eff es (View' Contact ())
contact u _ View = do
  pure $ viewContact u
contact u _ Edit = do
  pure $ viewEdit u
contact u' _ Save = do
  u <- userFormData u'.id
  send $ SaveUser u
  pure $ viewContact u


-- TYPE GUARANTEE that we are in the right component
viewContact :: User -> View' Contact ()
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


viewEdit :: User -> View' Contact ()
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


liveButton :: (Show action, Component id action) => action -> Mod -> View' id () -> View' id ()
liveButton a f cd = do
  -- you don't need to put the context in here. We can look it up from the above!
  -- pact <- context
  tag "button" (att "data-action" (cs . show $ a) . f) cd


-- liveButton' :: (Show action) => viewId -> action2 -> Mod -> View' (Component viewId action2) () -> View' (Component viewId action) ()
-- liveButton' viewId a f cd = undefined

-- -- you don't need to put the context in here. We can look it up from the above!
-- -- pact <- context
-- tag
--   "button"
--   ( f
--       . att "data-action" (cs . show $ a)
--       . actionTarget viewId
--   )
--   -- I would have to manually change the context to get this to work
--   -- arg...
--   cd

actionTarget :: (PageRoute viewId) => viewId -> Mod
actionTarget viewId = do
  att "data-target" (fromUrl $ routeUrl viewId)


liveView :: (PageRoute viewId) => viewId -> View' viewId () -> View' ctx ()
liveView viewId vw = do
  let tgt = fromUrl $ routeUrl viewId
  el (actionTarget tgt . att "id" tgt)
    $ addContext viewId
    $ vw


runAction :: (PageRoute viewId, Read act, Page :> es) => viewId -> (inp -> act -> Eff (Reader viewId : es) ()) -> inp -> Eff es ()
runAction viewId r inp = do
  -- TODO: fix Just here
  act <- send GetAction
  runReader viewId $ r inp act

-- liveView :: View' viewId () -> Eff es ()
-- liveView vw = do
--   u <- ask
--   respondView $ addContext u vw

-- livePage :: (Page :> es) => Eff es () -> (viewId -> Eff es ()) -> Maybe viewId -> Eff es ()
-- livePage rt _ Nothing = rt
-- livePage _ actions (Just vid) = actions vid
