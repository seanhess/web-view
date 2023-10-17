{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Users
import Web.Hyperbole hiding (target)
import Web.UI


-- data ViewId
--   = Contact Int
--   | Contacts
--   deriving (Show, Eq, Generic, PageRoute)

data Contact = Contact Int
  deriving (Show, Read, Eq, Param)


data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, Eq, Param)


data Contacts = Contacts
  deriving (Show, Read, Eq, Param)


data ContactsAction
  = Reload
  | Delete Int
  deriving (Show, Read, Eq, Param)


instance LiveView Contact where
  type Action Contact = ContactAction


instance LiveView Contacts where
  type Action Contacts = ContactsAction


-- TODO: put input into LiveView again?

pageLoad :: (Page :> es) => Eff es (View ()) -> Eff es ()
pageLoad pg = do
  vw <- pg
  respondView vw


liveAction :: forall id es. (Page :> es, LiveView id, Param id, Param (Action id), Show id) => (id -> Action id -> Eff es (View' id ())) -> Eff es ()
liveAction handle = do
  -- this continues if it doesn't match!
  mev <- send GetEvent
  case mev of
    Just (Event i a) -> do
      vw <- handle i a
      respondView $ liveView i vw
    _ -> do
      pure ()


page :: forall es. (Page :> es, Users :> es) => Eff es ()
page = do
  liveAction contacts
  liveAction contact

  pageLoad $ do
    us <- usersAll
    pure $ layout us
 where
  layout :: [User] -> View ()
  layout us = do
    col (pad 10 . gap 10) $ do
      -- liveButton' (Contact 2) Reload (bg GrayLight) "Edit #2"
      liveView Contacts $ viewAll us


contacts :: (Page :> es, Users :> es) => Contacts -> ContactsAction -> Eff es (View' Contacts ())
contacts _ Reload = do
  us <- usersAll
  pure $ viewAll us
contacts _ (Delete uid) = do
  userDelete uid
  us <- usersAll
  pure $ viewAll us


-- contacts :: (Reader (LiveView ViewId Contact) :> es, Page :> es) => [User] -> Contacts -> Eff es ()
-- contacts us Reload =
--   liveView $ viewAll us
--

viewAll :: [User] -> View' Contacts ()
viewAll us = do
  row (gap 10) $ do
    liveButton Reload (bg GrayLight) "Reload"
    -- TODO: we need to actually target the target.... hmm....
    target (Contact 2) $ liveButton Edit (bg GrayLight) "Edit 2"
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ do
        liveView (Contact u.id) $ viewContact u


-- you don't need the reader, you're passing it in
contact :: (Page :> es, Users :> es) => Contact -> ContactAction -> Eff es (View' Contact ())
contact (Contact uid) a = do
  u <- userFind uid
  action u a
 where
  action u View = do
    pure $ viewContact u
  action u Edit = do
    pure $ viewEdit u
  action u' Save = do
    u <- userFormData u'.id
    send $ SaveUser u
    pure $ viewContact u


-- TYPE GUARANTEE that we are in the *right* component
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
  liveForm Save (pad 10 . gap 10) $ do
    label id $ do
      text "First Name"
      input (name "firstName" . value u.firstName)

    label id $ do
      text "Last Name"
      input (name "lastName" . value u.lastName)

    label id $ do
      text "Email"
      input (name "email" . value u.email)

    submitButton id "Submit"

    liveButton View id (text "Cancel")

    target Contacts $ liveButton (Delete u.id) (bg Red) (text "Delete")


userFormData :: (Page :> es) => Int -> Eff es User
userFormData uid = do
  f <- formData
  firstName <- param "firstName" f
  lastName <- param "lastName" f
  email <- param "email" f
  pure $ User uid firstName lastName email True


userFind :: (Page :> es, Users :> es) => Int -> Eff es User
userFind uid = do
  mu <- send (LoadUser uid)
  maybe notFound pure mu


usersAll :: (Users :> es) => Eff es [User]
usersAll = send LoadUsers


userSave :: (Users :> es) => User -> Eff es ()
userSave = send . SaveUser


userDelete :: (Users :> es) => Int -> Eff es ()
userDelete = send . DeleteUser


liveForm :: (LiveView id, Param (Action id), Param id) => Action id -> Mod -> View' id () -> View' id ()
liveForm a f cd = do
  c <- context
  tag "form" (dataAction a . dataTarget c . f . flexCol) cd


submitButton :: Mod -> View' c () -> View' c ()
submitButton f = tag "button" (att "type" "submit" . f)


liveButton :: (LiveView id, Param (Action id), Param id) => Action id -> Mod -> View' id () -> View' id ()
liveButton a f cd = do
  c <- context
  tag "button" (dataAction a . dataTarget c . f) cd


-- I'd rather not make ANOTHER copy of liveButton

dataAction :: (Param a) => a -> Mod
dataAction = att "data-action" . toParam


dataTarget :: (Param a) => a -> Mod
dataTarget = att "data-target" . toParam


target :: (LiveView id) => id -> View' id () -> View' a ()
target vid vw =
  addContext vid vw


liveView :: (LiveView id, Param id) => id -> View' id () -> View' ctx ()
liveView vid vw = do
  el (att "id" (toParam vid))
    $ addContext vid vw
