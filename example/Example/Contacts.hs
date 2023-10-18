{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.Contacts where

import Control.Monad (forM_)
import Data.String.Conversions
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Debug
import Example.Effects.Users
import Web.Hyperbole
import Web.UI


data Contact = Contact Int
  deriving (Show, Read, Param)


data ContactAction
  = Edit
  | Save
  | View
  deriving (Show, Read, Param)


data Contacts = Contacts
  deriving (Show, Read, Param)


data ContactsAction
  = Reload
  | Delete Int
  deriving (Show, Read, Param)


instance LiveView Contact where
  type Action Contact = ContactAction


instance LiveView Contacts where
  type Action Contacts = ContactsAction


page :: forall es. (Page :> es, Users :> es, Debug :> es) => Eff es ()
page = do
  pageAction contacts
  pageAction contact
  pageLoad $ do
    us <- usersAll
    pure $ do
      col (pad 10 . gap 10) $ do
        liveView Contacts $ viewAll us


contacts :: (Page :> es, Users :> es, Debug :> es) => Contacts -> ContactsAction -> Eff es (View Contacts ())
contacts _ Reload = do
  send $ Delay 1000
  us <- usersAll
  pure $ viewAll us
contacts _ (Delete uid) = do
  userDelete uid
  us <- usersAll
  pure $ viewAll us


viewAll :: [User] -> View Contacts ()
viewAll us = do
  row (gap 10) $ do
    -- I want to change something, then run ANOTHER event on load
    liveButton Reload (bg GrayLight) "Reload"
    target (Contact 2) $ liveButton Edit (bg GrayLight) "Edit 2"
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ do
        liveView (Contact u.id) $ viewContact u


contact :: (Page :> es, Users :> es, Debug :> es) => Contact -> ContactAction -> Eff es (View Contact ())
contact (Contact uid) a = do
  u <- userFind uid
  action u a
 where
  action u View = do
    pure $ viewContact u
  action u Edit = do
    pure $ viewEdit u
  action u' Save = do
    send $ Delay 1000
    u <- userFormData u'.id
    send $ SaveUser u
    pure $ viewContact u


viewContact :: User -> View Contact ()
viewContact u = do
  col (pad 10 . gap 10) $ do
    el_ $ do
      label id (text "First Name:")
      text u.firstName

    el_ $ do
      label id (text "Last Name:")
      text u.lastName

    el_ $ do
      label id (text "Age")
      text (cs $ show u.age)

    liveButton Edit (bg Green . hover |: bg GreenLight) "Edit"


viewEdit :: User -> View Contact ()
viewEdit u = onRequest loading $ do
  liveForm Save (pad 10 . gap 10) $ do
    label id $ do
      text "First Name"
      input (name "firstName" . value u.firstName)

    label id $ do
      text "Last Name"
      input (name "lastName" . value u.lastName)

    label id $ do
      text "Age"
      input (name "age" . value (cs $ show u.age))

    submitButton id "Submit"

    liveButton View id (text "Cancel")

    target Contacts $ liveButton (Delete u.id) (bg Red) (text "Delete")
 where
  loading = el (bg Red) "Loading..."


userFormData :: (Page :> es) => Int -> Eff es User
userFormData uid = do
  f <- formData
  firstName <- param "firstName" f
  lastName <- param "lastName" f
  age <- param "age" f
  pure $ User uid firstName lastName age True


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
