module Contact where

import Effectful
import Effectful.Dispatch.Dynamic
import Example.Users
import Web.Hyperbole
import Web.UI
import Web.UI.Style (flexCol)

data Action
  = Edit
  | Save
  | Load
  deriving (Show, Read, PageAction)

instance Default Action where
  def = Load

handle :: (Users :> es, Page :> es) => Action -> User -> Eff es (View Content ())
handle Load = load
handle Edit = edit
handle Save = save

load :: Monad m => User -> m (View Content ())
load u = pure $ viewContact u

edit :: Monad m => User -> m (View Content ())
edit u = pure $ editContact u

save :: (Users :> es, Page :> es) => User -> Eff es (View Content ())
save u = do
  user <- userFormData u.id
  send $ SaveUser user
  pure $ viewContact user

viewContact :: User -> View Content ()
viewContact u = do
  row (hxTarget This . hxSwap OuterHTML) $ do
    col (pad 10 . gap 10) $ do
      el_ $ do
        label id "First Name:"
        text u.firstName

      el_ $ do
        label id "Last Name:"
        text u.lastName

      el_ $ do
        label id "Email"
        text u.email

      button (action Edit) "Click to Edit"
    space

editContact :: User -> View Content ()
editContact u = do
  row (hxTarget This . hxSwap OuterHTML) $ do
    form (flexCol . action Save) $ do
      label id $ do
        text "First Name"
        input (name "firstName" . value u.firstName)

      label id $ do
        text "Last Name"
        input (name "lastName" . value u.lastName)

      label id $ do
        text "Email"
        input (name "email" . value u.email)

      button id "Submit"

      button (action Load) "Cancel"
    space

userFormData :: Page :> es => Int -> Eff es User
userFormData n = do
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  pure $ User n firstName lastName email
