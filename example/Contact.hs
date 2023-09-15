module Contact where

import Effectful
import Effectful.Dispatch.Dynamic
import Example.Users
import Web.Hyperbole
import Web.UI
import Web.UI.Style (flexCol, flexRow)

data Action
  = Edit
  | Save
  | Load
  deriving (Show, Read, PageAction)

instance Default Action where
  def = Load

handle :: (Users :> es, Page :> es) => Action -> User -> Eff es (View Content ())
handle Load u = do
  pure $ viewContact u
handle Edit u = do
  pure $ editContact u
handle Save u = do
  user <- userFormData u.id
  send $ SaveUser user
  pure $ viewContact user

viewContact :: User -> View Content ()
viewContact u = do
  el flexRow $ do
    el (flexCol . hxTarget This . hxSwap OuterHTML . pad 10 . gap 10) $ do
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
  form (flexCol . action Save . hxSwap OuterHTML . hxTarget This) $ do
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

-- TODO: typeclass or something
userFormData :: Page :> es => Int -> Eff es User
userFormData n = do
  firstName <- param "firstName"
  lastName <- param "lastName"
  email <- param "email"
  pure $ User n firstName lastName email
