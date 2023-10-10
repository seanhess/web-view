module Example.Contacts where

import Control.Monad (forM_)
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.UI

data Route
  = Root
  | View Int
  | Edit Int
  | Save Int
  deriving (Show, Eq, Generic, PageRoute)

routes :: (Wai :> es, Users :> es) => Route -> Eff es ()
routes Root = do
  us <- send LoadUsers
  view $ viewAll us
routes (Edit uid) = do
  u <- loadUser uid
  view $ viewEdit u
routes (View uid) = do
  u <- loadUser uid
  view $ viewContact u
routes (Save uid) = do
  u <- userFormData uid
  send $ SaveUser u
  view $ viewContact u

loadUser :: (Wai :> es, Users :> es) => Int -> Eff es User
loadUser uid = do
  mu <- send (LoadUser uid)
  -- not found! Oh no!
  maybe notFound pure mu

-- hxRequest :: Mod -> Mod
-- hxRequest = prefix "hx-request"

viewAll :: [User] -> View ()
viewAll us = do
  row (pad 10 . gap 10) $ do
    forM_ us $ \u -> do
      el (border 1) $ swapTarget InnerHTML $ do
        viewContact u

userFormData :: (Wai :> es) => Int -> Eff es User
userFormData uid = do
  firstName <- formParam "firstName"
  lastName <- formParam "lastName"
  email <- formParam "email"
  pure $ User uid firstName lastName email True

viewContact :: User -> View ()
viewContact u = do
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

    button (action (Edit u.id) . bg Green . hover |: bg GreenLight) "Click to Edit"

viewEdit :: User -> View ()
viewEdit u = do
  form (action (Save u.id) . pad 10 . gap 10) $ do
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

    button (action $ View u.id) "Cancel"
