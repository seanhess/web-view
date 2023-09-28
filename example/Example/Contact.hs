module Example.Contact where

import Effectful
import Effectful.Dispatch.Dynamic
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.UI

-- TODO: right now we can't set routes! We are limited by where we currently are :(

-- it just uses relative urls!
data Routes
  = Contact
  | Edit
  | Save
  deriving (Show, Generic, Route)

routes :: (Wai :> es, Users :> es) => Int -> Routes -> Eff es ()
routes uid Contact = contact uid
routes uid Edit = edit uid
routes uid Save = saveContact uid

contact :: (Wai :> es, Users :> es) => Int -> Eff es ()
contact uid = do
  mu <- send $ LoadUser uid
  u <- maybe notFound pure mu
  view $ viewContact u

edit :: (Wai :> es, Users :> es) => Int -> Eff es ()
edit uid = do
  mu <- send $ LoadUser uid
  u <- maybe notFound pure mu
  view $ editContact u

saveContact :: (Wai :> es, Users :> es) => Int -> Eff es ()
saveContact _uid = view $ el_ "save"

-- route :: UserStore -> ScottyM ()
-- route us = do
--   -- 1. don't mess with get/post. Built-in
--   get "/contact/:id" $ do
--     u <- loadUser
--     view $ viewContact u
--
--   get "/contact/:id/edit" $ do
--     u <- loadUser
--     view $ editContact u
--
--   post "/contact/:id/save" $ do
--     u <- userFormData
--     Users.save us u
--     view $ viewContact u
--  where
--   loadUser :: ActionM User
--   loadUser = do
--     uid <- param "id"
--     mu <- Users.load us uid
--     maybe next pure mu

-- userFormData :: Wai :> es => Eff es User
-- userFormData = do
--   body <- send ReqBody
--   uid <- param "id"
--   firstName <- param "firstName"
--   lastName <- param "lastName"
--   email <- param "email"
--   pure $ User uid firstName lastName email True

viewContact :: User -> View ()
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

      -- this changes the whole page
      -- not the same as action!
      link (Url False $ routePaths Edit) id "Edit"
    space

editContact :: User -> View ()
editContact u = do
  row (hxTarget This . hxSwap OuterHTML) $ do
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

      button id "Submit"

      -- no, we don't want it to re-render everything!
      button (action Contact) "Cancel"
      link (Url False $ routePaths Contact) id "Cancel"
    space

action :: Route a => a -> Mod
action a = hxPost (Url False $ routePaths a)
