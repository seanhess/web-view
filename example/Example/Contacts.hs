module Example.Contacts where

import Control.Monad (forM_)
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Example.Colors
import Example.Effects.Users
import GHC.Generics (Generic)
import Web.Hyperbole
import Web.UI

-- but there's no action associated with it. Maybe a generic "Load"?
-- there's also no hierarchy
data Target
  = Contact Int
  deriving (Show, Eq, Generic, PageRoute)

page :: (Wai :> es, Users :> es) => Maybe Target -> Eff es ()
page = livePage root actions
 where
  root = do
    us <- send LoadUsers
    view $ viewAll us

  actions (Contact uid) act = do
    u <- loadUser uid
    runAction u act contact

data Contact
  = View
  | Edit
  | Save
  deriving (Show, Read, Eq, Generic, PageAction, PageRoute)

class PageAction a where
  toAction :: String -> Maybe a
  fromAction :: a -> String

data Component inp action es = Component
  { id :: inp -> Url
  , view :: inp -> View ()
  , action :: inp -> action -> Eff es ()
  }

viewComponent :: forall inp action es. Component inp action es -> inp -> View ()
viewComponent comp inp = do
  el (hxSwap InnerHTML . hxTarget This)
    $ comp.view inp

runAction :: inp -> act -> Component inp act es -> Eff es ()
runAction inp act comp = comp.action inp act

contact :: (Wai :> es, Users :> es) => Component User Contact es
contact = Component compId viewContact actions
 where
  compId u = routeUrl $ Contact u.id

  actions u View = do
    view $ viewContact u
  actions u Edit = do
    view $ viewEdit u
  actions u' Save = do
    u <- userFormData u'.id
    send $ SaveUser u
    view $ viewContact u

livePage :: (PageAction act) => Eff es () -> (comp -> act -> Eff es ()) -> Maybe comp -> Eff es ()
livePage = undefined

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
      el (border 1) $ do
        viewComponent contact u

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

    button (action Edit . bg Green . hover |: bg GreenLight) "Click to Edit"

viewEdit :: User -> View ()
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

    button id "Submit"

    button (action View) "Cancel"

component :: Target -> View () -> View ()
component i = el (att "id" (fromUrl . routeUrl $ i) . hxTarget This . hxSwap InnerHTML)

userFormData :: (Wai :> es) => Int -> Eff es User
userFormData uid = do
  firstName <- formParam "firstName"
  lastName <- formParam "lastName"
  email <- formParam "email"
  pure $ User uid firstName lastName email True
