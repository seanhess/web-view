module BulkUpdate where

import Effectful
import Effectful.Dispatch.Dynamic (send)
import Example.Users
import Web.Hyperbole
import Web.UI

-- handle :: (Users :> es, Page :> es) => Action -> User -> Eff es (View Content ())
-- handle Load = load
-- handle Edit = edit
-- handle Save = save

load :: (Users :> es) => Eff es (View Content ())
load = do
  users <- loadUsers
  pure $ viewUsers users

viewUsers :: [User] -> View Content ()
viewUsers _ = el_ "HI"
