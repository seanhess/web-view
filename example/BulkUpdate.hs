module BulkUpdate where

import Effectful
import Example.Colors
import Example.Users
import GHC.Generics
import Web.Hyperbole
import Web.UI

data Action = Load
  deriving (Show, Eq, Read, Generic, Default, PageAction)

handle :: (Users :> es, Page :> es) => Action -> Eff es (View Content ())
handle Load = load

-- the `pure` here is weird?
-- maybe `view` function?
load :: (Users :> es) => Eff es (View Content ())
load = viewUsers <$> loadUsers

viewUsers :: [User] -> View Content ()
viewUsers users = do
  form (pad 10) $ do
    table (border 1) users $ do
      tcol (cell . width 20) none $ const $ do
        input (att "type" "checkbox")

      tcol cell (th "First Name") $ \u -> do
        text u.firstName

      tcol cell (th "Last Name") $ \u -> do
        text u.lastName

      tcol cell (th "Email") $ \u -> do
        text u.email
 where
  cell = bg GreenLight . pad 2 . border 1
  th = el bold
