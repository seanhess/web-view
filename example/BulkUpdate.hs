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
      tcol (width 20) none $ const $ do
        el (cell . padX 10) $ input (att "type" "checkbox")

      tcol (border 1) (el th "First Name") $ \u -> do
        el cell $ text u.firstName

      tcol (border 1) (el th "Last Name") $ \u -> do
        el cell $ text u.lastName

      tcol (border 1) (el th "Email") $ \u -> do
        el cell $ text u.email
 where
  cell = bg GreenLight . pad 2
  th = cell . bold
