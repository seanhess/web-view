module BulkUpdate where

import Effectful
import Example.Effects.Debug
import Example.Colors
import Data.Text (pack)
import Example.Users
import GHC.Generics
import Web.Hyperbole
import Web.UI

data Action
  = Load
  | Activate
  -- | Deactivate
  deriving (Show, Eq, Read, Generic, PageAction)

instance Default Action where
  def = Load

handle :: (Users :> es, Debug :> es, Page :> es) => Action -> Eff es (View Content ())
handle Load = load
handle Activate = activate
-- handle Load = load

-- the `pure` here is weird?
-- maybe `view` function?
load :: (Users :> es) => Eff es (View Content ())
load = viewUsers <$> loadUsers

activate :: forall es. (Users :> es, Debug :> es, Page :> es) => Eff es (View Content ())
activate = do
  ids <- param "ids" :: Eff es [Int]
  us <- loadUsers
  dump "PARAMS" ids
  pure $ viewUsers us
  

viewUsers :: [User] -> View Content ()
viewUsers users = do
  form (pad 10 . action Activate . hxSwap OuterHTML) $ do
    table (border 1) users $ do
      tcol (cell . width 20) none $ \u -> do
        input (att "type" "checkbox" . name "ids" . value (pack $ show u.id))

      tcol cell (th "First Name") $ \u -> do
        text u.firstName

      tcol cell (th "Last Name") $ \u -> do
        text u.lastName

      tcol cell (th "Email") $ \u -> do
        text u.email

    button id "Activate"
 where
  cell = pad 4 . border 1 . borderColor GrayLight
  th = el bold
