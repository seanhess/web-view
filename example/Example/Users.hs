{-# LANGUAGE LambdaCase #-}

module Example.Users where

import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic

data User = User
  { id :: Int
  , firstName :: Text
  , lastName :: Text
  , email :: Text
  }
  deriving (Show)

-- Load a user AND do next if missing?
data Users :: Effect where
  LoadUser :: Int -> Users m (Maybe User)
  LoadUsers :: Users m [User]
  SaveUser :: User -> Users m ()

type instance DispatchOf Users = 'Dynamic

runUsersIO
  :: (IOE :> es)
  => UserStore
  -> Eff (Users : es) a
  -> Eff es a
runUsersIO var = interpret $ \_ -> \case
  LoadUser uid -> liftIO $ do
    us <- readMVar var
    pure $ M.lookup uid us
  LoadUsers -> liftIO $ do
    us <- readMVar var
    pure $ M.elems us
  SaveUser u -> liftIO $ do
    modifyMVar_ var $ \us -> pure $ M.insert u.id u us

loadUser :: Users :> es => Int -> Eff es (Maybe User)
loadUser = send . LoadUser

loadUsers :: Users :> es => Eff es [User]
loadUsers = send LoadUsers

saveUser :: Users :> es => User -> Eff es ()
saveUser = send . SaveUser

type UserStore = MVar (Map Int User)

initUsers :: MonadIO m => m UserStore
initUsers =
  liftIO $ newMVar [(1, User 1 "Joe" "Blow" "joe@blow.com")]
