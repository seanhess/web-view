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
  , isActive :: Bool
  }
  deriving (Show)

-- Load a user AND do next if missing?
data Users :: Effect where
  LoadUser :: Int -> Users m (Maybe User)
  LoadUsers :: Users m [User]
  SaveUser :: User -> Users m ()
  ModifyUser :: Int -> (User -> User) -> Users m ()

type instance DispatchOf Users = 'Dynamic

type UserStore = MVar (Map Int User)

runUsersIO
  :: (IOE :> es)
  => UserStore
  -> Eff (Users : es) a
  -> Eff es a
runUsersIO var = interpret $ \_ -> \case
  LoadUser uid -> Example.Users.load var uid
  LoadUsers -> Example.Users.all var
  SaveUser u -> Example.Users.save var u
  ModifyUser uid f -> Example.Users.modify var uid f

load :: MonadIO m => UserStore -> Int -> m (Maybe User)
load var uid = do
  us <- liftIO $ readMVar var
  pure $ M.lookup uid us

save :: MonadIO m => UserStore -> User -> m ()
save var u = do
  liftIO $ modifyMVar_ var $ \us -> pure $ M.insert u.id u us

all :: MonadIO m => UserStore -> m [User]
all var = do
  us <- liftIO $ readMVar var
  pure $ M.elems us

modify :: MonadIO m => UserStore -> Int -> (User -> User) -> m ()
modify var uid f = liftIO $ do
  modifyMVar_ var $ \us -> do
    pure $ M.adjust f uid us

initUsers :: MonadIO m => m UserStore
initUsers =
  liftIO $ newMVar $ M.fromList $ map (\u -> (u.id, u)) users
 where
  users =
    [ User 1 "Joe" "Blow" "joe@blow.com" True
    , User 2 "Sara" "Dane" "sara@dane.com" False
    ]
