{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Contact where

import Boop
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Exception (Exception)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.Reader.Dynamic
import Htmx
import Network.HTTP.Types (status400)
import Text.Read
import Web.Scotty (ActionM, Param, Parsable (..), next, params, raiseStatus)
import Web.UI
import Web.UI.Element hiding (html)
import Web.UI.Style (flexCol, flexRow)
import Web.UI.Types (Attribute)

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
  SaveUser :: User -> Users m ()

type instance DispatchOf Users = 'Dynamic

-- there's absolutely no reason to
data Action
  = Edit
  | Save
  | Load
  deriving (Show, Read)

-- Read, default

class PageAction a where
  actionName :: a -> L.Text
  default actionName :: Show a => a -> L.Text
  actionName = L.pack . show

  fromName :: L.Text -> Maybe a
  default fromName :: Read a => L.Text -> Maybe a
  fromName = readMaybe . L.unpack

  def :: a

instance PageAction Action where
  def = Load

data Page :: Effect where
  GetParam :: Parsable a => L.Text -> Page m a

type instance DispatchOf Page = 'Dynamic

data PageError
  = ParamError L.Text L.Text
  | NotFound
  deriving (Show, Exception)

runUsersIO
  :: (IOE :> es)
  => MVar (Map Int User)
  -> Eff (Users : es) a
  -> Eff es a
runUsersIO var = interpret $ \_ -> \case
  LoadUser uid -> liftIO $ do
    us <- readMVar var
    pure $ M.lookup uid us
  SaveUser u -> liftIO $ do
    modifyMVar_ var $ \us -> pure $ M.insert u.id u us

loadUser :: Users :> es => Int -> Eff es (Maybe User)
loadUser = send . LoadUser

runPage
  :: (Error PageError :> es)
  => [Param]
  -> Eff (Page : es) a
  -> Eff es a
runPage ps =
  interpret $ \_ -> \case
    GetParam k -> getParam k
 where
  getParam :: (Parsable a, Error PageError :> es) => L.Text -> Eff es a
  getParam k = do
    tv <- maybe (throwError $ ParamError "Missing" k) pure $ lookup k ps
    either (throwError . ParamError k) pure $ parseParam tv

runEffAction
  :: forall a
   . Eff [Page, Error PageError, IOE] a
  -> ActionM a
runEffAction m = do
  ps <- params
  -- runs to IO
  ea <- liftIO . runEff . runErrorNoCallStack @PageError . runPage ps $ m :: ActionM (Either PageError a)
  case ea of
    Left e@(ParamError _ _) -> raiseStatus status400 $ L.pack $ show e
    Left NotFound -> next
    Right a -> pure a

param' :: (Page :> es, Parsable a) => L.Text -> Eff es a
param' n = send $ GetParam n

-- TODO: pull off Users and handle differently.

-- How do we get the initial page state?
-- maybe an effect??
handle :: (Users :> es, Page :> es, Reader User :> es) => Action -> Eff es (View Content ())
handle Load = do
  u <- ask
  pure $ viewContact u
handle Edit = do
  u <- ask
  pure $ editContact u
handle Save = do
  (u :: User) <- ask
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

-- formAction :: PageAction action => Url -> action -> Mod Attribute
-- formAction base act = hxPut $ actionUrl base act

action :: PageAction action => action -> Mod Attribute
action act = hxPost $ actionUrl act

actionUrl :: PageAction action => action -> Url
actionUrl a =
  Url $ "?action=" <> L.toStrict (actionName a)

-- TODO: typeclass or something
userFormData :: Page :> es => Int -> Eff es User
userFormData n = do
  firstName <- param' "firstName"
  lastName <- param' "lastName"
  email <- param' "email"
  pure $ User n firstName lastName email
