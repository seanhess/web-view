{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Contact where

import Boop
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Exception (Exception)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
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
  LoadUser uid -> liftIO $ loadUser uid
  SaveUser u -> liftIO $ saveUser u
 where
  saveUser :: User -> IO ()
  saveUser u = do
    modifyMVar_ var $ \us -> pure $ M.insert u.id u us

  loadUser :: Int -> IO (Maybe User)
  loadUser uid = do
    us <- readMVar var
    pure $ M.lookup uid us

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

effToAction
  :: forall a
   . MVar (Map Int User)
  -> Eff [Users, Page, Error PageError, IOE] a
  -> ActionM a
effToAction var m = do
  ps <- params
  ea <- liftIO . runEff . runErrorNoCallStack @PageError . runPage ps . runUsersIO var $ m :: ActionM (Either PageError a)
  case ea of
    Left e@(ParamError _ _) -> raiseStatus status400 $ L.pack $ show e
    Left NotFound -> next
    Right a -> pure a

param' :: (Page :> es, Parsable a) => L.Text -> Eff es a
param' n = send $ GetParam n

paramAction :: PageAction a => ActionM a
paramAction = do
  ps <- params
  maybe (pure def) pure $ do
    nm <- snd <$> find (\p -> fst p == "action") ps
    fromName nm

-- TODO: pull off Users and handle differently.
--
pageHandler :: (PageAction action) => MVar (Map Int User) -> (action -> Eff [Users, Page, Error PageError, IOE] (View Content ())) -> ActionM (View Content ())
pageHandler var actions = do
  a <- paramAction
  effToAction var $ actions a

-- action :: (Page :> es, PageAction action) => action -> Eff es (View Content ()) -> Eff es ()
-- action = _

-- TODO: typeclass or something
formData :: Page :> es => Int -> Eff es User
formData n = do
  firstName <- param' "firstName"
  lastName <- param' "lastName"
  email <- param' "email"
  pure $ User n firstName lastName email

-- TODO: handle base urls
contactPage :: MVar (Map Int User) -> User -> ActionM (View Content ())
contactPage var u = pageHandler var handle
 where
  handle :: (Users :> es, Page :> es) => Action -> Eff es (View Content ())
  handle Load =
    pure $ viewContact baseUrl u
  handle Edit =
    pure $ editContact baseUrl u
  handle Save = do
    user <- formData u.id
    send $ SaveUser user
    pure $ viewContact baseUrl user

  baseUrl :: Url
  baseUrl =
    let uid = u.id
     in Url [i|/contact/#{uid}|]

viewContact :: Url -> User -> View Content ()
viewContact base u = do
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

      button (action base Edit) "Click to Edit"
    space

editContact :: Url -> User -> View Content ()
editContact base u = do
  form (flexCol . action base Save . hxSwap OuterHTML . hxTarget This) $ do
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

    button (action base Load) "Cancel"
  space

-- formAction :: PageAction action => Url -> action -> Mod Attribute
-- formAction base act = hxPut $ actionUrl base act

action :: PageAction action => Url -> action -> Mod Attribute
action base act = hxPost $ actionUrl base act

actionUrl :: PageAction action => Url -> action -> Url
actionUrl base a =
  base </> L.toStrict (actionName a)
