{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Boop
import Contact (Page, PageAction (..), PageError, User (..), Users (..), loadUser, runEffAction, runUsersIO)
import Contact qualified
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic
import Htmx
import System.FilePath ((</>))
import Web.Scotty hiding (text)
import Web.Scotty.Internal.Types
import Web.UI
import Web.UI.Element hiding (html)
import Web.UI.Style (flexCol, flexRow)

document :: View a () -> View a ()
document cnt = do
  script "https://unpkg.com/htmx.org@1.9.5"
  cnt

page :: L.Text -> ActionM (View a ()) -> ScottyM ()
page cap action = do
  matchAny (Capture cap) handle
 where
  handle = do
    view <- action
    mhr <- header "HX-Request"
    html $ renderLazyText $ addDocument mhr view

  addDocument Nothing v = document v
  addDocument (Just _) v = v

paramAction :: PageAction a => ActionM a
paramAction = do
  ps <- params
  maybe (pure def) pure $ do
    nm <- snd <$> find (\p -> fst p == "action") ps
    fromName nm

main :: IO ()
main = do
  users <- newMVar [(1, User 1 "Joe" "Blow" "joe@blow.com")] :: IO (MVar (Map Int User))
  server users

server :: MVar (Map Int User) -> IO ()
server users = do
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      html $ renderLazyText $ do
        col (pad 10 . gap 5 . border 1 . shadow . bg (HexColor "#F00")) $ do
          el bold $ text "Hello"
          text beam
          button (bg Green . hover |: bg GreenLight . pointer) "CLICK ME"

    page "/contact/:id" $ do
      uid <- param "id"
      mu <- run $ loadUser uid

      user <- maybe next pure mu :: ActionM User
      act <- paramAction

      run . runReader user $ Contact.handle act
 where
  run :: Eff [Users, Page, Error PageError, IOE] a -> ActionM a
  run = runEffAction . runUsersIO users

data AppColor
  = Green
  | GreenLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"
