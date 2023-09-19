{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import BulkUpdate
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
import Example.Contact qualified as Contact
import Example.Effects.Debug
import Example.Users
import System.FilePath ((</>))
import Web.Hyperbole hiding (param)
import Web.Scotty hiding (text)
import Web.Scotty.Internal.Types
import Web.UI hiding (html)
import Web.UI.Style (flexCol, flexRow)

main :: IO ()
main = do
  users <- initUsers
  server users

-- don't reinvent the wheel!
-- just use scotty
server :: UserStore -> IO ()
server users = do
  scotty 3000 $ do
    Contact.route users

-- get "/:word" $ do
--   beam <- param "word"
--   html $ renderLazyText $ do
--     col (pad 10 . gap 5 . border 1 . shadow . bg (HexColor "#F00")) $ do
--       el bold $ text "Hello"
--       text beam
--       button (bg Green . hover |: bg GreenLight . pointer) "CLICK ME"

-- run everything in ActionM, or... whatever
-- scotty works best when the routes are all declared together
-- page "/contact/:id" $ \act -> do
--   uid <- param "id"
--   mu <- run $ loadUser uid
--   user <- maybe next pure mu
--   run $ Contact.handle act user
--
-- page "/bulk" $ \act -> do
--   run $ BulkUpdate.handle act

-- where
--   run :: Eff [Debug, Users, Page, Error PageError, IOE] a -> ActionM a
--   run = runEffAction . runUsersIO users . runDebugIO
