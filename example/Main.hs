{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.String.Conversions (cs)

-- import BulkUpdate
-- import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
-- import Control.Monad.IO.Class (liftIO)
-- import Data.List
-- import Data.Map (Map)
-- import Data.Map qualified as M
-- import Data.Maybe (fromMaybe)
-- import Data.String.Interpolate (i)
-- import Data.Text (Text)
-- import Data.Text.Lazy qualified as L

-- import Effectful.Dispatch.Dynamic (send)
-- import Effectful.Error.Dynamic (Error)
-- import Effectful.Reader.Dynamic
-- import Example.Effects.Debug
-- import Example.Effects.Users
-- import Network.HTTP.Types (methodPost, status200, status404)
-- import Network.Wai
-- import System.FilePath ((</>))
-- import Web.Hyperbole
-- import Web.UI hiding (html)
-- import Web.UI.Style (flexCol, flexRow)

import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Network.HTTP.Types (methodPost, status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Web.Hyperbole
import Web.Hyperbole.Effects
import Web.UI
import Web.UI.Render (renderLazyByteString)

main :: IO ()
main = do
  putStrLn "Starting on :3000"
  run 3000 app

app :: Application
app req respond = case (requestMethod req, pathInfo req, queryString req) of
  ("GET", ["hello", msg], _) ->
    runEff . runWai req respond $ do
      view $ do
        el_ "Hello!"
        text msg
  -- respond
  -- \$ responseLBS status200 [textPlain] ("hello " <> cs msg <> cs (show qs))
  ("POST", ["echo"], _) -> do
    runEff . runWai req respond $ do
      bd <- send ReqBody
      let txt = L.toStrict $ L.decodeUtf8 bd
      liftIO $ putStrLn $ T.unpack txt
      view $ do
        el_ "YOU SAID"
        text $ L.toStrict $ L.decodeUtf8 bd
  -- putStrLn "ECHO"
  -- respond $ responseLBS status200 [textPlain] "OK\n"
  _ -> respond $ responseLBS status404 [textPlain] "404 - Not Found"
 where
  textPlain = ("Content-Type", "text/plain")

-- we should `respond` always with HTML...
-- a view

-- we want to run this all through effectful!
type Respond = Response -> IO ResponseReceived

view :: Wai :> es => View () -> Eff es ResponseReceived
view vw = do
  let bd = renderLazyByteString vw
  send $ ResHeader "Content-Type" "text/html"
  send $ ResBody bd
  send Respond

-- send Respond

-- If we use GET that means we want the whole document
-- If we use POST that menas we only want an update
data Route
  = Hello Hello
  | Goodbye
  deriving (Show, Eq)

data Hello
  = Greet String
  | Poof String
  deriving (Show, Eq)

-- Contact.route (users)
-- BulkUpdate.route users

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
