{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Example.Effects.Users as Users
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Web.Hyperbole
import Web.Hyperbole.Effects
import Web.Hyperbole.Route
import Web.UI
import Web.UI.Render (renderLazyByteString)

main :: IO ()
main = do
  putStrLn "Starting on :3000"
  users <- initUsers
  run 3000 $ app users

app :: UserStore -> Application
app users = application (runUsersIO users . handle)
 where
  handle :: (Wai :> es, Users :> es) => AppRoute -> Eff es ()
  handle (Hello h) = hello h
  handle Goodbye = view $ el_ "GOODBYE"
  handle Echo = do
    req <- send ReqBody
    view $ col_ $ do
      el_ "ECHO:"
      text $ cs req
  handle Users = do
    us <- send LoadUsers
    -- liftIO $ print us
    view $ do
      flip mapM_ us $ \u -> do
        row_ $ do
          text "User: "
          text u.firstName
          text " "
          text u.lastName

  hello (Greet s) = view $ do el_ "GREET"; text s
  hello (Poof s) = view $ do el_ "POOF"; text s

application :: (Route route) => (route -> Eff [Wai, IOE] ()) -> Application
application actions request respond = do
  -- let (method, paths, query) = (requestMethod req, pathInfo req, queryString req)
  case matchRoute (pathInfo request) of
    Nothing -> respond $ responseLBS status404 [textPlain] "Not Found"
    Just rt -> do
      runEff . runWai request respond $ do
        actions rt
        send Respond
 where
  textPlain = ("Content-Type", "text/plain")

view :: Wai :> es => View () -> Eff es ()
view vw = do
  let bd = renderLazyByteString vw
  send $ ResHeader "Content-Type" "text/html"
  send $ ResBody bd

-- send Respond
data AppRoute
  = Hello Hello
  | Goodbye
  | Echo
  | Users
  deriving (Show, Eq, Generic, Route)

data Hello
  = Greet Text
  | Poof Text
  deriving (Show, Eq, Generic, Route)
