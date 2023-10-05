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
import Example.Contacts qualified as Contacts
import Example.Effects.Users as Users
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, QueryItem, methodPost, status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Web.Hyperbole
import Web.UI

main :: IO ()
main = do
  putStrLn "Starting Examples on :3001"
  users <- initUsers
  run 3001 $ app users

app :: UserStore -> Application
app users = application (runUsersIO users . route)
 where
  route :: (Wai :> es, Users :> es) => Route -> Eff es ()
  route (Hello h) = hello h
  route Echo = do
    req <- send ReqBody
    view $ col id $ do
      el id "ECHO:"
      text $ cs req
  route (Contacts rt) = Contacts.routes rt
  route Main = view $ do
    col (gap 10 . pad 10) $ do
      el (bold . fontSize 32) "Examples"
      link (routeUrl (Hello (Greet "World"))) id "Hello World"
      link (routeUrl (Contacts Contacts.Root)) id "Contacts"

  hello (Greet s) = view $ el (pad 10) "GREET" >> text s
  hello (Poof s) = view $ el (pad 10) "POOF" >> text s

-- send Respond
data Route
  = Main
  | Hello Hello
  | Contacts Contacts.Route
  | Echo
  deriving (Show, Generic, Eq, PageRoute)

data Hello
  = Greet Text
  | Poof Text
  deriving (Show, Generic, Eq, PageRoute)
