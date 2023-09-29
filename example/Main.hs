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
  putStrLn "Starting on :3000"
  users <- initUsers
  run 3000 $ app users

app :: UserStore -> Application
app users = application (runUsersIO users . handle)
 where
  handle :: (Wai :> es, Users :> es) => Route -> Eff es ()
  handle (Hello h) = hello h
  handle Echo = do
    req <- send ReqBody
    view $ col id $ do
      el id "ECHO:"
      text $ cs req
  handle (Contacts rt) = Contacts.routes rt

  hello (Greet s) = view $ el_ "GREET" >> text s
  hello (Poof s) = view $ el_ "POOF" >> text s

-- send Respond
data Route
  = Contacts Contacts.Route
  | Hello Hello
  | Echo
  deriving (Show, Generic, PageRoute)

data Hello
  = Greet Text
  | Poof Text
  deriving (Show, Generic, PageRoute)
