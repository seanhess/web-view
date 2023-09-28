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
import Example.Contact qualified as Contact
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
  handle :: (Wai :> es, Users :> es) => Routes -> Eff es ()
  handle (Hello h) = hello h
  handle (Contact uid cr) = Contact.routes uid cr
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

  hello (Greet s) = view $ el_ "GREET" >> text s
  hello (Poof s) = view $ el_ "POOF" >> text s

-- send Respond
data Routes
  = Users
  | Contact Int Contact.Routes
  | Hello Hello
  | Echo
  deriving (Show, Generic, Route)

data Hello
  = Greet Text
  | Poof Text
  deriving (Show, Generic, Route)
