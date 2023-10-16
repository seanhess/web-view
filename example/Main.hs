{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Monad (forever)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
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
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Hyperbole
import Web.Hyperbole.Application
import Web.Hyperbole.Page
import Web.UI
import Web.UI.Embed (cssResetEmbed)


main :: IO ()
main = do
  putStrLn "Starting Examples on :3001"
  users <- initUsers
  run 3001
    $ staticPolicy (addBase "dist")
    $ app users


app :: UserStore -> Application
app users = application document (runUsersIO users . route)
 where
  route :: (Wai :> es, Users :> es) => Route -> Eff es ()
  route (Hello h) = hello h
  route Echo = do
    req <- send ReqBody
    view $ col id $ do
      el id "ECHO:"
      text $ cs req
  route (Contacts mv) = runPageWai $ Contacts.page mv
  route Main = view $ do
    col (gap 10 . pad 10) $ do
      el (bold . fontSize 32) "Examples"
      link (routeUrl (Hello (Greet "World"))) id "Hello World"
      link (routeUrl (Contacts Nothing)) id "Contacts"

  hello (Greet s) = view $ el (pad 10) "GREET" >> text s
  hello (Poof s) = view $ el (pad 10) "POOF" >> text s


-- send Respond
data Route
  = Main
  | Hello Hello
  | Contacts (Maybe Contacts.ViewId)
  | Echo
  deriving (Show, Generic, Eq, PageRoute)


data Hello
  = Greet Text
  | Poof Text
  deriving (Show, Generic, Eq, PageRoute)


document :: BL.ByteString -> BL.ByteString
document cnt =
  [i|<html>
    <head>
      <title>Hyperbole Examples</title>
      <script type="text/javascript" src="/main.js"></script>
      <style type type="text/css">#{cssResetEmbed}</style>
    </head>
    <body>#{cnt}</body>
  </html>|]
