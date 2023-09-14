{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Boop
import Contact
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy qualified as L
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

-- -- TODO: custom monad
-- newtype Page inp a = Page {runPage :: inp -> ActionM a}
--
-- instance Functor (Page inp) where
--   fmap f (Page ima) = Page $ \inp -> do
--     a <- ima inp
--     pure $ f a
--
-- instance Applicative (Page inp) where
--   pure a = Page $ \_ -> pure a
--
--   pf <*> fa = Page $ \inp -> do
--     f <- pf.runPage inp
--     a <- fa.runPage inp
--     pure $ f a
--
-- instance Monad (Page inp) where
--   ma >>= amb = Page $ \inp -> do
--     a <- ma.runPage inp
--     (amb a).runPage inp

-- page :: RoutePattern -> ActionM inp -> Page inp a -> ScottyM ()
-- page rp input page = do
--   matchAny rp

page :: L.Text -> ActionM (View a ()) -> ScottyM ()
page cap handler = do
  matchAny (Capture cap) handle
  matchAny (Capture $ cap <> "/:action") handle
 where
  handle = do
    view <- handler
    mhr <- header "HX-Request"
    html $ renderLazyText $ addDocument mhr view

  addDocument Nothing v = document v
  addDocument (Just _) v = v

loadUser :: MVar (Map Int User) -> Int -> ActionM User
loadUser users uid = do
  us <- liftIO $ readMVar users
  maybe next pure $ M.lookup uid us

main :: IO ()
main = do
  users <- newMVar [(1, User 1 "Joe" "Blow" "joe@blow.com")] :: IO (MVar (Map Int User))
  scotty 3000 $ do
    get "/:word" $ do
      beam <- param "word"
      html $ renderLazyText $ do
        col (pad 10 . gap 5 . border 1 . shadow . bg (HexColor "#F00")) $ do
          el bold $ text "Hello"
          text beam
          button (bg Green . hover |: bg GreenLight . pointer) "CLICK ME"

    -- 1. what do we need scotty for?  You might want to implement security, or something...
    -- but once we're inside the handler, we aren't scotty specific any more
    -- a warp-handler would be
    page "/contact/:id" $ do
      -- could be served with a simple load transformer...
      -- but...
      -- we aren't running in our effect here. We need to escape it?
      uid <- param "id"
      -- TODO: unifying effect, use scotty trans
      -- nothing fancy, just load it here!
      user <- loadUser users uid

      Contact.contactPage users user

data AppColor
  = Green
  | GreenLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"
