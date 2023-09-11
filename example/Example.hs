{-# LANGUAGE OverloadedStrings #-}

module Main where

import Boop
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import Htmx
import Web.Scotty hiding (text)
import Web.UI
import Web.UI.Element hiding (html)
import Web.UI.Style (flexCol, flexRow)

-- import Web.UI.Types

data User = User
  { id :: Int
  , firstName :: Text
  , lastName :: Text
  , email :: Text
  }

document :: View a () -> View a ()
document cnt = do
  script "https://unpkg.com/htmx.org@1.9.5"
  cnt

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

    get "/contact/:cid" $ do
      cid <- param "cid"
      us <- liftIO $ readMVar users
      u <- maybe next pure $ M.lookup cid us

      html $ renderLazyText $ document $ viewUser u

    get "/contact/:cid/edit" $ do
      cid <- param "cid"
      us <- liftIO $ readMVar users
      u <- maybe next pure $ M.lookup cid us

      html $ renderLazyText $ do
        form (flexCol . hxPut (Url [i|/contact/#{cid}|]) . hxSwap OuterHTML . hxTarget This) $ do
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

          button (hxGet $ Url [i|/contact/#{cid}|]) "Cancel"
        space

    put "/contact/:cid" $ do
      ps <- params
      liftIO $ print (ps)
      cid <- param "cid"
      liftIO $ print cid
      us <- liftIO $ readMVar users
      u <- maybe next pure $ M.lookup cid us
      fn <- param "firstName"
      ln <- param "lastName"
      em <- param "email"
      let u' = u{firstName = fn, lastName = ln, email = em}
      liftIO $ modifyMVar_ users (\us' -> pure $ M.insert cid u' us')
      html $ renderLazyText $ viewUser u'

viewUser :: User -> View Content ()
viewUser u = do
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

      let cid = u.id
      button (hxGet (Url [i|/contact/#{cid}/edit|])) "Click to Edit"
    space

data AppColor
  = Green
  | GreenLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"

{--
 <div hx-target="this" hx-swap="outerHTML">
    <div><label>First Name</label>: Joe</div>
    <div><label>Last Name</label>: Blow</div>
    <div><label>Email</label>: joe@blow.com</div>
    <button hx-get="/contact/1/edit" class="btn btn-primary">
    Click To Edit
    </button>
</div>

<form hx-put="/contact/1" hx-target="this" hx-swap="outerHTML">
  <div>
    <label>First Name</label>
    <input type="text" name="firstName" value="Joe">
  </div>
  <div class="form-group">
    <label>Last Name</label>
    <input type="text" name="lastName" value="Blow">
  </div>
  <div class="form-group">
    <label>Email Address</label>
    <input type="email" name="email" value="joe@blow.com">
  </div>
  <button class="btn">Submit</button>
  <button class="btn" hx-get="/contact/1">Cancel</button>
</form>

--}
