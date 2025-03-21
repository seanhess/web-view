{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Web.View


main :: IO ()
main = do
  putStrLn "Starting on http://localhost:3010/"
  Warp.run 3010 app


buttons :: View c ()
buttons = col (gap 10 . pad 20) $ do
  el (bold . fontSize 32) "My page"

  row (gap 10) $ do
    button (btn Primary) "Do Something"
    button (btn Secondary) "Cancel"

  --
  button' Secondary "Another Example"
 where
  -- Make style functions to encourage reuse
  btn c = bg c . hover (bg (light c)) . color White . rounded 3 . pad 15
  light Primary = PrimaryLight
  light Secondary = SecondaryLight
  light _ = Gray

  -- alternatively, we can make View functions
  button' c = button (btn c)


inputs :: View c ()
inputs = do
  layout (pad 20 . gap 10) $ do
    el bold "INPUT"
    input (border 1 . pad 10 . bg White . placeholder "Not Focused")
    input (border 1 . pad 10 . bg White . placeholder "Should Focus" . autofocus)


responsive :: View c ()
responsive = do
  layout (big flexRow) $ do
    nav (gap 10 . pad 20 . bg Primary . color White . small topbar . big sidebar) $ do
      el bold "SIDEBAR"
      el_ "One"
      el_ "Two"
      el_ "Three"

    col (scroll . grow . pad 20 . gap 20 . bg White) $ do
      el (bold . fontSize 24) "Make the window smaller"
      el_ "This demonstrates how to create a responsive design. Resize the window under 800px wide and the nav bar will switch to a top bar"

      col (color Gray . gap 20) $ do
        el_ $ text lorem
        el_ $ text lorem
        el_ $ text lorem
        el_ $ text lorem
        el_ $ text lorem
        el_ $ text lorem
        el_ $ text lorem
 where
  sidebar = width 250 . flexCol
  topbar = height 100 . flexRow
  big = media (MinWidth 800)
  small = media (MaxWidth 800)


holygrail :: View c ()
holygrail = layout id $ do
  row (bg Primary) "Top Bar"
  row grow $ do
    col (bg Secondary) "Left Sidebar"
    col grow $ do
      text "Content Upper Left"
      space
      row id $ do
        space
        text "Content Bottom Right"
    col (bg Secondary) "Right Sidebar"
  row (bg Primary) "Bottom Bar"


stacks :: View c ()
stacks = layout id $ do
  row (bg Primary . bold . pad 10 . color White) "Stacks"
  col (pad 10 . gap 10) $ do
    el_ "Stacks put contents on top of each other"
    stack (border 1) $ do
      layer (bg Light . pad 10) "In the background"
      layer (pad 10) $ do
        row id $ do
          space
          el (bg SecondaryLight . grow . pad 5) "Above"
      layer (pad (XY 15 5)) $ do
        row id $ do
          space
          el (bg Primary . pad 10 . color White) "Max Above!"

    el_ "We can collapse items in a stack so they don't affect the width"
    stack (bg Light . pad 10) $ do
      layer id $ do
        row (gap 5) $ do
          el_ "Some"
          el_ "Stuff"
          el_ "Here"
      layer (popup (BR 0 0)) $ col (pad 10 . bg SecondaryLight) $ do
        el_ "One"
        el_ "Two"
        el_ "Three"
        el_ "Four"

    stack (border 1) $ do
      layer (bg Light) "Background"
      layer (bg SecondaryLight . opacity 0.8 . popup (X 50)) $ do
        el_ "HMM"
        el_ "OK"
      layer (flexRow . bg Warning . opacity 0.8) $ do
        space
        el_ "Overlay"

    el_ "Example Popup Search"
    stack (border 1) $ do
      layer id $ row (bg Light . pad 10) "This is a search bar"
      layer (popup (TRBL 43 5 5 5) . border 1) $ do
        col (bg SecondaryLight . pad (L 50) . pad (R 50)) $ do
          el (hover (bg White) . pointer) "I am a popup"
          el_ "I am a popup"
          el_ "I am a popup"
          el_ "I am a popup"

    col (gap 10) $ do
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "
      el_ "Content asldkjfalsdk jjklasd flkajsd flkjasd lfkjalskdfj alsdkjf "

    col (border 1 . popup (TR 5 5)) "I AM AN ELEMENT"


texts :: View c ()
texts = col (gap 10 . pad 20) $ do
  el (bg Warning . bg Error) "Error"
  el (bg Error . bg Warning) "Warning"

  el (pad 10) $ do
    el (parent "htmx-request" flexRow . hide) "Loading..."
    el (parent "htmx-request" hide . flexRow) "Normal Content"

  el italic "Italic Text"
  el underline "Underline Text"
  el bold "Bold Text"

  ol id $ do
    let nums = list Decimal
    li nums "first"
    li nums "second"
    li nums "third"

  ul id $ do
    li (list Disc) "first"
    li (list Disc) "second"
    li (list None) "third"

  el bold "flexWrap"
  row (gap 5 . width 200 . flexWrap WrapReverse) $ do
    el (border 1 . pad 5) "one"
    el (border 1 . pad 5) "two"
    el (border 1 . pad 5) "three"
    el (border 1 . pad 5) "four"
    el (border 1 . pad 5) "five"
    el (border 1 . pad 5) "six"
    el (border 1 . pad 5) "seven"
    el (border 1 . pad 5) "eight"
    el (border 1 . pad 5) "nine"

  el bold "textWrap"
  el (border 1 . width 200 . textWrap NoWrap) (text lorem)
  el (border 1 . width 200 . textWrap Wrap) (text lorem)

  el bold "css order"
  el (flexCol . flexRow) $ do
    text "WOOT"
    text "BOOT"


lorem :: Text
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."


examples :: View c ()
examples = col (pad 20 . gap 15) $ do
  el (bold . fontSize 24) "Layout"
  link "buttons" lnk "Buttons"
  link "responsive" lnk "Responsive"
  link "holygrail" lnk "Holy Grail"
  link "stacks" lnk "Stacks"
  link "text" lnk "Text"
  link "inputs" lnk "Inputs"
 where
  lnk = color Primary


app :: Application
app req respond = do
  case pathInfo req of
    [] -> view examples
    ["buttons"] -> view buttons
    ["responsive"] -> view responsive
    ["holygrail"] -> view holygrail
    ["stacks"] -> view stacks
    ["text"] -> view texts
    ["inputs"] -> view inputs
    _ -> notFound
 where
  html h =
    respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] h

  notFound =
    respond $ responseLBS status404 [("Content-Type", "text/plain; charset=utf-8")] "Not Found"

  view v =
    html $ document $ renderLazyByteString v

  document cnt =
    [i|<html>
      <head><style type="text/css">#{cssResetEmbed}</style></head>
      <body>#{cnt}</body>
    </html>|]


data AppColor
  = White
  | Light
  | Gray
  | Dark
  | Success
  | Error
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show)


instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue Gray = "#888"
  colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Primary = "#2C74BB"
  colorValue PrimaryLight = "#3281cf"
  colorValue Success = "#D5E6DE"
  colorValue Error = "#F3D8DA"
  colorValue Warning = "#FDF3D1"
  colorValue Secondary = "#5CADDB"
  colorValue SecondaryLight = "#6CBDEB"
