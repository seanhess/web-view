{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import Data.Text as T (Text, filter, toLower)
import Example.Colors
import Example.Layout as Layout
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Web.View


main :: IO ()
main = do
  putStrLn "Starting on http://localhost:3010/"
  Warp.run 3010 app


app :: Application
app req respond = do
  case pathInfo req of
    [] -> view examples
    ["responsive"] -> view Layout.responsive
    ["holygrail"] -> view Layout.holygrail
    _ -> notFound
 where
  html h =
    respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] h

  notFound =
    respond $ responseLBS status404 [("Content-Type", "text/plain; charset=utf-8")] "Not Found"

  view v =
    html $ document $ renderLazyByteString () v

  document cnt =
    [i|<html>
      <head><style type="text/css">#{cssResetEmbed}</style></head>
      <body>#{cnt}</body>
    </html>|]


examples :: View c ()
examples = col (pad 20 . gap 15) $ do
  el (bold . fontSize 24) "Layout"
  link "Responsive"
  link "Holy Grail"


link :: Text -> View c ()
link n = tag "a" (att "href" url . color Primary) (text n)
 where
  url = toLower . T.filter (/= ' ') $ n
