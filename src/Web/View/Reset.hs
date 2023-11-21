{-# LANGUAGE TemplateHaskell #-}

module Web.View.Reset where

import Data.ByteString
import Data.FileEmbed
import Data.Text


{- | The entire contents of Tailwind's preflight. This or 'cssResetLink' is required.

> import Data.String.Interpolate (i)
>
> document :: Text -> Text
> document cnt =
>   [i|<html>
>     <head>
>       <style type="text/css">#{cssResetEmbed}</style>
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetEmbed :: ByteString
cssResetEmbed = $(embedFile "embed/preflight.css")


{- | Alternatively, this is available as on a CDN

> import Data.String.Interpolate (i)
>
> document :: Text -> Text
> document cnt =
>   [i|<html>
>     <head>
>       <link rel="stylesheet" href="#{cssResetEmbed}">
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetLink :: Text
cssResetLink = "<link rel=\"stylesheet\" href=\"https://unpkg.com/tailwindcss@3.3.3/src/css/preflight.css\"/>"
