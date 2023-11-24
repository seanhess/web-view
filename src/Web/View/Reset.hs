{-# LANGUAGE TemplateHaskell #-}

module Web.View.Reset where

import Data.ByteString
import Data.FileEmbed
import Data.Text


{- | Default CSS to clear unintuitive default styles. This or 'cssResetLink' is required.

> import Data.String.Interpolate (i)
>
> toDocument :: Text -> Text
> toDocument cnt =
>   [i|<html>
>     <head>
>       <style type="text/css">#{cssResetEmbed}</style>
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetEmbed :: ByteString
cssResetEmbed = $(embedFile "embed/preflight.css")


{- | Alternatively, the reset is available as on a CDN

> import Data.String.Interpolate (i)
>
> toDocument :: Text -> Text
> toDocument cnt =
>   [i|<html>
>     <head>
>       <link rel="stylesheet" href="#{cssResetEmbed}">
>     </head>
>     <body>#{cnt}</body>
>   </html>|]
-}
cssResetLink :: Text
cssResetLink = "<link rel=\"stylesheet\" href=\"https://unpkg.com/tailwindcss@3.3.3/src/css/preflight.css\"/>"
