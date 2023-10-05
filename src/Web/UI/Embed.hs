{-# LANGUAGE TemplateHaskell #-}

module Web.UI.Embed where

import Data.ByteString
import Data.FileEmbed
import Data.Text

cssResetEmbed :: ByteString
cssResetEmbed = $(embedFile "embed/preflight.css")

cssResetLink :: Text
cssResetLink = "<link rel=\"stylesheet\" href=\"https://unpkg.com/tailwindcss@3.3.3/src/css/preflight.css\"/>"
