{-# LANGUAGE TemplateHaskell #-}

module Web.Hyperbole.Embed
  ( cssResetEmbed
  , cssResetLink
  , scriptEmbed
  )
where

import Data.ByteString
import Data.FileEmbed
import Web.View.Embed


scriptEmbed :: ByteString
scriptEmbed = $(embedFile "client/dist/hyperbole.js")
