{-# LANGUAGE TemplateHaskell #-}

module Web.Hyperbole.Embed
  ( cssResetEmbed
  , cssResetLink
  , htmxScriptEmbed
  , htmxScriptLink
  )
where

import Data.ByteString
import Data.FileEmbed
import Data.Text
import Web.UI.Embed

htmxScriptEmbed :: ByteString
htmxScriptEmbed = $(embedFile "embed/htmx.js")

htmxScriptLink :: Text
htmxScriptLink =
  "<script src=\"https://unpkg.com/htmx.org@1.9.6\" integrity=\"sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni\" crossorigin=\"anonymous\"></script>"
