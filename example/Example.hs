{-# LANGUAGE OverloadedStrings #-}

module Main where

import Htmx
import Htmx.View
import Web.Scotty

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- A "View" is a single page. Automatically sub-routes? Yeah, it generates its own ids for stuff, etc
