{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty hiding (text)
import Web.UI

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ renderLazyText $ do
      col (pad 10 . gap 5 . border 1 . shadow . bg (HexColor "F00")) $ do
        el bold $ text "HI"
        el_ $ text beam
