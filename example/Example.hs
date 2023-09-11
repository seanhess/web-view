{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty hiding (text)
import Web.UI
import Web.UI.Types

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ renderLazyText $ do
      col (pad 10 . gap 5 . border 1 . shadow . bg (HexColor "F00")) $ do
        el bold $ text "Hello"
        text beam
        button (bg Green . hover |: bg GreenLight . pointer) "CLICK ME"

data AppColor
  = Green
  | GreenLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = Hex "080"
  colorValue GreenLight = Hex "0F0"
