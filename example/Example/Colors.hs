module Example.Colors where

import Web.UI

data AppColor
  = Green
  | GreenLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"
