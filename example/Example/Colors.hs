module Example.Colors where

import Web.UI


data AppColor
  = Green
  | GreenLight
  | GrayLight
  | Red
  deriving (Show)


instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"
  colorValue GrayLight = HexColor "DDD"
  colorValue Red = HexColor "F00"
