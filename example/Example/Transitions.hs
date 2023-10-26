module Example.Transitions where

import Effectful
import Example.Colors
import Web.Hyperbole
import Web.UI


data Contents = Contents
  deriving (Show, Read, Param)


data Action
  = Expand
  | Collapse
  deriving (Show, Read, Param)


instance LiveView Contents Action


-- need to be able to set bg color of page, sure
page :: (Page :> es) => Eff es ()
page = do
  pageAction content

  pageLoad $ do
    pure $ row (pad 20) $ do
      liveView Contents viewSmall


content :: (Page :> es) => Contents -> Action -> Eff es (View Contents ())
content _ Expand = do
  pure viewBig
content _ Collapse = do
  pure viewSmall


viewSmall :: View Contents ()
viewSmall = do
  col (gap 10 . transition MaxHeight 500 . maxHeight 0 . border 1 . pad 20) $ do
    el id "Hello"
    liveButton Expand btn "Expand"


viewBig :: View Contents ()
viewBig = col (gap 10 . transition MaxHeight 500 . maxHeight 1000 . border 1 . pad 20) $ do
  el_ "One"
  el_ "Two"
  el_ "Three"
  el_ "Four"
  el_ "Five"
  liveButton Collapse (bg Secondary . hover (bg SecondaryLight) . color White . pad 10) "Collapse"


btn :: Mod
btn = bg Primary . hover (bg PrimaryLight) . color White . pad 10
