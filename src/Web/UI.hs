module Web.UI
  ( -- * Render
    renderText
  , renderLazyText

    -- * Element
  , att
  , el
  , el_
  , text
  , none
  , row
  , row_
  , col
  , col_
  , space

    -- * Style
  , cls
  , cls1
  , pad
  , padY
  , padX
  , gap
  , grow
  , shadow
  , bg
  , bold
  , border
  , pointer
  , color
  , ToColor (..)
  , (|:)
  , hover

    -- * Types
  , View
  , Content
  , Document
  , Mod
  ) where

import Web.UI.Element
import Web.UI.Render
import Web.UI.Style
import Web.UI.Types
import Prelude hiding (head)
