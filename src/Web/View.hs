{- |
Module:      Web.View
Copyright:   (c) 2023 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <seanhess@gmail.com>
Stability:   experimental
Portability: portable

Easily do some stuff
-}
module Web.View
  ( -- * Use
    -- $use

    -- * Render
    renderText
  , renderLazyText
  , renderLazyByteString

    -- * Element
  , el
  , el_
  , text
  , raw
  , none
  , pre

    -- ** Layout
  , layout
  , root
  , col
  , row
  , space
  , grow
  , collapse
  , scroll
  , nav

    -- ** Inputs
  , form
  , input
  , name
  , value
  , label
  , button

    -- ** Head Metadata
  , script
  , style
  , stylesheet

    -- ** Table
  , table
  , tcol
  , th
  , td

    -- * Mods
  , Mod

    -- * Styles
  , width
  , height
  , minWidth
  , minHeight
  , flexRow
  , flexCol
  , pad
  , gap
  , hide
  , shadow
  , rounded
  , fontSize
  , color
  , bg
  , bold
  , border
  , borderColor
  , pointer
  , transition
  , textAlign

    -- ** Selector Modifiers
  , hover
  , active
  , even
  , odd
  , media
  , parent

    -- * View
  , View
  , context
  , addContext
  , tag
  , att

    -- * Types
  , Sides (..)
  , Media (..)
  , PxRem
  , Url (..)
  , TransitionProperty (..)
  , Ms
  , ToColor (..)
  , HexColor (..)

    -- * CSS Reset
  , module Web.View.Reset
  ) where

import Web.View.Element
import Web.View.Layout
import Web.View.Render
import Web.View.Reset
import Web.View.Style
import Web.View.Types
import Web.View.View
import Prelude hiding (even, head, odd)



{- $use

Here's how you do some stuff

> Woot

Another example

<<docs/pic.png>>

@
'el_' "woot"
@

* Atomic CSS
* Gradual CSS
* Layouts
* Mods and composition
-}
