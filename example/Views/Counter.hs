module Views.Counter where

import Htmx.View.Types
import Lucid.Html5

view :: View ()
view = View $ div_ "Hello"
