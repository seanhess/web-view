module Example.Simple where

import Web.View


simple :: View c ()
simple = col (gap 10) $ do
  el (bold . fontSize 32) "My page"
  button (border 1) "Click Me"
