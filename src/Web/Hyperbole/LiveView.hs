{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.LiveView where

import Data.Text
import Text.Read
import Web.UI


class (Param id, Param (Action id)) => LiveView id where
  type Action id


liveView :: (LiveView id, Param id) => id -> View id () -> View ctx ()
liveView vid vw = do
  el (att "id" (toParam vid) . att "class" "live-view")
    $ addContext vid vw


liveForm :: (LiveView id, Param (Action id), Param id) => Action id -> Mod -> View id () -> View id ()
liveForm a f cd = do
  c <- context
  tag "form" (dataAction a . dataTarget c . f . flexCol) cd


submitButton :: Mod -> View c () -> View c ()
submitButton f = tag "button" (att "type" "submit" . f)


liveButton :: (LiveView id, Param (Action id), Param id) => Action id -> Mod -> View id () -> View id ()
liveButton a f cd = do
  c <- context
  tag "button" (dataAction a . dataTarget c . f) cd


onRequest :: View id () -> View id () -> View id ()
onRequest a b = do
  el (att "class" "progress") a
  el (att "class" "complete") b


-- I'd rather not make ANOTHER copy of liveButton

dataAction :: (Param a) => a -> Mod
dataAction = att "data-action" . toParam


dataTarget :: (Param a) => a -> Mod
dataTarget = att "data-target" . toParam


-- | Change the target of any code running inside, allowing actions to target other live views on the page
target :: (LiveView id) => id -> View id () -> View a ()
target vid vw =
  addContext vid vw


class Param a where
  -- not as flexible as FromHttpApiData, but derivable
  parseParam :: Text -> Maybe a
  default parseParam :: (Read a) => Text -> Maybe a
  parseParam = readMaybe . unpack


  toParam :: a -> Text
  default toParam :: (Show a) => a -> Text
  toParam = pack . show


instance Param Integer
instance Param Float
instance Param Int


instance Param Text where
  parseParam = pure
  toParam = id
