module Web.Hyperbole.Scotty where

import Data.Default (def)
import Data.List (find)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Error.Dynamic
import Network.HTTP.Types (status400)
import Web.Hyperbole.Action
import Web.Hyperbole.Page hiding (params)
import Web.Scotty.Trans as Scotty hiding (text)
import Web.Scotty.Internal.Types (RoutePattern (..))
import Web.UI

-- | Read action from the url parameters, default if not found
getAction :: (PageAction a, Monad m) => ActionT L.Text m a
getAction = do
  ps <- params
  maybe (pure def) pure $ do
    nm <- snd <$> find (\p -> fst p == "action") ps
    fromName $ L.toStrict nm

-- | Run our effects into the ActionM monad
runEffAction
  :: forall a m
   . (MonadIO m)
  => Eff [Page, Error PageError, IOE] a
  -> ActionT L.Text m a
runEffAction m = do
  ps <- fmap strict <$> params
  ea <- liftIO . runEff . runErrorNoCallStack @PageError . runPage ps $ m :: ActionT L.Text m (Either PageError a)
  case ea of
    Left e@(ParamError _ _) -> raiseStatus status400 $ L.pack $ show e
    Right a -> pure a
 where
  strict (a, b) = (L.toStrict a, L.toStrict b)

{- | handle a Hyperbole page in Scotty

> page "/contact/:id" $ do
>   uid <- param "id"
>   mu <- run $ loadUser uid
>   user <- maybe next pure mu
>   act <- getAction
>   run $ Contact.handle act user
-}
page :: (MonadIO m, PageAction action) => L.Text -> (action -> ActionT L.Text m (View ())) -> ScottyT L.Text m ()
page cap handler = do
  matchAny (Capture cap) handle
 where
  handle = do
    act <- getAction
    vw <- handler act
    mhr <- header "HX-Request"
    Scotty.html $ renderLazyText $ addDocument mhr vw

  -- insert top-level document if it is not an HTMX request
  addDocument Nothing v = document v
  addDocument (Just _) v = v

-- TODO: custom top-level document
-- TODO: embed js
document :: View () -> View ()
document cnt = do
  script "https://unpkg.com/htmx.org@1.9.5"
  stylesheet "https://unpkg.com/modern-normalize@2.0.0/modern-normalize.css"
  style "table tr td, table tr th { padding: 0; }"
  cnt

view :: Monad m => View () -> ActionT L.Text m ()
view vw = do
  mhr <- Scotty.header "HX-Request"
  Scotty.html $ renderLazyText $ addDocument mhr vw
 where
  -- insert top-level document if it is not an HTMX request
  addDocument Nothing v = document v
  addDocument (Just _) v = v
