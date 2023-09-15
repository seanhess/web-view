module Web.Hyperbole.Scotty where

import Data.Default (def)
import Data.List (find)
import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Error.Dynamic
import Network.HTTP.Types (status400)
import Web.Hyperbole.Action
import Web.Hyperbole.Page
import Web.Scotty as Scotty hiding (text)
import Web.Scotty.Internal.Types (RoutePattern (..))
import Web.UI

-- | Read action from the url parameters, default if not found
getAction :: PageAction a => ActionM a
getAction = do
  ps <- params
  maybe (pure def) pure $ do
    nm <- snd <$> find (\p -> fst p == "action") ps
    fromName $ L.toStrict nm

-- | Run our effects into the ActionM monad
runEffAction
  :: forall a
   . Eff [Page, Error PageError, IOE] a
  -> ActionM a
runEffAction m = do
  ps <- params
  ea <- liftIO . runEff . runErrorNoCallStack @PageError . runPage ps $ m :: ActionM (Either PageError a)
  case ea of
    Left e@(ParamError _ _) -> raiseStatus status400 $ L.pack $ show e
    Right a -> pure a

{- | handle a Hyperbole page in Scotty

> page "/contact/:id" $ do
>   uid <- param "id"
>   mu <- run $ loadUser uid
>   user <- maybe next pure mu
>   act <- getAction
>   run $ Contact.handle act user
-}
page :: PageAction action => L.Text -> (action -> ActionM (View a ())) -> ScottyM ()
page cap handler = do
  matchAny (Capture cap) handle
 where
  handle = do
    act <- getAction
    view <- handler act
    mhr <- header "HX-Request"
    Scotty.html $ renderLazyText $ addDocument mhr view

  -- insert top-level document if it is not an HTMX request
  addDocument Nothing v = document v
  addDocument (Just _) v = v

-- TODO: custom top-level document
document :: View a () -> View a ()
document cnt = do
  script "https://unpkg.com/htmx.org@1.9.5"
  cnt
