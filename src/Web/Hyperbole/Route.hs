{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Route where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text as T (Text, dropWhile, intercalate, pack, splitOn, toLower, unpack)
import GHC.Generics
import Text.Read (readMaybe)

routeUrl :: (Route a) => a -> Text
routeUrl p = "/" <> intercalate "/" (routePaths p)

pathSegments :: Text -> [Text]
pathSegments path = T.splitOn "/" $ T.dropWhile (== '/') path

class Route a where
  matchRoute :: [Text] -> Maybe a
  routePaths :: a -> [Text]
  defRoute :: a

  default matchRoute :: (Generic a, GenRoute (Rep a)) => [Text] -> Maybe a
  matchRoute [] = pure defRoute
  matchRoute paths = to <$> genRoute paths

  default routePaths :: (Generic a, GenRoute (Rep a)) => a -> [Text]
  routePaths p = genPaths $ from p

  default defRoute :: (Generic a, GenRoute (Rep a)) => a
  defRoute = to genFirst

class GenRoute f where
  genRoute :: [Text] -> Maybe (f p)
  genPaths :: f p -> [Text]
  genFirst :: f p

-- datatype metadata
instance (GenRoute f) => GenRoute (M1 D c f) where
  genRoute ps = M1 <$> genRoute ps
  genPaths (M1 x) = genPaths x
  genFirst = M1 genFirst

-- Constructor names / lines
instance (Constructor c, GenRoute f) => GenRoute (M1 C c f) where
  genRoute (n : ps) = do
    -- take the first path off the list
    -- check that it matches the constructor name
    -- check that the rest matches
    let name = conName (undefined :: M1 C c f x)
    guard (n == toLower (pack name))
    M1 <$> genRoute ps
  genRoute [] = Nothing

  genFirst = M1 genFirst

  genPaths (M1 x) =
    let name = conName (undefined :: M1 C c f x)
     in toLower (pack name) : genPaths x

-- Unary constructors
instance GenRoute U1 where
  genRoute [] = pure U1
  genRoute _ = Nothing
  genPaths _ = []
  genFirst = U1

-- Selectors
instance (GenRoute f) => GenRoute (M1 S c f) where
  genRoute ps =
    M1 <$> genRoute ps

  genFirst = M1 genFirst

  genPaths (M1 x) = genPaths x

-- Sum types
instance (GenRoute a, GenRoute b) => GenRoute (a :+: b) where
  genRoute ps = L1 <$> genRoute ps <|> R1 <$> genRoute ps
  genFirst = L1 genFirst
  genPaths (L1 a) = genPaths a
  genPaths (R1 a) = genPaths a

-- Product types
instance (GenRoute a, GenRoute b) => GenRoute (a :*: b) where
  genRoute (p : ps) = do
    ga <- genRoute [p]
    gr <- genRoute ps
    pure $ ga :*: gr
  genRoute _ = Nothing

  genFirst = genFirst :*: genFirst

  genPaths (a :*: b) = genPaths a <> genPaths b

instance Route sub => GenRoute (K1 R sub) where
  genRoute ts = K1 <$> matchRoute ts

  genFirst = K1 defRoute

  genPaths (K1 sub) = routePaths sub

genRouteRead :: Read x => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (unpack t)
genRouteRead _ = Nothing

instance Route Text where
  matchRoute [t] = pure t
  matchRoute _ = Nothing
  routePaths t = [t]
  defRoute = ""

instance Route String where
  matchRoute [t] = pure (unpack t)
  matchRoute _ = Nothing
  routePaths t = [pack t]
  defRoute = ""

instance Route Integer where
  matchRoute = matchRouteRead
  routePaths = routePathsShow
  defRoute = 0

instance Route Int where
  matchRoute = matchRouteRead
  routePaths = routePathsShow
  defRoute = 0

matchRouteRead :: Read a => [Text] -> Maybe a
matchRouteRead [t] = readMaybe (unpack t)
matchRouteRead _ = Nothing

routePathsShow :: Show a => a -> [Text]
routePathsShow a = [pack (show a)]
