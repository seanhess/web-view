{-# LANGUAGE DefaultSignatures #-}

module Web.Hyperbole.Route where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text as T (Text, dropWhile, pack, splitOn, toLower, unpack)
import GHC.Generics
import Text.Read (readMaybe)
import Web.UI.Url

routeUrl :: (PageRoute a) => a -> Url
routeUrl a = Url False $ routePaths a

pathSegments :: Text -> [Text]
pathSegments path = T.splitOn "/" $ T.dropWhile (== '/') path

class PageRoute a where
  matchRoute :: [Text] -> Maybe a
  routePaths :: a -> [Text]
  defRoute :: a

  default matchRoute :: (Generic a, GenPageRoute (Rep a)) => [Text] -> Maybe a
  -- this will match a trailing slash, but not if it is missing
  matchRoute [""] = pure defRoute
  matchRoute paths = to <$> genRoute paths

  default routePaths :: (Generic a, Eq a, GenPageRoute (Rep a)) => a -> [Text]
  routePaths p
    | p == defRoute = [""]
    | otherwise = genPaths $ from p

  default defRoute :: (Generic a, GenPageRoute (Rep a)) => a
  defRoute = to genFirst

class GenPageRoute f where
  genRoute :: [Text] -> Maybe (f p)
  genPaths :: f p -> [Text]
  genFirst :: f p

-- datatype metadata
instance (GenPageRoute f) => GenPageRoute (M1 D c f) where
  genRoute ps = M1 <$> genRoute ps
  genPaths (M1 x) = genPaths x
  genFirst = M1 genFirst

-- Constructor names / lines
instance (Constructor c, GenPageRoute f) => GenPageRoute (M1 C c f) where
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
instance GenPageRoute U1 where
  genRoute [] = pure U1
  genRoute _ = Nothing
  genPaths _ = []
  genFirst = U1

-- Selectors
instance (GenPageRoute f) => GenPageRoute (M1 S c f) where
  genRoute ps =
    M1 <$> genRoute ps

  genFirst = M1 genFirst

  genPaths (M1 x) = genPaths x

-- Sum types
instance (GenPageRoute a, GenPageRoute b) => GenPageRoute (a :+: b) where
  genRoute ps = L1 <$> genRoute ps <|> R1 <$> genRoute ps
  genFirst = L1 genFirst
  genPaths (L1 a) = genPaths a
  genPaths (R1 a) = genPaths a

-- Product types
instance (GenPageRoute a, GenPageRoute b) => GenPageRoute (a :*: b) where
  genRoute (p : ps) = do
    ga <- genRoute [p]
    gr <- genRoute ps
    pure $ ga :*: gr
  genRoute _ = Nothing

  genFirst = genFirst :*: genFirst

  genPaths (a :*: b) = genPaths a <> genPaths b

instance (PageRoute sub) => GenPageRoute (K1 R sub) where
  genRoute ts = K1 <$> matchRoute ts

  genFirst = K1 defRoute

  genPaths (K1 sub) = routePaths sub

genRouteRead :: (Read x) => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (unpack t)
genRouteRead _ = Nothing

instance PageRoute Text where
  matchRoute [t] = pure t
  matchRoute _ = Nothing
  routePaths t = [t]
  defRoute = ""

instance PageRoute String where
  matchRoute [t] = pure (unpack t)
  matchRoute _ = Nothing
  routePaths t = [pack t]
  defRoute = ""

instance PageRoute Integer where
  matchRoute = matchRouteRead
  routePaths = routePathsShow
  defRoute = 0

instance PageRoute Int where
  matchRoute = matchRouteRead
  routePaths = routePathsShow
  defRoute = 0

instance (PageRoute a) => PageRoute (Maybe a) where
  matchRoute [] = pure Nothing
  matchRoute ps = Just <$> matchRoute ps

  routePaths _ = []
  defRoute = Nothing

matchRouteRead :: (Read a) => [Text] -> Maybe a
matchRouteRead [t] = readMaybe (unpack t)
matchRouteRead _ = Nothing

routePathsShow :: (Show a) => a -> [Text]
routePathsShow a = [pack (show a)]
