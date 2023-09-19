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

  default matchRoute :: (Generic a, GenRoute (Rep a)) => [Text] -> Maybe a
  matchRoute paths = to <$> genRoute paths

  default routePaths :: (Generic a, GenRoute (Rep a)) => a -> [Text]
  routePaths p = genPaths $ from p

class GenRoute f where
  genRoute :: [Text] -> Maybe (f p)
  genPaths :: f p -> [Text]

-- datatype metadata
instance (GenRoute f) => GenRoute (M1 D c f) where
  genRoute ps = M1 <$> genRoute ps
  genPaths (M1 x) = genPaths x

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

  genPaths (M1 x) =
    let name = conName (undefined :: M1 C c f x)
     in toLower (pack name) : genPaths x

-- Unary constructors
instance GenRoute U1 where
  genRoute [] = pure U1
  genRoute _ = Nothing
  genPaths _ = []

-- Selectors
instance (GenRoute f) => GenRoute (M1 S c f) where
  genRoute ps = do
    M1 <$> genRoute ps

  genPaths (M1 x) = genPaths x

-- Sum types
instance (GenRoute a, GenRoute b) => GenRoute (a :+: b) where
  genRoute ps = L1 <$> genRoute ps <|> R1 <$> genRoute ps
  genPaths (L1 a) = genPaths a
  genPaths (R1 a) = genPaths a

-- Route Param Types
instance GenRoute (K1 R Integer) where
  genRoute = genRouteRead
  genPaths (K1 n) = [pack $ show n]

instance GenRoute (K1 R Text) where
  genRoute [t] = pure $ K1 t
  genRoute _ = Nothing
  genPaths (K1 t) = [t]

genRouteRead :: Read x => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (unpack t)
genRouteRead _ = Nothing
