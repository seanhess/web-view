{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.View.Types where

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Debug.Trace
import Effectful
import Effectful.State.Static.Local
import GHC.Generics (Generic)
import Network.HTTP.Types (Query, parseQuery, renderQuery)
import Numeric (showFFloat)
import Text.Casing (kebab)


data Content
  = Node Element
  | Text Text
  | -- | Raw embedded HTML or SVG. See 'Web.View.Element.raw'
    Raw Text


-- | A single HTML tag. Note that the class attribute is stored separately from the rest of the attributes to make adding styles easier
data Element = Element
  { name :: Name
  , attributes :: Attributes
  , children :: [Content]
  }


data Attributes = Attributes
  { classes :: [Class]
  , other :: Map Name AttValue
  }
type Attribute = (Name, AttValue)
type Name = Text
type AttValue = Text


-- * Attribute Modifiers


{- | Element functions expect a Mod function as their first argument that adds attributes and classes.

> userEmail :: User -> View c ()
> userEmail user = input (fontSize 16 . active) (text user.email)
>   where
>     active = isActive user then bold else id
-}
type Mod = Attributes -> Attributes


-- * Atomic CSS


-- TODO: document atomic CSS here?

-- | All the atomic classes used in a 'Web.View.View'
type CSS = Map Selector Class


-- | Atomic classes include a selector and the corresponding styles
data Class = Class
  { selector :: Selector
  , properties :: Styles
  }


-- | The styles to apply for a given atomic 'Class'
type Styles = Map Name StyleValue


-- | The selector to use for the given atomic 'Class'
data Selector = Selector
  { parent :: Maybe Text
  , pseudo :: Maybe Pseudo
  , media :: Maybe Media
  , className :: ClassName
  }
  deriving (Eq, Ord)


instance IsString Selector where
  fromString s = Selector Nothing Nothing Nothing (fromString s)


-- | Create a 'Selector' given only a 'ClassName'
selector :: ClassName -> Selector
selector = Selector Nothing Nothing Nothing


-- | A class name
newtype ClassName = ClassName
  { text :: Text
  }
  deriving newtype (Eq, Ord, IsString)


-- | Convert a type into a className segment to generate unique compound style names based on the value
class ToClassName a where
  toClassName :: a -> Text
  default toClassName :: (Show a) => a -> Text
  toClassName = T.toLower . T.pack . show


instance ToClassName Int
instance ToClassName Text where
  toClassName = id
instance ToClassName Float where
  toClassName f = pack $ map noDot $ showFFloat (Just 3) f ""
   where
    noDot '.' = '-'
    noDot c = c


{- | Psuedos allow for specifying styles that only apply in certain conditions. See `Web.View.Style.hover` etc

> el (color Primary . hover (color White)) "hello"
-}
data Pseudo
  = Hover
  | Active
  | Even
  | Odd
  deriving (Show, Eq, Ord)


-- | The value of a css style property
newtype StyleValue = StyleValue String
  deriving newtype (IsString, Show)


-- | Use a type as a css style property value
class ToStyleValue a where
  toStyleValue :: a -> StyleValue
  default toStyleValue :: (Show a) => a -> StyleValue
  toStyleValue = StyleValue . kebab . show


instance ToStyleValue String where
  toStyleValue = StyleValue


instance ToStyleValue Text where
  toStyleValue = StyleValue . unpack


instance ToStyleValue Int


instance ToStyleValue Float where
  -- this does not convert to a percent, just a ratio
  toStyleValue n = StyleValue $ showFFloat (Just 2) n ""


data Length
  = -- | Px, converted to Rem. Allows for the user to change the document font size and have the app scale accordingly. But allows the programmer to code in pixels to match a design
    PxRem PxRem
  | Pct Float
  deriving (Show)


instance ToClassName Length where
  toClassName (PxRem p) = toClassName p
  toClassName (Pct p) = toClassName p


newtype PxRem = PxRem' Int
  deriving newtype (Show, ToClassName, Num, Eq, Integral, Real, Ord, Enum)


instance Num Length where
  -- only support numeric literals
  a + _ = a
  a * _ = a
  abs (PxRem a) = PxRem (abs a)
  abs (Pct a) = Pct (abs a)
  signum (PxRem a) = PxRem (signum a)
  signum (Pct a) = Pct (signum a)
  negate (PxRem a) = PxRem (negate a)
  negate (Pct a) = Pct (negate a)
  fromInteger n = PxRem (fromInteger n)


instance ToStyleValue PxRem where
  toStyleValue (PxRem' 0) = "0px"
  toStyleValue (PxRem' 1) = "1px"
  toStyleValue (PxRem' n) = StyleValue $ show ((fromIntegral n :: Float) / 16.0) <> "rem"


instance ToStyleValue Length where
  toStyleValue (PxRem p) = toStyleValue p
  toStyleValue (Pct n) = StyleValue $ showFFloat (Just 1) (n * 100) "" <> "%"


-- | Milliseconds, used for transitions
newtype Ms = Ms Int
  deriving (Show)
  deriving newtype (Num, ToClassName)


instance ToStyleValue Ms where
  toStyleValue (Ms n) = StyleValue $ show n <> "ms"


-- | Media allows for responsive designs that change based on characteristics of the window. See [Layout Example](https://github.com/seanhess/web-view/blob/master/example/Example/Layout.hs)
data Media
  = MinWidth Int
  | MaxWidth Int
  deriving (Eq, Ord)


{- | Options for styles that support specifying various sides. This has a "fake" Num instance to support literals

> border 5
> border (X 2)
> border (TRBL 0 5 0 0)
-}
data Sides a
  = All a
  | TRBL a a a a
  | X a
  | Y a
  | XY a a


-- Num instance is just to support literals
instance (Num a) => Num (Sides a) where
  a + _ = a
  a * _ = a
  abs a = a
  negate a = a
  signum a = a
  fromInteger n = All (fromInteger n)


-- | Element's attributes do not include class, which is separated. FlatAttributes generate the class attribute and include it
newtype FlatAttributes = FlatAttributes {attributes :: Map Name AttValue}
  deriving (Generic)


data Url = Url
  { scheme :: Text
  , domain :: Text
  , path :: [Text]
  , query :: Query
  }
  deriving (Show, Eq)
instance IsString Url where
  fromString = url . pack


url :: Text -> Url
url t = runPureEff $ evalState (T.toLower t) $ do
  s <- scheme
  d <- domain s
  ps <- paths
  q <- query
  pure $ Url{scheme = s, domain = d, path = ps, query = q}
 where
  parse :: (State Text :> es) => (Char -> Bool) -> Eff es Text
  parse b = do
    inp <- get
    let match = T.takeWhile b inp
        rest = T.dropWhile b inp
    put rest
    pure match

  string :: (State Text :> es) => Text -> Eff es (Maybe Text)
  string pre = do
    inp <- get
    case T.stripPrefix pre inp of
      Nothing -> pure Nothing
      Just rest -> do
        traceM $ show ("Prefix" :: String, rest, pre, inp)
        put rest
        pure (Just pre)

  -- it's either scheme AND domain, or relative path
  scheme = do
    http <- string "http://"
    https <- string "https://"
    pure $ fromMaybe "" $ http <|> https

  domain "" = pure ""
  domain _ = parse (not . isDomainSep)

  path = parse (not . isQuerySep)
  paths = do
    p <- path
    pure $ filter (not . T.null) $ T.splitOn "/" p

  query :: (State Text :> es) => Eff es Query
  query = do
    q <- parse (/= '\n')
    pure $ parseQuery $ encodeUtf8 q

  isDomainSep '/' = True
  isDomainSep _ = False

  isQuerySep '?' = True
  isQuerySep _ = False


renderUrl :: Url -> Text
renderUrl u = u.scheme <> u.domain <> "/" <> T.intercalate "/" u.path <> decodeUtf8 (renderQuery True u.query)


-- ** Colors


{- | ToColor allows you to create a type containing your application's colors:

> data AppColor
>   = White
>   | Primary
>   | Dark
>
> instance ToColor AppColor where
>   colorValue White = "#FFF"
>   colorValue Dark = "#333"
>   colorValue Primary = "#00F"
>
> hello :: View c ()
> hello = el (bg Primary . color White) "Hello"
-}
class ToColor a where
  colorValue :: a -> HexColor
  colorName :: a -> Text
  default colorName :: (Show a) => a -> Text
  colorName = T.toLower . pack . show


instance ToColor HexColor where
  colorValue c = c
  colorName (HexColor a) = T.dropWhile (== '#') a


-- | Hexidecimal Color. Can be specified with or without the leading '#'. Recommended to use an AppColor type instead of manually using hex colors. See 'Web.View.Types.ToColor'
newtype HexColor = HexColor Text


instance ToStyleValue HexColor where
  toStyleValue (HexColor s) = StyleValue $ "#" <> unpack (T.dropWhile (== '#') s)


instance IsString HexColor where
  fromString = HexColor . T.dropWhile (== '#') . T.pack


data Align
  = Center
  deriving (Show, ToClassName, ToStyleValue)
