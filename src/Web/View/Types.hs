{-# LANGUAGE DefaultSignatures #-}

module Web.View.Types where

import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local as ES
import GHC.Generics (Generic)
import Text.Casing (kebab)


-- * Views


{- | Views are HTML fragments that carry all atomic 'CSS' used by any child element.

> view :: View c ()
> view = col (pad 10 . gap 10) $ do
>   el (color Red) "Hello"
>   el_ "World"

They can also have a context which can be used to create type-safe or context-aware elements that accept specific child views. See 'Web.View.Element.table'
-}
newtype View context a = View {viewState :: Eff [Reader context, State ViewState] a}
  deriving newtype (Functor, Applicative, Monad)


instance IsString (View context ()) where
  fromString s = viewModContents (const [Text (pack s)])


data ViewState = ViewState
  { contents :: [Content]
  , css :: CSS
  }


instance Semigroup ViewState where
  va <> vb = ViewState (va.contents <> vb.contents) (va.css <> vb.css)


-- | Extract the 'ViewState' from a 'View'
runView :: context -> View context () -> ViewState
runView ctx (View ef) =
  runPureEff . execState (ViewState [] []) . runReader ctx $ ef


-- | Add or modify the contents of the current view
viewModContents :: ([Content] -> [Content]) -> View context ()
viewModContents f = View $ do
  ES.modify $ \s -> s{contents = f s.contents}


-- | Modify the css of the current view
viewModCss :: (CSS -> CSS) -> View context ()
viewModCss f = View $ do
  ES.modify $ \s -> s{css = f s.css}


-- | Get the current context
context :: View context context
context = View ask


-- | Run a view with a specific `context` in a parent 'View' with a different context. This can be used to create type safe view functions, like 'Web.View.Element.table'
addContext :: context -> View context () -> View c ()
addContext ctx vw = do
  -- runs the sub-view in a different context, saving its state
  -- we need to MERGE it
  let st = runView ctx vw
  View $ ES.modify $ \s -> s <> st


data Content
  = Node Element
  | Text Text
  | -- | Raw embedded HTML or SVG. See 'Web.View.Element.raw'
    Raw Text


-- | A single HTML tag. Note that the class attribute is stored separately from the rest of the attributes to make adding styles easier
data Element = Element
  { name :: Name
  , classes :: [[Class]]
  , attributes :: Attributes
  , children :: [Content]
  }


type Attributes = Map Name AttValue
type Attribute = (Name, AttValue)
type Name = Text
type AttValue = Text


-- * Element Modifiers


{- | A function that modifies an element. Allows for ergonomic composition of attributes and styles

> userEmail :: User -> View c ()
> userEmail user = input (fontSize 16 . active) (text user.email)
>   where
>     active = isActive user then bold else id
-}
type Mod = Element -> Element


-- * Atomic CSS


-- TODO: document atomic CSS here?

-- | All the atomic classes used in a 'View'
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
instance ToClassName Float
instance ToClassName Text where
  toClassName = id


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


-- | Px, converted to Rem. Allows for the user to change the document font size and have the app scale accordingly. But allows the programmer to code in pixels to match a design
newtype PxRem = PxRem Int
  deriving newtype (Show, ToClassName, Num)


instance ToStyleValue PxRem where
  toStyleValue (PxRem 0) = "0px"
  toStyleValue (PxRem 1) = "1px"
  toStyleValue (PxRem n) = StyleValue $ show ((fromIntegral n :: Float) / 16.0) <> "rem"


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


-- | Element's attributes do not include class, which is separated. FlatAttributes include the class attribute
newtype FlatAttributes = FlatAttributes {attributes :: Attributes}
  deriving (Generic)


newtype Url = Url Text
  deriving newtype (IsString)


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
