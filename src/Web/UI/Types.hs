module Web.UI.Types where

import Control.Monad.State.Strict (MonadState, State, execState, modify)
import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T

-- import Data.Text.Lazy qualified as L

type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

data Class = Class
  { className :: ClassName
  , classProperties :: ClassProps
  }

type ClassProps = Map Text Style

className :: Class -> Text
className c = classNameText c.className

classNameSelector :: ClassName -> Text
classNameSelector (ClassName Nothing n) = n
classNameSelector (ClassName p n) =
  pseudoText p <> "\\:" <> n <> ":" <> pseudoText p

classNameText :: ClassName -> Text
classNameText (ClassName Nothing n) = n
classNameText (ClassName p n) =
  pseudoText p <> ":" <> n

pseudoText :: Maybe Pseudo -> Text
pseudoText Nothing = ""
pseudoText (Just p) = T.toLower $ pack $ show p

data ClassName = ClassName
  { pseudo :: Maybe Pseudo
  , name :: Text
  }
  deriving (Eq)

instance IsString ClassName where
  fromString s = ClassName Nothing (pack s)

instance Ord ClassName where
  compare a b = compare (classNameText a) (classNameText b)

data Pseudo
  = Hover
  deriving (Show, Eq)

data Style = Style
  { units :: Units
  , value :: String
  }

instance Show Style where
  show cv = unitsValue cv.units cv.value

instance IsString Style where
  fromString = Style None

data Units
  = None
  | Px
  | Rem
  | Hex
  | RGB

unitsValue :: Units -> String -> String
unitsValue None s = s
unitsValue Px s = s <> "px"
unitsValue Rem s = s <> "rem"
unitsValue Hex s = "#" <> s
-- it needs to have a string?
-- this might need to get more complicated
unitsValue RGB s = "rgb(" <> s <> ")"

attribute :: Name -> AttValue -> Attribute
attribute n v = (n, v)

data Element = Element
  { name :: Name
  , classes :: [[Class]]
  , attributes :: Attributes
  , children :: [Content]
  }

data Content
  = Node Element
  | Text Text

data Document
data Body
data Script
  = Url Text
  | Code Text

-- | Views contain their contents, and a list of all styles mentioned during their rendering
newtype View a x = View
  { runView :: State ViewState x
  }
  deriving newtype (Functor, Applicative, Monad, MonadState ViewState)

data ViewState = ViewState
  { contents :: [Content]
  , classStyles :: Map ClassName ClassProps
  }

instance IsString (View Content ()) where
  fromString s = modify $ \vs -> vs{contents = [Text (pack s)]}

viewContents :: View a x -> [Content]
viewContents (View wts) = (.contents) $ execState wts (ViewState [] [])

-- | All classes contained anywhere in the view
viewClasses :: View a () -> Map ClassName ClassProps
viewClasses (View st) = do
  (.classStyles) $ execState st (ViewState [] [])

-- | A function that modifies an element. Allows for easy chaining and composition
type Mod a = Element -> Element
