module Types where

import Data.Aeson
import qualified Data.Text as T

data Gender = Masculine | Feminine | Neuter
  deriving (Show, Eq)

data Number = Singular | Plural
  deriving (Show, Eq)

data Case = Nominative | Accusative | Dative | Genitive
  deriving (Show, Eq)

data NounForm = SingularForm | PluralForm | GenitiveForm
  deriving (Show, Eq)

data GermanNoun = GermanNoun
  { word :: T.Text,
    gender :: T.Text,
    plural :: Maybe T.Text,
    genitive :: T.Text
  }
  deriving (Show, Eq)

-- Erweiterte ArticleType um alle möglichen Artikel-Typen
data ArticleType
  = Definite -- der, die, das
  | Demonstrative -- dieser, diese, dieses
  | Universal -- jeder, jede, jedes
  | Indefinite -- ein, eine
  | Some -- mancher, manche, manches
  | Such -- solcher, solche, solches
  | Interrogative -- welcher, welche, welches
  | Possessive -- mein, dein, etc.
  | Negative -- kein
  | All -- alle (nur Plural)
  | Both -- beide
  | NoArticle -- kein Artikel
  deriving (Show, Eq)

-- Erweiterte PossessiveType um spezielle Fälle wie unser/euer
data PossessiveType
  = Mein
  | Dein
  | Sein
  | Ihr
  | Unser -- Spezialfall: -er ist Teil des Artikels
  | Euer -- Spezialfall: -er ist Teil des Artikels
  deriving (Show, Eq)

-- Neue data type für nicht-Artikel Wörter die wie Adjektive dekliniert werden
data NonArticleAdjective
  = Viele -- viele
  | Einige -- einige
  | Mehrere -- mehrere
  | Wenige -- wenige
  deriving (Show, Eq)

data Ending = E | En | Er | Es | Empty
  deriving (Show, Eq)

data AdjectivePhrase = AdjectivePhrase
  { article :: Maybe String,
    adjective :: String,
    noun :: GermanNoun,
    nounStr :: String,
    articleType :: ArticleType,
    case_ :: Case,
    number :: Number,
    nounForm :: NounForm
  }
  deriving (Show)

instance FromJSON GermanNoun where
  parseJSON = withObject "GermanNoun" $ \v ->
    GermanNoun
      <$> v .: "word"
      <*> v .: "gender"
      <*> v .:? "plural"
      <*> v .: "genitive"