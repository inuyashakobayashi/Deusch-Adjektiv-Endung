module Types where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

data Gender = Masculine | Feminine | Neuter
  deriving (Show, Eq)

data Number = Singular | Plural
  deriving (Show, Eq)

data Case = Nominative | Accusative | Dative | Genitive
  deriving (Show, Eq)

data GermanNoun = GermanNoun {
  word :: T.Text,
  gender :: T.Text,
  plural :: Maybe T.Text
} deriving (Show, Eq)

data ArticleType
  = Definite | Demonstrative | Universal | Indefinite
  | Some | Such | Interrogative | Possessive
  | Negative | All | Both | NoArticle
  deriving (Show, Eq)

data PossessiveType
  = Mein | Dein | Sein | Ihr | Unser | Euer
  deriving (Show, Eq)

data Ending = E | En | Er | Es | Empty
  deriving (Show, Eq)

data AdjectivePhrase = AdjectivePhrase
  { article :: Maybe String
  , adjective :: String
  , noun :: GermanNoun
  , articleType :: ArticleType
  , possessiveType :: Maybe PossessiveType
  , number :: Number
  , case_ :: Case
  } deriving (Show)

instance FromJSON GermanNoun where
    parseJSON = withObject "GermanNoun" $ \v -> GermanNoun
        <$> v .: "word"
        <*> v .: "gender"
        <*> v .:? "plural"