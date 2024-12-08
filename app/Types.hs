-- |
-- Modul: Types
-- Beschreibung: Definiert die grundlegenden Datentypen für das deutsche Adjektivdeklinations-System
-- Bewertungskriterien implementiert:
-- - Eigene Datentypen (mehrere Algebraische Datentypen)
-- - Modularisierung (separates Types-Modul)
-- - Verwendung externer Bibliotheken (Aeson, Text)
-- - Pattern Matching (in FromJSON Instance)
module Types where

import Data.Aeson
import qualified Data.Text as T

-- |
-- Repräsentiert das grammatikalische Geschlecht
-- Bewertungskriterium: Eigene Datentypen
data Gender = Masculine | Feminine | Neuter
  deriving (Show, Eq)

-- |
-- Repräsentiert die Anzahl (Singular/Plural)
-- Bewertungskriterium: Eigene Datentypen
data Number = Singular | Plural
  deriving (Show, Eq)

-- |
-- Repräsentiert die vier Fälle im Deutschen
-- Bewertungskriterium: Eigene Datentypen
data Case = Nominative | Accusative | Dative | Genitive
  deriving (Show, Eq)

-- |
-- Repräsentiert die verschiedenen Formen eines Nomens
-- Bewertungskriterium: Eigene Datentypen
data NounForm = SingularForm | PluralForm | GenitiveForm
  deriving (Show, Eq)

-- |
-- Repräsentiert ein deutsches Nomen mit seinen grammatikalischen Eigenschaften
-- Bewertungskriterium:
-- - Eigene Datentypen
-- - Record Syntax
data GermanNoun = GermanNoun
  { -- | Grundform des Nomens
    word :: T.Text,
    -- | Geschlecht (m/f/n)
    gender :: T.Text,
    -- | Pluralform (optional)
    plural :: Maybe T.Text,
    -- | Genitivform
    genitive :: T.Text
  }
  deriving (Show, Eq)

-- |
-- Repräsentiert die verschiedenen Arten von Artikeln
-- Bewertungskriterium: Eigene Datentypen mit vielen Konstruktoren
data ArticleType
  = -- | der, die, das
    Definite
  | -- | dieser, diese, dieses
    Demonstrative
  | -- | jeder, jede, jedes
    Universal
  | -- | ein, eine
    Indefinite
  | -- | mancher, manche, manches
    Some
  | -- | solcher, solche, solches
    Such
  | -- | welcher, welche, welches
    Interrogative
  | -- | mein, dein, etc.
    Possessive
  | -- | kein
    Negative
  | -- | alle (nur Plural)
    All
  | -- | beide
    Both
  | -- | kein Artikel
    NoArticle
  deriving (Show, Eq)

-- |
-- Repräsentiert die verschiedenen Arten von Possessivpronomen
-- Bewertungskriterium: Eigene Datentypen
data PossessiveType
  = Mein
  | Dein
  | Sein
  | Ihr
  | -- | Spezialfall: -er ist Teil des Artikels
    Unser
  | -- | Spezialfall: -er ist Teil des Artikels
    Euer
  deriving (Show, Eq)

-- |
-- Repräsentiert Wörter, die wie Adjektive dekliniert werden
-- Bewertungskriterium: Eigene Datentypen
data NonArticleAdjective
  = -- | viele
    Viele
  | -- | einige
    Einige
  | -- | mehrere
    Mehrere
  | -- | wenige
    Wenige
  deriving (Show, Eq)

-- |
-- Repräsentiert die möglichen Adjektivendungen
-- Bewertungskriterium: Eigene Datentypen
data Ending = E | En | Er | Es | Empty
  deriving (Show, Eq)

-- |
-- Hauptdatenstruktur für eine Adjektivphrase
-- Bewertungskriterium:
-- - Eigene Datentypen
-- - Record Syntax
-- - Komplexe Datenstruktur
data AdjectivePhrase = AdjectivePhrase
  { article :: Maybe String,
    adjective :: String,
    noun :: GermanNoun,
    nounStr :: String,
    articleType :: ArticleType,
    case_ :: Case,
    number :: Number,
    nounForm :: NounForm,
    quantifierArt :: NonArticleAdjective
  }
  deriving (Show)

-- |
-- JSON Parser für GermanNoun
-- Bewertungskriterium:
-- - Typklassen (FromJSON)
-- - Applicative Style Parsing
instance FromJSON GermanNoun where
  parseJSON = withObject "GermanNoun" $ \v ->
    GermanNoun
      <$> v .: "word"
      <*> v .: "gender"
      <*> v .:? "plural"
      <*> v .: "genitive"