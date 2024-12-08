module Parser
  ( parseInput,
    lookupNoun,
    validateNoun,
    isArticlePlural,
    getOriginalNoun,
  )
where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Types
import Utils

-- | Parst einen Eingabestring und extrahiert Artikel (optional), Adjektiv und Nomen
-- Gibt entweder einen Fehler (Left String) oder ein Tupel (Right) mit den extrahierten Werten zurück

-- | Extrahiert das Nomen aus der Eingabe, unabhängig ob mit oder ohne Artikel
getOriginalNoun :: String -> T.Text
getOriginalNoun input = case words (cleanInput input) of
  -- Fall 1: Zwei Wörter (Adjektiv + Nomen)
  [_, noun] -> T.pack noun -- Wandelt das Nomen in Text um

  -- Fall 2: Drei Wörter (Artikel + Adjektiv + Nomen)
  [_, _, noun] -> T.pack noun -- Wandelt das Nomen in Text um

  -- Fehlerfall: Ungültiges Format
  _ -> T.empty -- Gibt leeren Text zurück

isArticlePlural :: Maybe String -> Gender -> Bool
isArticlePlural Nothing _ = False
isArticlePlural (Just "die") Feminine = False -- die 用于阴性单数
isArticlePlural (Just "die") _ = True -- die 用于其他情况是复数
isArticlePlural _ _ = False

-- | Sucht ein Nomen in der Datenbank

{-hier sucht word in json datei,wenn noun word gibt dann zurückliefert
die noun
hier die struktur noun ist ein json,man kann einfach in die germannoun.json
einschauen,
{
      "word": "Baske",
      "gender": "m",
      "plural": "Basken"
    },

    das ist ein beispiel ,und wenn man die singular nicht finden,
    findet es anstatt plural
-}
-- Helper function to extract the last word from genitive form
-- Helper function to extract the last word from genitive formextractLastWord :: T.Text -> T.Text
extractLastWord :: T.Text -> T.Text
extractLastWord text = last $ T.words text

lookupNoun :: T.Text -> [GermanNoun] -> Maybe GermanNoun
lookupNoun searchWord nouns =
  case listToMaybe $ filter (\n -> word n == searchWord) nouns of
    Just noun -> Just noun
    Nothing -> case listToMaybe $ filter (\n -> plural n == Just searchWord) nouns of
      Just noun -> Just noun
      Nothing -> listToMaybe $ filter (\n -> extractLastWord (genitive n) == searchWord) nouns

-- Parser.hs
parseInput :: String -> Either String (Maybe String, String, String)
parseInput input = case words (cleanInput input) of
  -- Leere Eingabe
  [] -> Left "Tipp: Bitte geben Sie ein Adjektiv und ein Nomen ein.\nBeispiel: 'gut Haus' oder 'das groß Haus'"
  -- Nur ein Wort
  [word] ->
    Left $
      "Tipp: '"
        ++ word
        ++ "' ist unvollständig.\n"
        ++ "Bitte geben Sie ein Adjektiv und ein Nomen ein.\n"
        ++ "Beispiele:\n- gut Haus\n- das groß Haus"
  -- Zwei Wörter (Adjektiv + Nomen)
  [adj, noun] -> Right (Nothing, adj, noun)
  -- Drei Wörter (Artikel + Adjektiv + Nomen)
  [art, adj, noun] -> Right (Just art, adj, noun)
  -- Zu viele Wörter
  _ -> Left "Tipp: Bitte geben Sie maximal drei Wörter ein.\nFormat: [Artikel] Adjektiv Nomen\nBeispiel: 'das groß Haus'"

-- Noun Validation
validateNoun :: [GermanNoun] -> (Maybe String, String, String) -> Either String (Maybe String, String, GermanNoun, String)
validateNoun nouns (art, adj, nounStr) =
  case lookupNoun (T.pack nounStr) nouns of
    Nothing ->
      Left $
        "Das Nomen '"
          ++ nounStr
          ++ "' wurde nicht gefunden.\n"
          ++ "Verfügbare Nomen sind zum Beispiel:\n"
          ++ showExampleNouns (take 3 nouns)
    Just noun -> Right (art, adj, noun, nounStr) -- Now includes the original nounStr

-- Hilfsfunktion für Beispiele
showExampleNouns :: [GermanNoun] -> String
showExampleNouns nouns = unlines $ map (\n -> "- " ++ T.unpack (word n)) nouns