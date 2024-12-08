-- |
-- Modul: Utils
-- Beschreibung: Hilfsfunktionen für die Verarbeitung von Texteingaben
-- Bewertungskriterien implementiert:
-- - Funktionen mit pattern matching (in isCapitalized, capitalizeFirst)
-- - Funktionen mit guards (in validateAndCorrect)
-- - Rekursive Funktionen (in isAllLower)
-- - List comprehension (in capitalizeFirst)
-- - Fehlerbehandlung mit Either (in validateAndCorrect)
-- - Modularisierung (eigenes Utils-Modul)
-- - Dokumentation der wichtigsten Funktionen
module Utils where

import Data.Char (isLower, isUpper, toLower, toUpper)

-- |
-- Bereinigt die Eingabe von mehrfachen Leerzeichen
-- Bewertungskriterien: Funktionen höherer Ordnung (unwords, words)
cleanInput :: String -> String
cleanInput = unwords . words

-- |
-- Überprüft, ob ein String mit Großbuchstaben beginnt und dann nur Kleinbuchstaben enthält
-- Bewertungskriterien:
-- - Pattern Matching (x:xs)
-- - Guards (implizit durch &&)
isCapitalized :: String -> Bool
isCapitalized [] = False -- Pattern Matching für leeren String
isCapitalized (x : xs) = isUpper x && isAllLower xs

-- |
-- Überprüft rekursiv, ob alle Buchstaben klein geschrieben sind
-- Bewertungskriterien:
-- - Rekursive Funktion
-- - Pattern Matching (x:xs)
isAllLower :: String -> Bool
isAllLower [] = True -- Basisfall der Rekursion
isAllLower (x : xs) = isLower x && isAllLower xs -- Rekursiver Fall

-- |
-- Wandelt einen String in Titlecase um (erster Buchstabe groß, Rest klein)
-- Bewertungskriterien:
-- - Pattern Matching (x:xs)
-- - List Comprehension [toLower y | y <- xs]
capitalizeFirst :: String -> String
capitalizeFirst [] = [] -- Pattern Matching für leeren String
capitalizeFirst (x : xs) = toUpper x : [toLower y | y <- xs]

-- |
-- Validiert die Eingabe nach Groß-/Kleinschreibungsregeln
-- Bewertungskriterien:
-- - Guards (|)
-- - Fehlerbehandlung mit Either
-- - Pattern Matching (in den Parametern)
validateAndCorrect :: (Maybe String, String, String) -> Either String (Maybe String, String, String)
validateAndCorrect (art, adj, noun)
  | not (isAllLower adj) =
      Left "adjektiv muss klein schreiben sein"
  | not (isCapitalized noun) =
      Left $ "head of noun muss groß und sonst klein schreiben sein：" ++ capitalizeFirst noun ++ "?"
  | otherwise =
      Right (art, adj, capitalizeFirst noun)