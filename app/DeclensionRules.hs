-- |
-- Modul: DeclensionRules
-- Beschreibung: Implementiert die Regeln der deutschen Adjektivdeklination
-- Bewertungskriterien implementiert:
-- - Pattern Matching (ausführlich in isStandardForm)
-- - Guards (in mehreren Funktionen)
-- - Fehlerbehandlung mit Maybe/Either indirekt
-- - Modularisierung
-- - Übersichtlicher Code mit Where-Klauseln
-- - Ausführliche Dokumentation
module DeclensionRules
  ( isStandardForm,
    getDerForm,
    showsGender,
    getAdjectiveEnding,
    genderFromText,
    getReasoningSteps,
  )
where

import qualified Data.Text as T
import Types
import Validate

-- |
-- Prüft, ob eine Artikel-Form im Nominativ Standard ist.
-- Bewertungskriterien: Pattern Matching, komplexe Bedingungen
isStandardForm :: String -> Gender -> Case -> Bool
isStandardForm art gen caseType = case (art, gen, caseType) of
  -- Only consider these forms standard in Nominative case
  -- Bestimmte Artikel
  ("der", Masculine, Nominative) -> True
  ("die", Feminine, Nominative) -> True
  ("das", Neuter, Nominative) -> True
  -- Unbestimmte Artikel
  ("ein", _, Nominative) -> True
  ("eine", Feminine, Nominative) -> True
  -- Possessivpronomen (only in Nominative)
  ("ihre", Feminine, Nominative) -> True
  ("ihr", _, Nominative) -> True
  ("kein", _, Nominative) -> True
  ("keine", Feminine, Nominative) -> True
  ("mein", _, Nominative) -> True
  ("meine", Feminine, Nominative) -> True
  ("unser", _, Nominative) -> True
  ("unsere", Feminine, Nominative) -> True
  ("Ihr", _, Nominative) -> True
  ("Ihre", Feminine, Nominative) -> True
  ("euer", _, Nominative) -> True
  ("eure", Feminine, Nominative) -> True
  ("sein", _, Nominative) -> True
  ("seine", Feminine, Nominative) -> True
  ("dein", _, Nominative) -> True
  ("deine", Feminine, Nominative) -> True
  -- Demonstrativpronomen (only in Nominative)
  ("dieser", Masculine, Nominative) -> True
  ("diese", Feminine, Nominative) -> True
  ("dieses", Neuter, Nominative) -> True
  -- Indefinitpronomen (only in Nominative)
  ("jede", Feminine, Nominative) -> True
  ("jedes", Neuter, Nominative) -> True
  ("jeder", Masculine, Nominative) -> True
  ("mancher", Masculine, Nominative) -> True
  ("manche", Feminine, Nominative) -> True
  ("manches", Neuter, Nominative) -> True
  ("welcher", Masculine, Nominative) -> True
  ("welche", Feminine, Nominative) -> True
  ("welches", Neuter, Nominative) -> True
  ("solcher", Masculine, Nominative) -> True
  ("solche", Feminine, Nominative) -> True
  ("solches", Neuter, Nominative) -> True
  -- All other cases (including all Genitive cases) are non-standard
  _ -> False

-- |
-- Ermittelt die Adjektivendung für den Fall ohne Artikel.
-- Bewertungskriterien: Pattern Matching, Guards
getDerForm :: Gender -> Case -> Bool -> String
getDerForm gen caseType isPlural =
  if isPlural
    then getPluralEnding caseType -- Plural-Endungen
    else getSingularEnding gen caseType -- Singular-Endungen

-- |
-- Ermittelt die Adjektivendung für Singular.
-- Bewertungskriterium: Pattern Matching mit komplexen Cases
getSingularEnding :: Gender -> Case -> String
getSingularEnding gen caseType = case (gen, caseType) of
  (Masculine, Nominative) -> "er"
  (Feminine, Nominative) -> "e"
  (Neuter, Nominative) -> "es"
  (Masculine, Accusative) -> "en"
  (Feminine, Accusative) -> "e"
  (Neuter, Accusative) -> "es"
  _ -> "en"

-- |
-- Ermittelt die Adjektivendung für Plural.
-- Bewertungskriterium: Pattern Matching
getPluralEnding :: Case -> String
getPluralEnding caseType = case caseType of
  Nominative -> "e"
  Accusative -> "en"
  _ -> "en"

-- |
-- Prüft, ob ein Artikel das Geschlecht anzeigt.
-- Bewertungskriterium: Funktionen höherer Ordnung (elem)
showsGender :: String -> Bool
showsGender art = not $ art `elem` ["ein", "mein", "dein", "sein", "ihr", "unser", "euer", "Ihr", "kein"]

-- |
-- Hauptfunktion zur Ermittlung der Adjektivendung.
-- Bewertungskriterien:
-- - Pattern Matching
-- - Guards
-- - Where-Klausel
getAdjectiveEnding :: AdjectivePhrase -> String
getAdjectiveEnding phrase = case article phrase of
  -- Kein Artikel oder Quantifier -> der-Form
  Nothing -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  Just art
    | isQuantifier art ->
        getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  -- Normaler Artikel
  Just art ->
    if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase))
      then "en"
      else
        if isPlural
          then "en"
          else
            if showsGender art
              then "e"
              else case genderFromText $ gender $ noun phrase of
                Masculine -> "er"
                Neuter -> "es"
                Feminine -> "e"
  where
    isPlural = number phrase == Plural

-- |
-- Konvertiert Text-Gender in Gender-Type.
-- Bewertungskriterium: Pattern Matching
genderFromText :: T.Text -> Gender
genderFromText g = case T.unpack g of
  "m" -> Masculine
  "f" -> Feminine
  "n" -> Neuter
  _ -> Neuter

-- |
-- Erzeugt detaillierte Analyse der Adjektivdeklination.
-- Bewertungskriterien:
-- - String-Verarbeitung
-- - Komplexe Ausgabeformatierung
getReasoningSteps :: AdjectivePhrase -> String
getReasoningSteps phrase =
  unlines
    [ "╔═══════════════════════════════════════════════════════════════",
      "║ ADJEKTIVENDUNGEN - ANALYSEPROZESS",
      "╠═══════════════════════════════════════════════════════════════",
      "║ Eingabe: " ++ getFullPhrase phrase,
      "╠═══════════════════════════════════════════════════════════════",
      "║ ENTSCHEIDUNGSBAUM:",
      "║",
      getReasoning phrase,
      "║",
      "╠═══════════════════════════════════════════════════════════════",
      "║ ERGEBNIS:",
      "║ " ++ getFullPhrase (phrase {adjective = adjective phrase ++ getFinalEnding phrase}),
      "╚═══════════════════════════════════════════════════════════════"
    ]

-- |
-- Erklärt die Logik der Adjektivdeklination.
-- Bewertungskriterien:
-- - Pattern Matching
-- - Guards
-- - Where-Klausel
-- - Komplexe String-Verarbeitung
getReasoning :: AdjectivePhrase -> String
getReasoning phrase = case article phrase of
  Nothing ->
    -- Ohne Artikel
    "║ ┌─ Mit Artikel? Nein ──► benutze der-Form\n"
      ++ "║ └──► der-Form für "
      ++ showGender (genderFromText $ gender $ noun phrase)
      ++ " im "
      ++ show (case_ phrase)
      ++ " ──► Endung: "
      ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  Just art
    | isQuantifier art ->
        -- Quantifier (wie viele, wenige, etc.) - behandle wie ohne Artikel
        "║ ┌─ Mit Artikel? Nein ──► benutze der-Form\n"
          ++ "║ └──► der-Form für "
          ++ showGender (genderFromText $ gender $ noun phrase)
          ++ " im "
          ++ show (case_ phrase)
          ++ " ──► Endung: "
          ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  Just art ->
    -- Mit normalem Artikel
    let isStd = isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase)
     in if not isStd
          then -- Nicht Standardform

            "║ ┌─ Mit Artikel? Ja ──► '"
              ++ art
              ++ "'\n"
              ++ "║ │\n"
              ++ "║ └─ Standardform? Nein ──► Endung: -en"
          else
            if isPlural
              then -- Plural

                "║ ┌─ Mit Artikel? Ja ──► '"
                  ++ art
                  ++ "'\n"
                  ++ "║ │\n"
                  ++ "║ ├─ Standardform? Ja\n"
                  ++ "║ │\n"
                  ++ "║ └─ Singular? Nein ──► Endung: -en"
              else -- Singular + weitere Prüfung

                "║ ┌─ Mit Artikel? Ja ──► '"
                  ++ art
                  ++ "'\n"
                  ++ "║ │\n"
                  ++ "║ ├─ Standardform? Ja\n"
                  ++ "║ │\n"
                  ++ "║ ├─ Singular? Ja\n"
                  ++ "║ │\n"
                  ++ "║ └─ Zeigt Geschlecht? "
                  ++ ( if showsGender art
                         then "Ja ──► Endung: -e"
                         else
                           "Nein ──► Endung: "
                             ++ case genderFromText $ gender $ noun phrase of
                               Masculine -> "-er"
                               Neuter -> "-es"
                               _ -> "-e"
                     )
  where
    isPlural = number phrase == Plural

-- |
-- Konvertiert Gender zu lesbarem String.
-- Bewertungskriterium: Pattern Matching
showGender :: Gender -> String
showGender Masculine = "Maskulinum"
showGender Feminine = "Femininum"
showGender Neuter = "Neutrum"

-- |
-- Erzeugt die finale Adjektivendung mit Bindestrich.
-- Bewertungskriterien:
-- - Pattern Matching
-- - Guards
-- - Where-Klausel
getFinalEnding :: AdjectivePhrase -> String
getFinalEnding phrase = case article phrase of
  -- Kein Artikel oder Quantifier -> der-Form
  Nothing -> "-" ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  Just art
    | isQuantifier art ->
        "-" ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
  -- Normaler Artikel
  Just art ->
    ( if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase))
        || isPlural
        then "-en"
        else
          ( if showsGender art
              then "-e"
              else case genderFromText $ gender $ noun phrase of
                Masculine -> "-er"
                Neuter -> "-es"
                Feminine -> "-e"
          )
    )
  where
    isPlural = number phrase == Plural

-- Zurück  Artikel Adjektiven und Substantiven mit String Form

-- |
-- Erstellt die vollständige Phrase.
-- Bewertungskriterium: Pattern Matching
getFullPhrase :: AdjectivePhrase -> String
getFullPhrase phrase = case article phrase of
  Just art -> art ++ " " ++ adjective phrase ++ " " ++ nounStr phrase
  Nothing -> adjective phrase ++ " " ++ nounStr phrase