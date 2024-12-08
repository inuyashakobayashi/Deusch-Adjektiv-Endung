-- |
-- Modul: Process
-- Beschreibung: Verarbeitet die Nomenformen und Artikeltypen für die deutsche Adjektivdeklination
-- Bewertungskriterien implementiert:
-- - Pattern Matching (in beiden Funktionen)
-- - Guards (in preprocessPhrase)
-- - Funktionen mit where/let (in determineNounForm)
-- - Modularisierung
module Prozess
  ( determineNounForm,
    preprocessPhrase,
  )
where

import qualified Data.Text as T
import Types
import Validate
  ( isAll,
    isBoth,
    isDefinite,
    isDemonstrative,
    isIndefinite,
    isInterrogative,
    isNegative,
    isPossessive,
    isQuantifier,
    isSome,
    isSuch,
    isUniversal,
  )

-- |
-- Bestimmt die Form eines Nomens (Singular, Plural oder Genitiv)
-- basierend auf der Originalform und dem optionalen Artikel.
--
-- Bewertungskriterien:
-- - Pattern Matching (in case words)
-- - Verwendung von let für lokale Definitionen
-- - Guards (in elem-Check)
--
-- Parameter:
--   - nounObj: Das Nomen-Objekt aus der Datenbank oder json
--   - originalNoun: Die eingegebene Form des Nomens
--   - maybeArticle: Der optionale Artikel
--
-- Beispiele:
--   - "Animation" mit "die" -> SingularForm
--   - "Bilds" -> GenitiveForm
--   - "Häuser" -> PluralForm
determineNounForm :: GermanNoun -> String -> Maybe String -> NounForm
determineNounForm nounObj originalNoun maybeArticle =
  let baseForm = T.unpack (word nounObj) -- Grundform, z.B. "Animation"
      fullGenitiveForm = T.unpack (genitive nounObj) -- Vollständige Genitivform
      genitiveForm = case words fullGenitiveForm of
        (_ : nounPart : _) -> nounPart -- Extrahiere Nomen aus "der Animation"
        _ -> ""
   in if originalNoun == genitiveForm && originalNoun == baseForm
        then -- Spezialfall: Grundform = Genitivform
        case maybeArticle of
          Just "des" -> GenitiveForm
          Just "der" -> GenitiveForm
          Just art
            | art
                `elem` [ "deines",
                         "meines",
                         "seines",
                         "ihres",
                         "unseres",
                         "eures"
                       ] ->
                GenitiveForm
          _ -> SingularForm
        else
          if originalNoun == genitiveForm
            then GenitiveForm -- Klarer Genitiv (wie bei "Bilds")
            else
              if originalNoun == baseForm
                then SingularForm -- Klare Grundform
                else case plural nounObj of
                  Just pForm | T.unpack pForm == originalNoun -> PluralForm
                  _ -> SingularForm -- Fallback

-- |
-- Verarbeitet eine AdjectivePhrase und bestimmt den korrekten ArticleType
-- basierend auf dem vorhandenen Artikel.
--
-- Bewertungskriterien:
-- - Pattern Matching mit Guards
-- - Record Update Syntax
-- - Mehrfache Guards für verschiedene Artikeltypen
--
-- Parameter:
--   - phrase: Die zu verarbeitende AdjectivePhrase
--
-- Beispiele:
--   - "der" -> Definite
--   - "dieser" -> Demonstrative
--   - "viele" -> NoArticle
preprocessPhrase :: AdjectivePhrase -> AdjectivePhrase
preprocessPhrase phrase =
  case article phrase of
    Just art
      | isQuantifier art || isAll art || isBoth art ->
          phrase {articleType = NoArticle}
    Just art
      | isDefinite art ->
          phrase {articleType = Definite}
    Just art
      | isDemonstrative art ->
          phrase {articleType = Demonstrative}
    Just art
      | isUniversal art ->
          phrase {articleType = Universal}
    Just art
      | isIndefinite art ->
          phrase {articleType = Indefinite}
    Just art
      | isSome art ->
          phrase {articleType = Some}
    Just art
      | isSuch art ->
          phrase {articleType = Such}
    Just art
      | isInterrogative art ->
          phrase {articleType = Interrogative}
    Just art
      | isPossessive art ->
          phrase {articleType = Possessive}
    Just art
      | isNegative art ->
          phrase {articleType = Negative}
    _ -> phrase