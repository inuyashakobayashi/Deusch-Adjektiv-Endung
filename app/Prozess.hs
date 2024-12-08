module Prozess
  ( determineNounForm,
    preprocessPhrase,
  )
where

import qualified Data.Text as T
import Types
import Validate

determineNounForm :: GermanNoun -> String -> Maybe String -> NounForm
determineNounForm noun originalNoun maybeArticle =
  let baseForm = T.unpack (word noun) -- z.B. "Animation"
      fullGenitiveForm = T.unpack (genitive noun) -- z.B. "der Animation"
      genitiveForm = case words fullGenitiveForm of
        (_ : nounPart : _) -> nounPart -- Extrahiere Nomen aus "der Animation"
        _ -> ""
   in if originalNoun == genitiveForm && originalNoun == baseForm
        then -- Spezialfall: Grundform = Genitivform
        case maybeArticle of
          -- Eindeutige Genitivartikel
          Just "des" -> GenitiveForm
          Just "der" -> GenitiveForm
          -- Artikel die auf Genitiv hinweisen können
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
          -- Bei anderen Artikeln -> Grundform
          _ -> SingularForm
        else
          if originalNoun == genitiveForm
            then GenitiveForm -- Klarer Genitiv (wie bei "Bilds")
            else
              if originalNoun == baseForm
                then SingularForm -- Klare Grundform
                else case plural noun of
                  Just pForm | T.unpack pForm == originalNoun -> PluralForm
                  _ -> SingularForm -- Fallback
                  -- Vorverarbeitung der AdjectivePhrase

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