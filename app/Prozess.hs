module Prozess (
    determineNounForm
    ,preprocessPhrase
)where
import Types
import qualified Data.Text as T
import Validate

determineNounForm :: GermanNoun -> String -> Maybe String -> NounForm
determineNounForm noun originalNoun maybeArticle =
    -- First check article-dependent cases
    case maybeArticle of
        Just "des" | T.unpack (genitive noun) == originalNoun -> GenitiveForm
        Just "der" | T.unpack (genitive noun) == originalNoun -> GenitiveForm
        
        -- Check plural forms with articles
        Just "die" | case plural noun of
            Just pluralForm -> T.unpack pluralForm == originalNoun
            Nothing -> False -> PluralForm
            
        -- If word matches genitive but no genitive article, treat as normal form
        _ | T.unpack (genitive noun) == originalNoun -> 
            case maybeArticle of
                Just "der" -> GenitiveForm
                Just "des" -> GenitiveForm
                _ -> SingularForm  -- Default to singular if no genitive article
                
        -- Check plural without relying on article
        _ | case plural noun of
            Just pluralForm -> T.unpack pluralForm == originalNoun
            Nothing -> False -> PluralForm
            
        -- Default case: must be singular
        _ -> SingularForm

-- Vorverarbeitung der AdjectivePhrase
preprocessPhrase :: AdjectivePhrase -> AdjectivePhrase
preprocessPhrase phrase =
    case article phrase of
        Just art | isQuantifier art || isAll art || isBoth art ->
            phrase { articleType = NoArticle }
        Just art | isDefinite art ->
            phrase { articleType = Definite }
        Just art | isDemonstrative art ->
            phrase { articleType = Demonstrative }
        Just art | isUniversal art ->
            phrase { articleType = Universal }
        Just art | isIndefinite art ->
            phrase { articleType = Indefinite }
        Just art | isSome art ->
            phrase { articleType = Some }
        Just art | isSuch art ->
            phrase { articleType = Such }
        Just art | isInterrogative art ->
            phrase { articleType = Interrogative }
        Just art | isPossessive art ->
            phrase { articleType = Possessive }
        Just art | isNegative art ->
            phrase { articleType = Negative }
        _ -> phrase