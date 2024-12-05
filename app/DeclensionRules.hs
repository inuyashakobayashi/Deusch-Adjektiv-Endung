module DeclensionRules 
    ( isStandardForm
    , getDerForm
    , showsGender
    , getAdjectiveEnding
    , genderFromText
    , getReasoningSteps
    ) where

import Types
import qualified Data.Text as T
import Validate

isStandardForm :: String -> Gender -> Case -> Bool
isStandardForm art gen caseType = case (art,gen,caseType) of
    -- Bestimmte Artikel
    ("der",Masculine,Nominative) -> True
    ("die",Feminine,Nominative) -> True
    ("das",Neuter,Nominative) -> True
    
    -- Unbestimmte Artikel
    ("ein",_,Nominative) -> True
    ("eine",Feminine,Nominative) -> True
    
    -- Possessivpronomen
    ("ihre",Feminine,Nominative) -> True
    ("ihr",_,Nominative) -> True
    ("kein",_,Nominative) -> True
    ("keine",Feminine,Nominative) -> True
    ("mein",_,Nominative) -> True
    ("meine",Feminine,Nominative) -> True
    ("unser",_,Nominative) -> True
    ("unsere",Feminine,Nominative) -> True
    ("Ihr",_,Nominative) -> True
    ("Ihre",Feminine,Nominative) -> True
    ("euer",_,Nominative) -> True
    ("eure",Feminine,Nominative) -> True
    ("sein",_,Nominative) -> True
    ("seine",Feminine,Nominative) -> True
    ("dein",_,Nominative) -> True
    ("deine",Feminine,Nominative) -> True
    
    -- Demonstrativpronomen
    ("dieser",Masculine,Nominative) -> True
    ("diese",Feminine,Nominative) -> True
    ("dieses",Neuter,Nominative) -> True
    
    -- Indefinitpronomen
    ("jede",Feminine,Nominative) -> True
    ("jedes",Neuter,Nominative) -> True
    ("jeder",Masculine,Nominative) -> True
    
    ("mancher",Masculine,Nominative) -> True
    ("manche",Feminine,Nominative) -> True
    ("manches",Neuter,Nominative) -> True
    
    ("welcher",Masculine,Nominative) -> True
    ("welche",Feminine,Nominative) -> True
    ("welches",Neuter,Nominative) -> True
    
    ("solcher",Masculine,Nominative) -> True
    ("solche",Feminine,Nominative) -> True
    ("solches",Neuter,Nominative) -> True
    
    _ -> False
    
    

    --viele und viel ist kein artikel und beide und wenige mehrere und einige

getDerForm :: Gender -> Case -> Bool -> String
getDerForm gen caseType isPlural = 
    if isPlural 
        then getPluralEnding caseType  -- Plural-Endungen
        else getSingularEnding gen caseType  -- Singular-Endungen

getSingularEnding :: Gender -> Case -> String
getSingularEnding gen caseType = case (gen, caseType) of
    (Masculine, Nominative) -> "er"
    (Feminine, Nominative) -> "e"
    (Neuter, Nominative) -> "es"
    (Masculine, Accusative) -> "en"
    (Feminine, Accusative) -> "e"
    (Neuter, Accusative) -> "es"
    _ -> "en"
--tatächtlich wenn es ohne Artikel kann man es nur ob es in genitiv und nominativ erkennen
getPluralEnding :: Case -> String
getPluralEnding caseType = case caseType of
    Nominative -> "e"   
    Accusative -> "en"  
    _ -> "en"

showsGender :: String -> Bool
showsGender art = not $ art `elem` ["ein", "mein", "dein", "sein", "ihr", "unser", "euer", "Ihr"]
--hier sollt noch mehr ergänzen


getAdjectiveEnding :: AdjectivePhrase -> String
getAdjectiveEnding phrase = 
    let isPlural = number phrase == Plural
    in case articleType phrase of
        NoArticle -> getDerForm 
            (genderFromText $ gender $ noun phrase) 
            (case_ phrase) 
            isPlural  -- Neuer Parameter für Plural
        _ -> case article phrase of
            Nothing -> getDerForm 
                (genderFromText $ gender $ noun phrase) 
                (case_ phrase)
                isPlural  -- Neuer Parameter für Plural
            Just art -> if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase))
                || isPlural  -- Plural-Check
                then "en"
                else if not (showsGender art)
                    then case genderFromText $ gender $ noun phrase of
                        Masculine -> "er"
                        Neuter -> "es"
                        _ -> "e"
                    else "e"
--Mapping from json Datei
genderFromText :: T.Text -> Gender
genderFromText g = case T.unpack g of
    "m" -> Masculine
    "f" -> Feminine
    "n" -> Neuter
    _ -> Neuter
getReasoningSteps :: AdjectivePhrase -> String
getReasoningSteps phrase = unlines [
    "╔═══════════════════════════════════════════════════════════════",
    "║ ADJEKTIVENDUNGEN - ANALYSEPROZESS",
    "╠═══════════════════════════════════════════════════════════════",
    "║ Eingabe: " ++ getFullPhrase phrase,
    "║ Nomen: " ++ T.unpack (word $ noun phrase) ++ 
        " (" ++ showGender (genderFromText $ gender $ noun phrase) ++ 
        ", " ++ if isPlural then "Plural" else "Singular" ++ 
        ", " ++ show (case_ phrase) ++ ")",
    "╠═══════════════════════════════════════════════════════════════",
    "║ ENTSCHEIDUNGSBAUM:",
    "║",
    getReasoning phrase,
    "║",
    "╠═══════════════════════════════════════════════════════════════",
    "║ ERGEBNIS:",
    "║ " ++ getFullPhrase (phrase { adjective = adjective phrase ++ getFinalEnding phrase }),
    "╚═══════════════════════════════════════════════════════════════"
    ]
    where
    isPlural = number phrase == Plural

getReasoning :: AdjectivePhrase -> String
getReasoning phrase = case article phrase of
    Nothing -> -- Ohne Artikel
        "║ ┌─ Mit Artikel? Nein ──► benutze der-Form\n" ++
        "║ └──► der-Form für " ++ showGender (genderFromText $ gender $ noun phrase) ++ 
        " im " ++ show (case_ phrase) ++ 
        " ──► Endung: " ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
        
    Just art -> -- Mit Artikel
        let isStd = isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase)
        in if not isStd 
            then -- Nicht Standardform
                "║ ┌─ Mit Artikel? Ja ──► '" ++ art ++ "'\n" ++
                "║    │\n" ++
                "║    └─ Standardform? Nein ──► Endung: -en"
            else if isPlural
                then -- Plural
                    "║ ┌─ Mit Artikel? Ja ──► '" ++ art ++ "'\n" ++
                    "║    │\n" ++
                    "║    ├─ Standardform? Ja\n" ++
                    "║    │\n" ++
                    "║    └─ Singular? Nein ──► Endung: -en"
                else -- Singular + weitere Prüfung
                    "║ ┌─ Mit Artikel? Ja ──► '" ++ art ++ "'\n" ++
                    "║    │\n" ++
                    "║    ├─ Standardform? Ja\n" ++
                    "║    │\n" ++
                    "║    ├─ Singular? Ja\n" ++
                    "║    │\n" ++
                    "║    └─ Zeigt Geschlecht? " ++ 
                    (if showsGender art 
                        then "Ja ──► Endung: -e"
                        else "Nein ──► Endung: " ++ 
                            case genderFromText $ gender $ noun phrase of
                                Masculine -> "-er"
                                Neuter -> "-es"
                                _ -> "-e")
    where
    isPlural = number phrase == Plural
    
showGender :: Gender -> String
showGender Masculine = "Maskulinum"
showGender Feminine = "Femininum"
showGender Neuter = "Neutrum"

getFinalEnding :: AdjectivePhrase -> String
getFinalEnding phrase = case article phrase of
    Nothing -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase) isPlural
    Just art -> 
        if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase))
            then "-en"
            else if isPlural
                then "-en"
                else if showsGender art
                    then "-e"
                    else case genderFromText $ gender $ noun phrase of
                        Masculine -> "-er"
                        Neuter -> "-es"
                        Feminine -> "-e"
    where
    isPlural = number phrase == Plural

getFullPhrase :: AdjectivePhrase -> String
getFullPhrase phrase = case article phrase of
    Just art -> art ++ " " ++ adjective phrase ++ " " ++  (nounStr  phrase)
    Nothing -> adjective phrase ++ " " ++  (nounStr  phrase)