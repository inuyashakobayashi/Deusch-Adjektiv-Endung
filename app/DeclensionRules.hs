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

isStandardForm :: String -> Gender -> Case -> Bool
isStandardForm article gender caseType = case (article,gender,caseType) of
    ("der",Masculine,Nominative) -> True
    ("die",Feminine,Nominative) -> True
    ("das",Neuter,Nominative) -> True
    ("ein",_,Nominative) -> True
    ("eine",Feminine,Nominative) -> True
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
    _ -> False

getDerForm :: Gender -> Case -> String
getDerForm gender caseType = case (gender, caseType) of
    (Masculine, Nominative) -> "er"
    (Feminine, Nominative) -> "e"
    (Neuter, Nominative) -> "es"
    (Masculine, Accusative) -> "en"
    (Feminine, Accusative) -> "e"
    (Neuter, Accusative) -> "es"
    _ -> "en"

showsGender :: String -> Bool
showsGender article = not $ article `elem` ["ein", "mein", "dein", "sein", "ihr", "unser", "euer", "Ihr"]

getAdjectiveEnding :: AdjectivePhrase -> String
getAdjectiveEnding phrase = case articleType phrase of
    NoArticle -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase)
    _ -> case article phrase of
        Nothing -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase)
        Just art -> if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase)) 
                      || (number phrase == Plural)
                   then "en"
                   else if not (showsGender art)
                        then case genderFromText $ gender $ noun phrase of
                            Masculine -> "er"
                            Neuter -> "es"
                            _ -> "e"
                        else "e"

genderFromText :: T.Text -> Gender
genderFromText g = case T.unpack g of
    "m" -> Masculine
    "f" -> Feminine
    "n" -> Neuter
    "masculine" -> Masculine
    "feminine" -> Feminine
    "neuter" -> Neuter
    _ -> Neuter

getReasoningSteps :: AdjectivePhrase -> String
getReasoningSteps phrase = case articleType phrase of
    NoArticle -> unlines [
        "Step 1: Checking for article - No article found",
        "Step 2: Getting der-form ending for " ++ T.unpack (gender $ noun phrase) ++ " noun in " ++ show (case_ phrase) ++ " case",
        "Step 3: Using der-word ending: " ++ getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase)
        ]
    _ -> case article phrase of
        Nothing -> "Error: Article type present but no article string found"
        Just art -> unlines [
            "Step 1: Checking for article - Found article: " ++ art,
            "Step 2: Checking if article is in standard form - " ++ 
                (if isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase) 
                 then "Yes, standard form" 
                 else "No, modified form -> use -en"),
            "Step 3: Checking number - " ++ 
                (if number phrase == Plural
                 then "Found plural form -> use -en"  -- 修改了这里的提示信息
                 else "Found singular form -> continue to step 4"),
            if number phrase == Plural 
            then "" -- 如果是复数，不显示第4步
            else "Step 4: Checking if article shows gender - " ++
                (if showsGender art 
                 then "Yes -> use -e" 
                 else "No -> use " ++ case genderFromText $ gender $ noun phrase of
                    Masculine -> "-er"
                    Neuter -> "-es"
                    Feminine -> "-e")
            ]