module Parser 
    ( parseInput
    , lookupNoun
    , validateNoun
    , isArticlePlural
    , getOriginalNoun
    ) where

import Types
import Utils
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

-- | Parst einen Eingabestring und extrahiert Artikel (optional), Adjektiv und Nomen
-- Gibt entweder einen Fehler (Left String) oder ein Tupel (Right) mit den extrahierten Werten zurück
parseInput :: String -> Either String (Maybe String, String, String)
parseInput input = validateAndCorrect $ case words (cleanInput input) of
    -- Fall 1: Zwei Wörter - interpretiert als Adjektiv + Nomen
    -- Nothing bedeutet "kein Artikel vorhanden"
    [adj, noun] -> (Nothing, adj, noun)
    
    -- Fall 2: Drei Wörter - interpretiert als Artikel + Adjektiv + Nomen
    -- Just art bedeutet "Artikel vorhanden"
    [art, adj, noun] -> (Just art, adj, noun)
    
    -- Fehlerfall: Wenn die Eingabe nicht 2 oder 3 Wörter enthält
    _ -> error "Invalid input format"


-- | Extrahiert das Nomen aus der Eingabe, unabhängig ob mit oder ohne Artikel
getOriginalNoun :: String -> T.Text
getOriginalNoun input = case words (cleanInput input) of
   -- Fall 1: Zwei Wörter (Adjektiv + Nomen)
   [_, noun] -> T.pack noun         -- Wandelt das Nomen in Text um
   
   -- Fall 2: Drei Wörter (Artikel + Adjektiv + Nomen) 
   [_, _, noun] -> T.pack noun      -- Wandelt das Nomen in Text um
   
   -- Fehlerfall: Ungültiges Format
   _ -> T.empty                     -- Gibt leeren Text zurück



isArticlePlural :: Maybe String -> Gender -> Bool
isArticlePlural Nothing _ = False
isArticlePlural (Just "die") Feminine = False  -- die 用于阴性单数
isArticlePlural (Just "die") _ = True          -- die 用于其他情况是复数
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
lookupNoun :: T.Text -> [GermanNoun] -> Maybe GermanNoun
lookupNoun searchWord nouns =
    case listToMaybe $ filter (\n -> word n == searchWord) nouns of
        Just noun -> Just noun
        Nothing -> listToMaybe $ filter (\n -> plural n == Just searchWord) nouns



-- | Validiert das Nomen gegen die Datenbank
validateNoun :: [GermanNoun] -> (Maybe String, String, String) -> Either String (Maybe String, String, GermanNoun)
validateNoun nouns (art, adj, nounStr) =
    case lookupNoun (T.pack nounStr) nouns of
        Nothing -> Left $ "Error: noun '" ++ nounStr ++ "' not found in dictionary"
        Just noun -> Right (art, adj, noun)