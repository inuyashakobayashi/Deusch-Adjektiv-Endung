module Parser 
    ( parseInput
    , lookupNoun
    , validateNoun
    , lookupNounForPlural
    , isArticlePlural
    , getOriginalNoun
    ) where

import Types
import Utils
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

parseInput :: String -> Either String (Maybe String, String, String)
parseInput input = validateAndCorrect $ case words (cleanInput input) of
    [adj, noun] -> (Nothing, adj, noun)
    [art, adj, noun] -> (Just art, adj, noun)
    _ -> error "Invalid input format"

-- 新增：获取原始输入的名词形式
getOriginalNoun :: String -> T.Text
getOriginalNoun input = case words (cleanInput input) of
    [_, noun] -> T.pack noun
    [_, _, noun] -> T.pack noun
    _ -> T.empty

lookupNoun :: T.Text -> [GermanNoun] -> Maybe GermanNoun
lookupNoun searchWord nouns = 
    case listToMaybe $ filter (\n -> word n == searchWord) nouns of
        Just noun -> Just noun
        Nothing -> listToMaybe $ filter (\n -> plural n == Just searchWord) nouns

isArticlePlural :: Maybe String -> Gender -> Bool
isArticlePlural Nothing _ = False
isArticlePlural (Just "die") Feminine = False  -- die 用于阴性单数
isArticlePlural (Just "die") _ = True          -- die 用于其他情况是复数
isArticlePlural _ _ = False

-- 修改后的复数检查函数
lookupNounForPlural :: T.Text -> T.Text -> [GermanNoun] -> Bool
lookupNounForPlural originalWord dbWord nouns = 
    let originalStr = T.unpack originalWord
        isEnEnding = length originalStr > 2 && drop (length originalStr - 2) originalStr == "en"
    in case lookupNoun dbWord nouns of
        Just noun -> case plural noun of
            Just pluralForm -> originalWord == pluralForm || (isEnEnding && originalWord /= word noun)
            Nothing -> isEnEnding
        Nothing -> isEnEnding

validateNoun :: [GermanNoun] -> (Maybe String, String, String) -> Either String (Maybe String, String, GermanNoun)
validateNoun nouns (art, adj, nounStr) =
    case lookupNoun (T.pack nounStr) nouns of
        Nothing -> Left $ "Error: noun '" ++ nounStr ++ "' not found in dictionary"
        Just noun -> Right (art, adj, noun)