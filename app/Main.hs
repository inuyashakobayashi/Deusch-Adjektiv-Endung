import Data.Char (isUpper, isLower, toUpper)
import qualified Data.Text as T
import Data.Maybe(listToMaybe)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO (hSetEncoding, stdout,stdin, utf8)
-- 清理字符串，去除多余空格
cleanInput :: String -> String
cleanInput = unwords . words

-- 性别
data Gender = Masculine | Feminine | Neuter
  deriving (Show, Eq)

-- 数
data Number = Singular | Plural
  deriving (Show, Eq)

-- 格
data Case = Nominative | Accusative | Dative | Genitive
  deriving (Show, Eq)
--GermanNoun Form anpassen zu json Datei
data GermanNoun = GermanNoun {
  word :: T.Text,
  gender :: T.Text,
  plural :: Maybe T.Text
} deriving (Show, Eq)
-- 扩展的冠词类型
data ArticleType
  = Definite              -- 定冠词 (der, die, das, den, dem)
  | Demonstrative         -- 指示代词 (dieser, diese, dieses, diesen, diesem)
  | Universal            -- 全称词 (jeder, jede, jedes, jeden, jedem)
  | Indefinite           -- 不定冠词 (ein, eine, einen, einem, einer)
  | Some                 -- 不确定代词 (mancher, manche, manches, manchen, manchem)
  | Such                 -- 指示性形容词 (solcher, solche, solches, solchen, solchem)
  | Interrogative        -- 疑问代词 (welcher, welche, welches, welchen, welchem)
  | Possessive           -- 物主代词 (mein, dein, sein, ihr, unser, euer, Ihr)
  | Negative             -- 否定冠词 (kein)
  | All                  -- 全体词 (alle)
  | Both                 -- 双数词 (beide)
  | NoArticle            -- 无冠词
  deriving (Show, Eq)

  -- 可以添加一个数据类型来具体指明是哪种物主代词
data PossessiveType
  = Mein    -- my
  | Dein    -- your (informal)
  | Sein    -- his/its
  | Ihr     -- her/their/your (formal)
  | Unser   -- our
  | Euer    -- your (plural informal)
  deriving (Show, Eq)


-- 形容词词尾
data Ending = E | En | Er | Es | Empty
  deriving (Show, Eq)
-- 修改 AdjectivePhrase 以包含更多信息
data AdjectivePhrase = AdjectivePhrase
  { article :: Maybe String
  , adjective :: String
  , noun :: GermanNoun
  , articleType :: ArticleType
  , possessiveType :: Maybe PossessiveType  -- 只有当 articleType 为 Possessive 时才有值
  , number :: Number
  , case_ :: Case
  } deriving (Show)

-- 检查字符串是否以大写字母开头，其余小写
isCapitalized :: String -> Bool
isCapitalized [] = False
isCapitalized (x:xs) = isUpper x && all isLower xs

-- 检查字符串是否全部小写
isAllLower :: String -> Bool
isAllLower = all isLower

-- 自动修正名词首字母大写
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

-- 验证和修正输入
validateAndCorrect :: (Maybe String, String, String) -> Either String (Maybe String, String, String)
validateAndCorrect (art, adj, noun)
  | not (isAllLower adj) = Left "形容词必须全部小写"
  | not (isCapitalized noun) = Left $ "名词必须首字母大写。你是否想输入：" ++ capitalizeFirst noun ++ "?"
  | otherwise = Right (art, adj, capitalizeFirst noun)



-- 修改后的解析输入函数
parseInput :: String -> Either String (Maybe String, String, String)
parseInput input = validateAndCorrect $ case words (cleanInput input) of
    [adj, noun] -> (Nothing, adj, noun)
    [art, adj, noun] -> (Just art, adj, noun)
    _ -> error "Invalid input format. Please use either 'article adjective noun' or 'adjective noun'"


-- Modified putStrLn functions to use Text
getAdjPhraseInput :: IO (Maybe String, String, String)
getAdjPhraseInput = do
    TIO.putStrLn "Enter German phrase (e.g., 'der gut Mann' or 'gut Mann'):"
    input <- TIO.getLine
    case parseInput (T.unpack input) of
        Left err -> do
            TIO.putStrLn $ T.pack $ "Error: " ++ err
            getAdjPhraseInput
        Right result -> return result

validateNoun :: [GermanNoun] -> (Maybe String, String, String) -> Either String (Maybe String, String, GermanNoun)
validateNoun nouns (art, adj, nounStr) =
    case lookupNoun (T.pack nounStr) nouns of
        Nothing -> Left $ "Error: noun '" ++ nounStr ++ "' not found in dictionary"
        Just noun -> Right (art, adj, noun)

lookupNoun :: T.Text -> [GermanNoun] -> Maybe GermanNoun
lookupNoun searchWord nouns = 
    case listToMaybe $ filter (\n -> word n == searchWord) nouns of
        Just noun -> Just noun
        Nothing -> listToMaybe $ filter (\n -> plural n == Just searchWord) nouns


instance FromJSON GermanNoun where
    parseJSON = withObject "GermanNoun" $ \v -> GermanNoun
        <$> v .: "word"
        <*> v .: "gender"
        <*> v .:? "plural"

loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
    content <- B.readFile filePath
    case eitherDecode content of
        Left err -> error $ "Error parsing JSON: " ++ err
        Right nouns -> return nouns

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    nouns <- loadNouns "germannoun.json"
    mainLoop nouns

lookupNounForPlural :: T.Text -> [GermanNoun] -> Bool
lookupNounForPlural searchWord nouns = 
    case lookupNoun searchWord nouns of
        Just noun -> case plural noun of
            Just _ -> True    -- 找到名词且有复数形式
            Nothing -> True   -- 找到名词且复数形式为Null(与单数相同)
        Nothing -> False      -- 没找到名词，不是复数形式

mainLoop :: [GermanNoun] -> IO ()
mainLoop nouns = do
    TIO.putStrLn "\nEnter German phrase (or 'quit' to exit):"
    input <- TIO.getLine
    if T.toLower input == T.pack "quit"
        then TIO.putStrLn "Auf Wiedersehen!"
        else do
            case parseInput (T.unpack input) of
                Left err -> do
                    TIO.putStrLn $ T.pack $ "Error: " ++ err
                Right result -> case validateNoun nouns result of
                    Left err -> TIO.putStrLn $ T.pack err
                    Right (art, adj', noun) -> do
                        let isPlural = lookupNounForPlural (word noun) nouns
                        let phrase = AdjectivePhrase {
                            article = art,
                            adjective = adj',
                            noun = noun,
                            articleType = if art == Nothing then NoArticle else Definite,
                            possessiveType = Nothing,
                            number = if isPlural then Plural else Singular,
                            case_ = Nominative
                        }
                        let reasoning = getReasoningSteps phrase
                        TIO.putStrLn $ T.pack reasoning
                        let ending = getAdjectiveEnding phrase
                        let finalResult = case art of
                             Nothing -> adj' ++ ending ++ " " ++ T.unpack (word noun)
                             Just article -> article ++ " " ++ adj' ++ ending ++ " " ++ T.unpack (word noun)
                        TIO.putStrLn $ T.pack $ "Final result: " ++ finalResult
            mainLoop nouns

-- 示例输出:
-- Enter German phrase (or 'quit' to exit):
-- das gut Buch
-- Step 1: Checking for article...
-- ...
-- Final result: das gute Buch

isStandardForm :: String ->Gender -> Case ->Bool
isStandardForm article gender caseType = case (article,gender,caseType) of
  ("der",Masculine,Nominative) -> True
  ("die",Feminine,Nominative) -> True
  ("das",Neuter,Nominative) ->True
  ("ein",_,Nominative) ->True
  ("eine",Feminine,Nominative) ->True
  ("ihre",Feminine,Nominative) ->True
  ("ihr",_,Nominative) ->True
  ("kein",_,Nominative) ->True
  ("keine",Feminine,Nominative) ->True
  ("mein",_,Nominative) ->True
  ("meine",Feminine,Nominative) ->True
  ("unser",_,Nominative) -> True
  ("unsere",Feminine,Nominative) ->True
  ("Ihr",_,Nominative) ->True
  ("Ihre",Feminine,Nominative) ->True
  ("euer",_,Nominative) ->True
  ("eure",Feminine,Nominative) ->True
  ("sein",_,Nominative) ->True
  ("seine",Feminine,Nominative) ->True
  ("dein",_,Nominative) ->True
  ("deine",Feminine,Nominative) ->True


  

  -- Add other standard forms as needed
  _ ->False

-- Get the der-word form for a noun based on its gender and case
getDerForm :: Gender -> Case -> String
getDerForm gender caseType = case (gender, caseType) of
    (Masculine, Nominative) -> "er"
    (Feminine, Nominative) -> "e"
    (Neuter, Nominative) -> "es"
    (Masculine, Accusative) -> "en"
    (Feminine, Accusative) -> "e"
    (Neuter, Accusative) -> "es"
    -- Add other cases
    _ -> "en"  -- Default for dative and genitive

-- Check if an article shows gender
showsGender :: String -> Bool
showsGender article = not $ article `elem` ["ein", "mein", "dein", "sein", "ihr", "unser", "euer", "Ihr"]

-- Get the appropriate adjective ending
getAdjectiveEnding :: AdjectivePhrase -> String
getAdjectiveEnding phrase = case articleType phrase of
    NoArticle -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase)
    _ -> case article phrase of
        Nothing -> getDerForm (genderFromText $ gender $ noun phrase) (case_ phrase)
        Just art -> (if not (isStandardForm art (genderFromText $ gender $ noun phrase) (case_ phrase)) || (number phrase == Plural) then "en" else (if not (showsGender art)
                    then case genderFromText $ gender $ noun phrase of
                        Masculine -> "er"
                        Neuter -> "es"
                        _ -> "e"
                    else "e"))

-- Helper function to convert Text gender to Gender type
genderFromText :: T.Text -> Gender
genderFromText g = case T.unpack g of
    "m" -> Masculine      -- 添加这行处理 "m" 的情况
    "f" -> Feminine      -- 添加这行处理 "f" 的情况
    "n" -> Neuter       -- 添加这行处理 "n" 的情况
    "masculine" -> Masculine
    "feminine" -> Feminine
    "neuter" -> Neuter
    _ -> Neuter
-- Add reasoning steps function
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
                 then "Plural -> use -en"
                 else "Singular -> continue to step 4"),
            "Step 4: Checking if article shows gender - " ++
                (if showsGender art 
                 then "Yes -> use -e" 
                 else "No -> use " ++ case genderFromText $ gender $ noun phrase of
                    Masculine -> "-er"
                    Neuter -> "-es"
                    Feminine -> "-e")
         ]