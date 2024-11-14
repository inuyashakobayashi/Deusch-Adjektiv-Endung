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
    listToMaybe $ filter (\n -> word n == searchWord) nouns


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
    (maybeArticle, adj, nounStr) <- getAdjPhraseInput
    case validateNoun nouns (maybeArticle, adj, nounStr) of
        Left err -> TIO.putStrLn $ T.pack err
        Right (art, adj', noun) -> do
            TIO.putStrLn "Results:"
            TIO.putStrLn $ T.concat ["Article: ", T.pack $ show art]
            TIO.putStrLn $ T.concat ["Adjective: ", T.pack adj']
            TIO.putStrLn $ T.concat ["Noun: ", word noun]
            TIO.putStrLn $ T.concat ["Gender: ", gender noun]
            TIO.putStrLn $ T.concat ["Plural form: ", maybe "Same as singular" id (plural noun)]