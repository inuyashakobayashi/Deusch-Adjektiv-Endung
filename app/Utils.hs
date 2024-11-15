module Utils where

import Types
import qualified Data.Text as T
import Data.Char (isUpper, isLower, toUpper)

cleanInput :: String -> String
cleanInput = unwords . words

isCapitalized :: String -> Bool
isCapitalized [] = False
isCapitalized (x:xs) = isUpper x && all isLower xs

isAllLower :: String -> Bool
isAllLower = all isLower

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

validateAndCorrect :: (Maybe String, String, String) -> Either String (Maybe String, String, String)
validateAndCorrect (art, adj, noun)
  | not (isAllLower adj) = Left "形容词必须全部小写"
  | not (isCapitalized noun) = Left $ "名词必须首字母大写。你是否想输入：" ++ capitalizeFirst noun ++ "?"
  | otherwise = Right (art, adj, capitalizeFirst noun)
