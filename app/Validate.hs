module Validate (
isAll,
isDemonstrative,
isDefinite,
isNegative,
isBoth,
isInterrogative,
isPossessive,
isSome,
isQuantifier,
isUniversal,
isIndefinite,
isSuch
)where
isQuantifier :: String -> Bool
isQuantifier word = word `elem` [
    "viel", "viele", 
    "wenig", "wenige", 
    "einige", "mehrere"
    ]
isDefinite :: String -> Bool
isDefinite word = word `elem` [
  "der", "die", "das", 
  "dem", "den",
  "des"
  ]

isDemonstrative :: String -> Bool
isDemonstrative word = word `elem` [
  "dieser", "diese", "dieses",
  "diesem", "diesen",
  "jener", "jene", "jenes",
  "jenem", "jenen"
  ]

isUniversal :: String -> Bool
isUniversal word = word `elem` [
  "jeder", "jede", "jedes",
  "jedem", "jeden"
  ]

isIndefinite :: String -> Bool
isIndefinite word = word `elem` [
  "ein", "eine", "eines",
  "einem", "einen",
  "einer"
  ]

isSome :: String -> Bool
isSome word = word `elem` [
  "mancher", "manche", "manches",
  "manchem", "manchen"
  
  ]

isSuch :: String -> Bool
isSuch word = word `elem` [
  "solcher", "solche", "solches",
  "solchem", "solchen"
  ]

isInterrogative :: String -> Bool
isInterrogative word = word `elem` [
  "welcher", "welche", "welches",
  "welchem", "welchen"
  ]

isPossessive :: String -> Bool
isPossessive word = word `elem` [
  "mein", "meine", "meines",
  "dein", "deine", "deines",
  "sein", "seine", "seines",
  "ihr", "ihre", "ihres",
  "unser", "unsere", "unseres",
  "euer", "eure", "eures"
  ]

isNegative :: String -> Bool
isNegative word = word `elem` [
  "kein", "keine", "keines",
  "keinem", "keinen",
  "keiner"
  ]



isAll :: String -> Bool
isAll word = word `elem` [
  "alle"
  ]

isBoth :: String -> Bool
isBoth word = word `elem` [
  "beide"
  ]