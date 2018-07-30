module HTML where

import Data.Text (Text, pack, concatMap, singleton)

escape :: Text -> Text
escape = Data.Text.concatMap escapeChar
  where
    escapeChar '<' = pack "&lt;"
    escapeChar '>' = pack "&gt;"
    escapeChar '&' = pack "&amp;"
    escapeChar '"' = pack "&quot;"
    escapeChar '\'' = pack "&apos;"
    escapeChar c = singleton c

dl, dt, dd :: [Text] -> [Text]
dl = tag "dl"
dt = tag "dt"
dd = tag "dd"

img :: Text -> [Text]
img f = [pack "<img src='" <> escape f <> pack "' />"]

tag :: String -> [Text] -> [Text]
tag t ts = [pack $ "<" ++ t ++ ">"] ++ ts ++ [pack $ "</" ++ t ++ ">"]
