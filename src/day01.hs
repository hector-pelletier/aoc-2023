-- Advent of code day 01
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Text (Text, pack, replace, unpack)

patterns = [("one", "o1e"), ("two", "t2o"), ("three", "th3e")
           ,("four", "4"), ("five", "5e"), ("six", "6")
           ,("seven", "7n"), ("eight", "e8t"), ("nine", "9e")]

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll [] text = text
replaceAll (p:ps) text = replaceAll ps (replace o n text) where (o, n) = p

calVal :: String -> Int
calVal = read . (\s -> [(head s), (last s)]) . filter isDigit

main = do
    pars <- lines <$> getContents
    print $ foldl1 (+) $ map calVal $ pars
    print $ foldl1 (+) $ map (calVal . unpack . (replaceAll patterns) . pack) $ pars