-- Advent of code day 07

import Data.List (sortBy, elemIndex)

bid :: String -> (String, Int)
bid s = (h, read b) where [h, b] = words s

tblComp :: String -> String -> String -> Ordering
tblComp tbl h1 h2 = compare (strengths h1) (strengths h2)
    where strengths = map (flip elemIndex tbl)

counts :: String -> [Int]
counts c = rCounts [0] c where
    rCounts acc [] = sortBy (flip compare) acc
    rCounts acc h@(c:cs) = rCounts ((length $ filter (==c) $ h):acc) (filter (/=c) cs)

bestCard :: String -> [Int]
bestCard hand = (head cards + nJ):(tail cards)
    where nJ    = length $ filter (=='J') $ hand
          cards = counts $ filter (/='J') $ hand

winnings :: [(String, Int)] -> Int
winnings = (foldl (\s (a, b) -> s + a * b) 0) . (zip [1..]) . (map snd)

handComp :: (String -> [Int]) -> String -> (String, a) -> (String, a) -> Ordering
handComp card tbl (h1, _) (h2, _) = (compare (card h1) (card h2)) `mappend` (tblComp tbl h1 h2)

main :: IO ()
main = do
    hands <- map bid <$> lines <$> getContents 
    print $ winnings $ sortBy (handComp counts   "23456789TJQKA") $ hands 
    print $ winnings $ sortBy (handComp bestCard "J23456789TQKA") $ hands