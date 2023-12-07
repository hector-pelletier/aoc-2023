-- Advent of code day 07

import Data.List (sortBy, elemIndex)

bid :: String -> (String, Int)
bid s = (h, read b) where [h, b] = words s

cardsComp :: String -> String -> String -> Ordering
cardsComp tbl c1 c2 = compare (strengths c1) (strengths c2)
    where strengths = map (flip elemIndex tbl)

hand :: String -> [Int]
hand cards = recHand [0] cards where
    recHand acc [] = sortBy (flip compare) acc
    recHand acc h@(c:cs) = recHand ((length $ filter (==c) $ h):acc) (filter (/=c) cs)

bestHand :: String -> [Int]
bestHand cards = (head partialHand + nJ):(tail partialHand)
    where nJ          = length $ filter (=='J') $ cards
          partialHand = hand   $ filter (/='J') $ cards

winnings :: [(String, Int)] -> Int
winnings = (foldl (\s (a, b) -> s + a * b) 0) . (zip [1..]) . (map snd)

handComp :: (String -> [Int]) -> String -> (String, a) -> (String, a) -> Ordering
handComp hand tbl (c1, _) (c2, _) = (compare (hand c1) (hand c2)) `mappend` (cardsComp tbl c1 c2)

main :: IO ()
main = do
    bids <- map bid <$> lines <$> getContents 
    print $ winnings $ sortBy (handComp hand     "23456789TJQKA") $ bids 
    print $ winnings $ sortBy (handComp bestHand "J23456789TQKA") $ bids