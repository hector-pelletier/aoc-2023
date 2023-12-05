-- Advent of code day 04

import Data.List (break)
import qualified Data.Set as Set

wins :: [String] -> Int
wins l = Set.size $ Set.intersection (Set.fromList a) (Set.fromList $ tail b) 
    where (a, b) = break (== "|") l

mcardwins :: [Int] -> Int -> Int
mcardwins cards = (map (cardwins cards) [0..] !!)
    where cardwins cards n = 1 + (foldr (\a b -> b + (mcardwins cards a)) 0 [(n+1)..(n+(cards !! n))])

main :: IO ()
main = do
    cards <- map (wins . tail . tail . words)  <$> lines <$> getContents
    print $ sum $ map (\s -> 2 ^ (s - 1)) $ filter (> 0) $ cards 
    print $ sum $ map (mcardwins cards) $ [0..(length cards - 1)]