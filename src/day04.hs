-- Advent of code day 04

import Data.List (break)
import qualified Data.Set as Set

parseCard :: [String] -> Int
parseCard l = Set.size $ Set.intersection (Set.fromList a) (Set.fromList $ tail b) 
    where (a, b) = break (== "|") l

addcards :: Int -> Int -> [Int] -> [Int]
addcards n gain l = (map (+ n) $ take gain $ l) ++ (drop gain l)

scratch :: [Int] -> [Int] -> Int -> Int
scratch [] [] tot = tot
scratch (gain:gains) (n:ns) tot = n + scratch gains (addcards n gain ns) tot

main :: IO ()
main = do
    gains <- map (parseCard . tail . tail . words)  <$> lines <$> getContents
    print $ sum $ map (\s -> 2 ^ (s - 1)) $ filter (> 0) $ gains
    print $ scratch gains (replicate (length gains) 1) $ 0