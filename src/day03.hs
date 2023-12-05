-- Advent of code day 03

import Data.Char (isDigit)
import Text.Regex.TDFA

numbers :: (Int, String) -> [Int]
numbers (i, s) = let matches = getAllTextMatches $ s =~ "[[:digit:]]+" :: [String]
    in map read matches

places :: (Int, String) -> [(Int, Int, Int)]
places (i, s) = let matches = getAllMatches $ s =~ "[[:digit:]]+" :: [(MatchOffset, MatchLength)]
    in map (\(a, b) -> (i, a, b)) matches

adjacent :: (Int, Int) -> (Int, Int, Int) -> Bool
adjacent (rs, cs) (r, c, l) = (rs >= r - 1) && (rs <= r + 1) && (cs >= c - 1) && (cs <= c + l)

condi :: Int -> Int -> (Int, (Int, Int, Int)) -> Bool
condi r c match = adjacent (r, c) $ snd match

nghbsum :: [(Int, (Int, Int, Int))] -> Int -> Int -> Int
nghbsum matches r c = sum $ map fst $ filter (condi r c) matches

silver :: [(Int, (Int, Int, Int))] -> Int -> (Int, String) -> Int
silver matches c (i, []) = 0
silver matches c (i, s:ss) = x + silver matches (c+1) (i, ss)
    where x = if isDigit s || s == '.' then 0 else nghbsum matches i c

gear :: [(Int, (Int, Int, Int))] -> Int -> Int -> Int
gear matches r c = case filter (condi r c) matches of
    a:b:[] -> fst a * fst b
    _ -> 0

gold :: [(Int, (Int, Int, Int))] -> Int -> (Int, String) -> Int
gold matches c (i, []) = 0
gold matches c (i, s:ss) = x + gold matches (c+1) (i, ss)
    where x = if s == '*' then gear matches i c else 0

main :: IO ()
main = do
    rows <- zip [0..] <$> lines <$> getContents :: IO [(Int, String)]
    n <- return $ concat $ map numbers $ rows
    p <- return $ concat $ map places $ rows
    np <- return $ zip n p
    print $ sum $ map (silver np 0) $ rows
    print $ sum $ map (gold np 0) $ rows