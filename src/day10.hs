-- Advent of code day 10

import Data.Map (Map)
import qualified Data.Map as Map

gridFrom :: String -> (Map (Int, Int) Char, (Int, Int))
gridFrom l = aux Map.empty (0,0) 0 0 l
    where aux grid start i j l = case l of
            [] -> (grid, start)
            ('\n':t) -> aux grid start (i+1) 0 t
            ('S':t) -> aux (Map.insert (i,j) 'S' grid) (i,j) i (j+1) t
            (c:t) -> aux (Map.insert (i,j) c grid) start i (j+1) t 

findStart :: Map (Int, Int) Char -> (Int, Int) -> ((Int,Int), Char)
findStart grid (i,j)
    | n == '|' || n == '7' || n == 'F' = ((i-1, j), 'N')
    | s == '|' || s == 'J' || s == 'L' = ((i+1, j), 'S')
    | otherwise = ((i, j+1), 'E') -- N, S are not in the cycle, so E is.
    where n = Map.findWithDefault '#' (i-1, j) grid
          s = Map.findWithDefault '#' (i+1, j) grid

trackFrom :: Map (Int, Int) Char -> (Int, Int) -> Char -> Map (Int, Int) (Int, Int) -> Map (Int, Int) (Int, Int)
trackFrom grid (i,j) mv track
    | c == 'S' = track
    | (c, mv) `elem` [('|','N'), ('J','E'), ('L','W')] = trackFrom grid (i-1, j) 'N' (Map.insert (i,j) (i-1, j) track)
    | (c, mv) `elem` [('|','S'), ('F','W'), ('7','E')] = trackFrom grid (i+1, j) 'S' (Map.insert (i,j) (i+1, j) track)
    | (c, mv) `elem` [('-','E'), ('L','S'), ('F','N')] = trackFrom grid (i, j+1) 'E' (Map.insert (i,j) (i, j+1) track)
    | (c, mv) `elem` [('-','W'), ('J','S'), ('7','N')] = trackFrom grid (i, j-1) 'W' (Map.insert (i,j) (i, j-1) track)
    | otherwise = track
    where c = grid Map.! (i,j)

findS :: Map (Int, Int) Char -> (Int, Int) -> Char
findS grid (i,j)
    | n && s = '|'
    | n && w = 'J'
    | n && e = 'L'
    | s && w = '7'
    | s && e = 'F'
    | otherwise = '-'
    where n = (Map.findWithDefault '#' (i-1, j) grid) `elem` ['|','F','7']
          s = (Map.findWithDefault '#' (i+1, j) grid) `elem` ['|','J','L']
          e = (Map.findWithDefault '#' (i, j+1) grid) `elem` ['-','J','7']
          w = (Map.findWithDefault '#' (i, j-1) grid) `elem` ['-','F','L']

isIn :: Map (Int, Int) Char -> Map (Int, Int) (Int, Int) -> Char -> (Int, Int) -> Bool
isIn grid track ssym (i, j) = 
    if (Map.member (i,j) track) then False else ((probe 0 0 '#') `mod` 2 == 1)
    where probe d n s
            | d == i = n
            | onTrack && (sym == '-') = probe (d+1) (n+1) '#'
            | onTrack && (sym == 'F') = probe (d+1) n 'F'
            | onTrack && (sym == '7') = probe (d+1) n '7'
            | onTrack && (sym == 'J') = probe (d+1) (if s == 'F' then n+1 else n) '#'
            | onTrack && (sym == 'L') = probe (d+1) (if s == '7' then n+1 else n) '#'
            | otherwise = probe (d+1) n s
            where onTrack = Map.member (d,j) track
                  sym = if grid Map.! (d,j) == 'S' then ssym else grid Map.! (d,j)

main :: IO ()
main = do
    (grid, start) <- gridFrom <$> getContents
    let (p, mv) = findStart grid start
    let track = trackFrom grid p mv (Map.singleton start p)
    print $ (Map.size track) `div` 2
    print $ length $ filter (isIn grid track (findS grid start)) $ Map.keys $ grid