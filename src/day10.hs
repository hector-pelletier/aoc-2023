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
    | n `elem` ['|','7','F'] = ((i-1, j), 'N')
    | s `elem` ['|','J','L'] = ((i+1, j), 'S')
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

area :: Map (Int, Int) (Int, Int) -> Int
area track = (flip div) 2 $ abs $ Map.foldrWithKey (\(i,j) (k,l) s -> s + i * l - j * k) 0 $ track

points :: Map (Int, Int) (Int, Int) -> Int
points track = (area track) - (Map.size track `div` 2) + 1

main :: IO ()
main = do
    (grid, start) <- gridFrom <$> getContents
    let (p, mv) = findStart grid start
    let track = trackFrom grid p mv (Map.singleton start p)
    print $ (Map.size track) `div` 2
    print $ points $ track