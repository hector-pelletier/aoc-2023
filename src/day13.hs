-- Advent of code day 13

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (intercalate, find)

splitBlocks :: [String] -> [[String]]
splitBlocks lines = case dropWhile (==[]) lines of
                        [] -> []
                        lines' -> line : (splitBlocks lines'')
                                    where (line, lines'') = break (==[]) lines'

gridFrom :: String -> (Map (Int, Int) Char, Int, Int)
gridFrom l = aux Map.empty 1 1 l
    where aux grid i j l = case l of
            [] -> (grid, i, j-1)
            ('|':t) -> aux grid (i+1) 1 t
            (c:t) -> aux (Map.insert (i,j) c grid) i (j+1) t 

vReflection :: Map (Int, Int) Char -> Int -> Int -> Maybe Int
vReflection grid i j = find isRef [1..(i-1)] 
    where isRef n = aux n (n+1)
            where aux a b
                    | a < 1 || b > i = True
                    | otherwise = 
                        if all (==True) [(grid Map.! (a, y)) == (grid Map.! (b, y)) | y <- [1..j]]
                        then aux (a-1) (b+1) 
                        else False

hReflection :: Map (Int, Int) Char -> Int -> Int -> Maybe Int
hReflection grid i j = find isRef [1..(j-1)] 
    where isRef n = aux n (n+1)
            where aux a b
                    | a < 1 || b > j = True
                    | otherwise = 
                        if all (==True) [(grid Map.! (x, a)) == (grid Map.! (x, b)) | x <- [1..i]]
                        then aux (a-1) (b+1) 
                        else False

vReflectionExcept :: Int -> Map (Int, Int) Char -> Int -> Int -> Maybe Int
vReflectionExcept val grid i j = find isRef [1..(i-1)] 
    where isRef n = (n * 100 /= val) && (aux n (n+1))
            where aux a b
                    | a < 1 || b > i = True
                    | otherwise = 
                        if all (==True) [(grid Map.! (a, y)) == (grid Map.! (b, y)) | y <- [1..j]]
                        then aux (a-1) (b+1) 
                        else False

hReflectionExcept :: Int -> Map (Int, Int) Char -> Int -> Int -> Maybe Int
hReflectionExcept val grid i j = find isRef [1..(j-1)] 
    where isRef n = (n /= val) && (aux n (n+1))
            where aux a b
                    | a < 1 || b > j = True
                    | otherwise = 
                        if all (==True) [(grid Map.! (x, a)) == (grid Map.! (x, b)) | x <- [1..i]]
                        then aux (a-1) (b+1) 
                        else False

findReflection :: (Map (Int, Int) Char, Int, Int) -> Int
findReflection (grid, i, j) = case (hR, vR) of
    (Just y, _) -> y
    (_, Just x) -> 100 * x
    _ -> 0
    where hR = hReflection grid i j
          vR = vReflection grid i j
        
findSmudgeReflection :: (Map (Int, Int) Char, Int, Int) -> Int
findSmudgeReflection (grid, i, j) = aux 1 1
    where aux a b = case (hR, vR) of
            (Just y, _) -> y
            (_, Just x) -> x * 100
            _ -> aux a' b'
            where   a' = if a == i then 1 else a + 1
                    b' = if a == i then b + 1 else b
                    smudge = if (grid Map.! (a, b)) == '#' then '.' else '#'
                    smudged = Map.insert (a, b) smudge grid
                    hR = hReflectionExcept val smudged i j
                    vR = vReflectionExcept val smudged i j
          val = findReflection (grid, i, j)

main :: IO ()
main = do
    grids <- map (gridFrom . (intercalate "|")) <$> splitBlocks <$> lines <$> getContents
    print $ grids
    print $ sum $ map findReflection $ grids
    print $ sum $ map findSmudgeReflection $ grids