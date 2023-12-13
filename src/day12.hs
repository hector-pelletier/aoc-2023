-- Advent of code day 12

import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

split :: String -> [Int]
split s = case dropWhile (==',') s of
                "" -> []
                s' -> (read w) : (split s'')
                      where (w, s'') = break (==',') s'  

parsePicross :: [String] -> (String, [Int])
parsePicross [m1, m2] = (m1, split m2)

possibilities (grid, ins) = 
    let lgrid = length grid
        lins  = length ins
        encode p ni = ni * (lgrid) + (lgrid - 1 - p)
        decode hash = (lgrid - 1 - (hash `mod` (lgrid)), hash `div` (lgrid))
        mem_poss = (map partial_poss [0..] !!)
            where partial_poss hash
                        | [] == il = if '#' `elem` rg then 0 else 1
                        | (sum il + ni - 1) > (length rg) = 0 
                        | otherwise = ( if (head rg == '#') || (length rg == 1) then 0 else mem_poss (encode (p+1) ni) ) 
                                        + 
                                      ( if ('.' `elem` (take (head il) rg)) || ((length rg > (head il)) && ((rg !! (head il)) == '#'))          
                                        then 0
                                        else ( if length rg > (head il + 1)
                                               then mem_poss (encode (p + (head il) + 1) (ni - 1))
                                               else (if (ni /= 1) || ((length rg) < (head il)) then 0 else 1)))
                    where   (p, ni) = decode hash
                            il    = drop (lins - ni) ins
                            rg    = drop p grid
    in mem_poss ((lgrid) * (lins + 1) - 1)

goldPossibilities (grid, ins) = possibilities (longGrid, longIns)
    where longGrid = intercalate "?" (replicate 5 grid)
          longIns = concat (replicate 5 ins)

main :: IO ()
main = do
    springRows <- map (parsePicross . words) <$> lines <$> getContents
    print $ sum $ map possibilities $ springRows
    print $ sum $ map goldPossibilities $ springRows