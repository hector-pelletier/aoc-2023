-- Advent of code day 05

getSeeds :: [String] -> [Int]
getSeeds a = map read $ (tail . words . head) a

getMaps :: [[Int]] -> [String] -> [[[Int]]]
getMaps acc [] = [acc]
getMaps acc (l:ls) = case l of
    "" -> acc : (getMaps [] $ tail $ ls)
    s -> getMaps (ins : acc) ls where ins = map read $ words $ s

inter :: (Int, Int) -> (Int, Int) -> (Int, Int)
inter (x1, l1) (x2, l2) = (x, (min (x1 + l1) (x2 + l2)) - x) where x = max x1 x2

step :: [[Int]] -> (Int, Int) -> [(Int, Int)]
step [] (s, l) = [(s, l)]
step (ins:inss) (s, l)
    | l <= 0 = []
    | ri > 0 = rem (s, xi - s) ++ rem (xi + ri, s + l - xi - ri) ++ [(t + xi - b, ri)] 
    | otherwise = rem (s, l)
    where [t,b,r] = ins
          rem = step inss
          (xi, ri) = inter (s, l) (b, r)

solve :: [(Int, Int)] -> [[[Int]]] -> Int
solve seed = minimum . (map fst) .  allSteps
    where allSteps = foldl (\d inss -> concat $ map (step inss) $ d) seed

ranges :: [Int] -> [(Int, Int)]
ranges [] = []
ranges (x:y:t) = (x, y) : (ranges t)

trivialRanges :: [Int] -> [(Int, Int)]
trivialRanges = foldl (\s x -> (x, 1) : s) []

main :: IO ()
main = do
    alma <- lines <$> getContents
    let seeds = getSeeds $ alma
    let maps = getMaps [] $ (tail . tail . tail) $ alma
    print $ solve (trivialRanges seeds) $ maps
    print $ solve (ranges seeds) $ maps