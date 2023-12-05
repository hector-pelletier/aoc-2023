-- Advent of code day 05

getSeeds :: [String] -> [Int]
getSeeds a = map read $ (tail . words . head) a

getMaps :: [[Int]] -> [String] -> [[[Int]]]
getMaps acc [] = [acc]
getMaps acc (l:ls) = case l of
    "" -> acc : (getMaps [] $ tail $ ls)
    s -> getMaps (ins : acc) ls where ins = map read $ words $ s

step :: [[Int]] -> Int -> Int
step [] n = n
step (ins:inss) n = if n >= s && n < s + l then t + (n - s) else step inss n
    where [t,s,l] = ins 

rangeStep :: [[Int]] -> (Int, Int) -> [(Int, Int)]
rangeStep [] (s, l) = [(s, l)]
rangeStep (ins:inss) (s, l)
    | l <= 0 = []
    | b >= s && b + r <= s + l =
        [(t, r)] ++ (rem (s, (b - s))) ++ (rem ((b + r), (s + l - b - r))) 
    | b >= s && b < s + l && b + r > s + l =
        [(t, s + l - b)] ++ (rem (s, (b - s)))
    | b < s && b + r >= s + 1 && b + r < s + l =
        [(t + (s - b), (b + r - s))] ++ (rem (b + r, l - (b + r - s)))
    | b < s && b + r >= s + l = [(t + (s - b), l)]
    | otherwise = rem (s, l)
    where [t,b,r] = ins
          rem = rangeStep inss

allSteps :: [Int] -> [[[Int]]] -> [Int]
allSteps = foldl (\d inss -> map (step inss) $ d)

allRangeSteps :: [(Int, Int)] -> [[[Int]]] -> [(Int, Int)]
allRangeSteps = foldl (\d inss -> concat $ map (rangeStep inss) $ d)

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:y:t) = (x, y) : (pairs t)

main :: IO ()
main = do
    alma <- lines <$> getContents
    let seeds = getSeeds $ alma
    let maps = getMaps [] $ (tail . tail . tail) $ alma
    print $ minimum $ allSteps seeds $ maps
    print $ minimum $ map fst $ allRangeSteps (pairs seeds) $ maps