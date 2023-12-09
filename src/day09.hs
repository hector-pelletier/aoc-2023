-- Advent of code day 09

diffs :: [Int] -> [Int]
diffs (x:y:l) = (y-x):(diffs (y:l))
diffs _ = []

next :: [Int] -> Int
next l = (last l) + (if all (==0) dl then 0 else next dl)
    where dl = diffs l

prev :: [Int] -> Int
prev l = (head l) + (if all (==0) dl then 0 else prev dl)
    where dl = map negate $ diffs l

main :: IO ()
main = do
    seqs <- map ((map read) . words) <$> lines <$> getContents :: IO [[Int]]
    print $ sum $ map next $ seqs
    print $ sum $ map prev $ seqs