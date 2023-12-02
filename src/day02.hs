-- Advent of code day 02

possible :: [Int] -> Bool
possible (r:g:b:_) = (r <= 12) && (g <= 13) && (b <= 14)

required :: [[Char]] -> [Int]
required [] = [0, 0, 0]
required (n:c:xs) = 
    case head c of
        'r' -> [max num r, g, b] 
        'g' -> [r, max num g, b]
        'b' -> [r, g, max num b]
    where num = read n :: Int
          (r:g:b:_) = required xs
          
main :: IO ()
main = do
    reqs <- map (required . tail . tail . words) <$> lines <$> getContents
    print $ foldl1 (+) $ map fst $ filter snd $ zip [1..] $ map possible $ reqs
    print $ foldl1 (+) $ map product $ reqs