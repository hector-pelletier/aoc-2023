-- Advent of code day 06

nwins :: Double -> Double -> Int
nwins t d =  (min (floor t) (ceiling ((t + del) / 2) - 1)) - (max 0 (floor ((t - del) / 2) + 1)) + 1
    where del = sqrt (t ^ 2 - 4 * d)

main :: IO ()
main = do
    times <- tail . words <$> getLine
    dists <- tail . words <$> getLine
    print $ product $ map (\(t, d) -> nwins (read t) (read d)) $ zip times dists
    print $ nwins (read $ concat times) (read $ concat dists)