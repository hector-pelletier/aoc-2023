-- Advent of code day 11

import Data.List (nub, tails)

parseImage :: String -> [(Int, Int)]
parseImage s = aux 0 0 [] s
    where aux i j l []       = l
          aux i j l ('\n':t) = aux (i+1) 0     l         t
          aux i j l ('#':t)  = aux i     (j+1) ((i,j):l) t 
          aux i j l (h:t)    = aux i     (j+1) l         t
          
pairs :: [a] -> [(a,a)]
pairs l = [(x, y) | (x:xt) <- tails l, y <- xt]   

expDist :: [(Int, Int)] -> Int -> ((Int, Int), (Int, Int)) -> Int
expDist gxs f ((x1,y1),(x2,y2)) = dx + dy 
    where (dx, dy) = (dist (map fst gxs) f x1 x2, dist (map snd gxs) f y1 y2)
          dist gxs f a b | a == b = 0
                         | a > b  = dist gxs f b a
                         | a <= b = d + (f - 1) * (d - occ - 1)
                            where d = b - a
                                  occ = length $ nub $ filter (\x -> (x > a) && (x < b)) $ gxs

main :: IO ()
main = do
    galaxies <- parseImage <$> getContents
    print $ sum $ map (expDist galaxies 2) $ pairs $ galaxies
    print $ sum $ map (expDist galaxies 1000000) $ pairs $ galaxies