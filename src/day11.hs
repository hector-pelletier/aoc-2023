-- Advent of code day 11

import Data.List (tails)

parseImage :: String -> ([(Int, Int)], Int, Int)
parseImage s = aux 0 0 0 [] s
    where aux i j mj l []       = (l, i, mj)
          aux i j mj l ('\n':t) = aux (i+1)     0 (j-1)         l t
          aux i j mj l ('#':t)  = aux     i (j+1)    mj ((i,j):l) t 
          aux i j mj l (h:t)    = aux     i (j+1)    mj         l t
          
pairs :: [a] -> [(a,a)]
pairs l = [(x, y) | (x:xt) <- tails l, y <- xt]

ds :: [Int] -> Int -> Int -> [Int]
ds gxs m f = [if a `elem` gxs then 1 else f | a <- [0..m]]

dist :: [Int] -> [Int] -> ((Int, Int), (Int, Int)) -> Int
dist dx dy ((x1,y1),(x2,y2)) = partial dx x1 x2 + partial dy y1 y2
    where partial ds a b | a > b  = partial ds b a
                         | a <= b = sum $ take (b-a) $ drop a $ ds

main :: IO ()
main = do
    (gxs, i, j) <- parseImage <$> getContents
    let (sdx, sdy) = (ds (map fst gxs) i 2, ds (map snd gxs) j 2)
    print $ sum $ map (dist sdx sdy) $ pairs $ gxs
    let (gdx, gdy) = (ds (map fst gxs) i 1000000, ds (map snd gxs) j 1000000)
    print $ sum $ map (dist gdx gdy) $ pairs $ gxs