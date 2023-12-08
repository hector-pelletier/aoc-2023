-- Advent of code day 08

import Data.Map (Map)
import qualified Data.Map as Map

node :: String -> (String, (String, String))
node s = (here, (left, right))
    where here = (head . words) s
          left = take 3 $ drop 7 $ s
          right = take 3 $ drop 12 $ s

walk :: (Map String (String, String)) -> String -> String -> String
walk net pos [] = pos
walk net pos ('L':ins) = walk net (fst (net Map.! pos)) ins 
walk net pos ('R':ins) = walk net (snd (net Map.! pos)) ins

follow :: (Map String (String, String)) -> String -> Int
follow net ins = rfollow 0 "AAA"
    where rfollow n "ZZZ" = n
          rfollow n pos = rfollow (length ins + n) (walk net pos ins)

follow2 :: (Map String (String, String)) -> String -> String -> Int
follow2 net ins start = rfollow2 0 start
    where rfollow2 n [a,b,'Z'] = n
          rfollow2 n pos = rfollow2 (length ins + n) (walk net pos ins)

main :: IO ()
main = do
    pars <- lines <$> getContents
    let ins = head pars
    let net = Map.fromList $ map node $ (tail . tail) $ pars
    print $ follow net ins
    print $ foldl1 lcm $ map (follow2 net ins) $ filter (\k -> k !! 2 == 'A') $ Map.keys $ net