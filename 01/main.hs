import System.IO
import Control.Monad

splitIntoPairs :: [a] -> [(a,a)]
splitIntoPairs xs = go xs []
    where
    go [x,y] tmp = tmp ++ [(x,y)]
    go (x:y:xs) tmp = go (y:xs) (tmp ++ [(x,y)])

isIncreasing :: (Int, Int) -> Int
isIncreasing (x,y)
    | x < y     = 1
    | otherwise = 0

main = do
    contents <- readFile "input.txt"
    let depths = map read (lines contents) :: [Int]
    print $ (sum . map isIncreasing) $ splitIntoPairs depths
