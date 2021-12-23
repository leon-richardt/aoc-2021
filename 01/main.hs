import System.IO
import Control.Monad

splitIntoPairs :: [a] -> [(a,a)]
splitIntoPairs xs = go xs []
    where
    go [x,y] pairsSoFar = pairsSoFar ++ [(x,y)]
    go (x:y:xs) pairsSoFar = go (y:xs) (pairsSoFar ++ [(x,y)])

buildSlidingWindows :: [a] -> [(a,a,a)]
buildSlidingWindows xs = go xs []
    where
    go [x,y,z] windowsSoFar = windowsSoFar ++ [(x,y,z)]
    go (x:y:z:xs) windowsSoFar = go (y:z:xs) (windowsSoFar ++ [(x,y,z)])

isIncreasing :: (Int, Int) -> Int
isIncreasing (x,y)
    | x < y     = 1
    | otherwise = 0

isIncreasingWindow :: ((Int, Int, Int), (Int, Int, Int)) -> Int
isIncreasingWindow ((x1, y1, z1), (x2, y2, z2))
    | (x1 + y1 + z1) < (x2 + y2 + z2) = 1
    | otherwise = 0

main = do
    contents <- readFile "input.txt"
    let depths = map read (lines contents) :: [Int]
    print $ (sum . map isIncreasing) $ splitIntoPairs depths -- First answer
    print $ (sum . map isIncreasingWindow) $ (splitIntoPairs . buildSlidingWindows) depths -- Second answer
