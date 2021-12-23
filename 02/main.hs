import System.IO

count :: [[String]] -> (Int, Int)
count xs = go xs (0, 0)
    where
    go [] (hor, vert) = (hor, vert)
    go [[a, b]] (hor, vert) = (hor + countHor a b, vert + countVert a b)
    go ([a, b]:xs) (hor, vert) = go xs (hor + countHor a b, vert + countVert a b)
    countHor a b
        | (a == "forward") = (read b :: Int)
        | otherwise = 0
    countVert a b
        | (a == "up") = -1 * (read b :: Int)
        | (a == "down") = (read b :: Int)
        | otherwise = 0

countWithAim :: [[String]] -> (Int, Int)
countWithAim xs = go xs (0, 0, 0) -- <horizontal>, <vertical>, <aim>
    where
    go [] (hor, vert, aim) = (hor, vert)
    go [[a, b]] (hor, vert, aim) = (hor + countHor a b, vert + aim * countHor a b)
    go ([a, b]:xs) (hor, vert, aim) = go xs (hor + countHor a b,
                                             vert + aim * countHor a b,
                                             aim + countAim a b)
    countHor a b
        | (a == "forward") = (read b :: Int)
        | otherwise = 0
    countAim a b
        | (a == "up") = -1 * (read b :: Int)
        | (a == "down") = (read b :: Int)
        | otherwise = 0

one = do
    contents <- readFile "input.txt"
    let instructions = map words $ lines contents
    let (hor, vert) = count instructions
    print $ hor * vert

two = do
    contents <- readFile "input.txt"
    let instructions = map words $ lines contents
    let (hor, vert) = countWithAim instructions
    print $ hor * vert

main = do
    one
    two
