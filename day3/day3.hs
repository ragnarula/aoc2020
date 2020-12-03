
movesImpl r d y [] = movesImpl r d y [(r, d)]
movesImpl _ _ y ((_, b):xs)
    | b >= y = xs
movesImpl r d y ((a, b):xs) = movesImpl r d y $ (a + r, b + d) : (a, b) : xs

moveLst r d x y = map (\(a, b) -> (a `rem` x, b)) $ movesImpl r d y []

isTree :: (Int, Int) -> [[Char]] -> Bool
isTree (x, y) xs = (xs !! y) !! x == '#'

trees r d map =
    let x = length $ head map; y = length map
        in length [0 | m <- moveLst r d x y, isTree m map]

main :: IO ()
main = do 
    file <- readFile "input.txt"
    let map = lines file
    let trees11 = trees 1 1 map
    let trees31 = trees 3 1 map
    let trees51 = trees 5 1 map
    let trees71 = trees 7 1 map
    let trees12 = trees 1 2 map
    putStrLn $ "Part 1: " ++  (show $ trees31)
    putStrLn $ "Part 2: " ++ (show $ trees11 * trees31 * trees51 * trees71 * trees12)
    -- putStrLn $ "Test 1: " ++  (show $ (movesImpl 3 1 4 []))
