-- Generate a list of possible moves using right(r), down(d), width(x) height(y)
gatherMoves :: (Integral a) => a -> a -> a -> a -> [(a, a)]
gatherMoves r d x y = zip [a `rem` x | a <- [r, (r + r)..]] [b | b <- [d, (d + d)..y], b < y]

-- Check if a particular cell (x, y) is a tree in the (input)
isTree :: (Int, Int) -> [[Char]] -> Bool
isTree (x, y) input = (input !! y) !! x == '#'

-- Count the number of cells which are trees in the (input) by moving in incements of right(r) and down(d)
countTrees :: [[Char]] -> (Int, Int) -> Int
countTrees input (r, d)  =
    let x = length $ head input; y = length input
        in length [0 | m <- gatherMoves r d x y, isTree m input]

-- Map tree counting given an (input) over a list of (right, down) move increments, then take product
solution :: [[Char]] -> [(Int, Int)] -> Int
solution input = product . map (countTrees input)

main :: IO ()
main = do 
    file <- readFile "input.txt"
    let input = lines file
    putStrLn $ "Part 1: " ++ (show $ solution input [(3, 1)])
    putStrLn $ "Part 1: " ++ (show $ solution input [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
    -- putStrLn $ "Test 1: " ++  (show $ take 100 (gatherMoves 3 1 7 7))
