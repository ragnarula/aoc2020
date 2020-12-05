
import Data.List
binpart _ _ lower upper [] 
    | lower == upper = Just lower
    | otherwise = Nothing
binpart l u lower upper (x:xs)
    | x == l = binpart l u lower mid xs
    | x == u = binpart l u (mid + 1) upper xs
    | otherwise = Nothing
    where mid = ((upper - lower) `div` 2) + lower

row = binpart 'F' 'B' 0 127

col = binpart 'L' 'R' 0 7

breakStr x = (take 7 x, take 3 $ drop 7 x)

split = map breakStr

rowColBinPart (rowstr, colstr) = (row rowstr, col colstr)

toRowCol xs = [(r, c) | (Just r, Just c) <- map rowColBinPart xs]

seatId (row, col) = (row * 8) + col

toId = map seatId

collectGaps (xs, i) x 
    | x == 0 = (xs, x)
    | otherwise = (xs ++ [i + 1..x - 1], x)

gatherMissing xs = (\(missing, _) -> missing) $ foldl collectGaps ([], 0) $ sort xs

findSeat (m:ms) ids
    | m + 1 `elem` ids && m - 1 `elem` ids = m
    | otherwise = findSeat ms ids

main = do
    file <- readFile "input.txt"
    let input = lines file
    let splitted = split input
    let rowcol = toRowCol splitted
    let ids = toId rowcol
    let part1 = maximum ids
    let missing = gatherMissing ids
    let part2 = findSeat missing ids
    putStrLn $ "Part 1: " ++ (show part1)
    putStrLn $ "Part 2: " ++ (show part2)
    -- putStrLn $ "gatherMissing: " ++ (show $ gatherMissing [0, 2, 3, 7])
    -- putStrLn $ "split: " ++ (show $ split ["BBFFBBFRLL"])
    -- putStrLn $ "FBFBBFF: " ++ (show $ row "BBFFBBF")
    -- putStrLn $ "RLR: " ++ (show $ col "LRR")

