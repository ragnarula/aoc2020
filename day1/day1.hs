checkIs2020 :: (Eq a, Foldable t, Num a) => t a -> Maybe (t a)
checkIs2020 xs
    | sum xs == 2020 = Just xs
    | otherwise = Nothing

pairs :: [b] -> [[b]]
pairs xs = [[a,b] | a <- xs, b <- xs]

trips :: [a] -> [[a]]
trips xs = [[a, b, c] | a <- xs, b <- xs, c <- xs]

solution :: (t1 -> [a]) -> (a -> Maybe t2) -> (t2 -> t3) -> t1 -> t3
solution splitter checker combiner xs = combiner $ head [ x | Just x <- map checker $ splitter xs]

toNum :: [String] -> [Int]
toNum xs = map (\x -> read x::Int) xs 

part1 :: [Int] -> Int
part1 = solution pairs checkIs2020 product

part2 :: [Int] -> Int
part2 = solution trips checkIs2020 product

main :: IO ()
main = do
    file <- readFile "input.txt"
    let strings = lines file
    let nums = toNum strings
    let p1 = part1 nums
    let p2 = part2 nums
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2
