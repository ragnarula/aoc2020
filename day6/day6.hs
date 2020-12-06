import Data.List
import Data.Bits

groupsOfPeople :: String -> [[String]]
groupsOfPeople = map words . map (intercalate " ") . Prelude.filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines

letterToInt :: Char -> Maybe Int
letterToInt c = lookup c (zip ['a'..'z'] [0..26])

stringToLetterNumbers :: String -> [Maybe Int]
stringToLetterNumbers xs = map letterToInt xs

accumLettersToBits :: Int -> Maybe Int -> Int
accumLettersToBits i (Just x) = (.|.) i (unsafeShiftL 1 x)
accumLettersToBits i Nothing = i

letterNumbersToBits :: (Foldable t) => t (Maybe Int) -> Int
letterNumbersToBits xs = foldl accumLettersToBits 0 xs

personAsInt :: String -> Int
personAsInt s = letterNumbersToBits $ stringToLetterNumbers s

groupAsInts :: [String] -> [Int]
groupAsInts xs = map personAsInt xs

groupsAsInts :: [[String]] -> [[Int]]
groupsAsInts xs = map groupAsInts xs

groupToBitsAll :: [Int] -> Int
groupToBitsAll xs = foldr (.&.) (maxBound :: Int) xs

groupToBitsAny :: [Int] -> Int
groupToBitsAny xs = foldr (.|.) 0 xs

groupsAsBits :: (a -> b) -> [a] -> [b]
groupsAsBits f xs = map f xs

groupsAsCount :: [Int] -> [Int]
groupsAsCount xs = map popCount xs

solution :: ([Int] -> Int) -> String -> Int
solution f xs = sum $ groupsAsCount $ groupsAsBits f $ groupsAsInts $ groupsOfPeople xs

part1 :: String -> Int
part1 xs = solution groupToBitsAny xs

part2 :: String -> Int
part2 xs = solution groupToBitsAll xs

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 file)
    putStrLn $ "Part 2: " ++ (show $ part2 file)
    -- putStrLn $ "Test : " ++ (show $ groupsAsInts input2)

        