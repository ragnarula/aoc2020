import Text.ParserCombinators.ReadP
import Control.Applicative

digit :: ReadP Char
digit = satisfy (\x -> x >= '0' && x <= '9')

numbers :: Read b => Int -> ReadP b
numbers digits = fmap read (count digits digit)

lower :: ReadP Char
lower = satisfy (\x -> x >= 'a' && x<= 'z')

policyParser :: ReadP (Int, Int, Char, String)
policyParser = do
    min <- numbers 1 <|> numbers 2
    satisfy (== '-')
    max <- numbers 1 <|> numbers 2
    satisfy (== ' ')
    letter <- lower
    satisfy (== ':')
    satisfy (== ' ')
    password <- many1 lower
    eof
    return (min, max, letter, password)

addIf :: (Eq a, Num p) => a -> p -> a -> p
addIf letter sum input  
    | input == letter = sum + 1
    | otherwise = sum   

counter :: (Foldable t, Eq a, Num b) => a -> t a -> b
counter l = foldl (addIf l) 0

maybeAt :: (Ord t, Num t) => t -> [a] -> Maybe a
maybeAt _ [] = Nothing
maybeAt x _ | x < 0 = Nothing
maybeAt 0 (x:_) = Just x
maybeAt i (_:xs) = maybeAt (i-1) xs

validate_p2 :: (Ord t1, Num t1, Eq a) => (t1, t1, a, [a]) -> Bool
validate_p2 (first, second, letter, password) = ((maybeAt (first - 1) password) == Just letter) /=  ((maybeAt (second - 1) password) == Just letter)

main :: IO ()
main = do 
    file <- readFile "input.txt"
    let strings = lines file
    let parsed = map ((\(a, _) -> a) . head . readP_to_S policyParser) strings

    -- Part 1
    let counted = map (\(min, max, letter, pass) -> (min, max, (counter letter) pass)) parsed
    let valid_p1 = [ 0 | (min, max, count) <- counted, count >= min && count <= max]

    -- Part 2
    let valid_p2 = [ 0 | p <- parsed, validate_p2 p]

    putStrLn $ "Part 1: " ++ (show $ length valid_p1)
    putStrLn $ "Part 2: " ++ (show $ length valid_p2)
    -- putStrLn $ "test 1: " ++ (show $ validate_p2 (1, 3, 'a', "abcde"))
    -- putStrLn $ "test 2: " ++ (show $ validate_p2 (1, 3, 'b', "cdefg"))
    -- putStrLn $ "test 3: " ++ (show $ validate_p2 (2, 9, 'c', "ccccccccc"))

    
