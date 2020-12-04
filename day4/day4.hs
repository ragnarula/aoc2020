import Data.List
import Text.ParserCombinators.ReadP
import Control.Applicative

-- Parser combinators
letter :: ReadP Char
letter = satisfy (\x -> x >= 'a' && x<= 'z')

digit :: ReadP Char
digit = satisfy (\x -> x >= '0' && x <= '9')

hex :: ReadP Char
hex = digit <|> satisfy (\x -> x >= 'a' && x <= 'f')

numRangeP :: Integer -> Integer -> String -> ReadP (String, String)
numRangeP low high key = do
    string key
    satisfy (== ':')
    value <- count 4 digit
    let asInt = read value :: Integer
    if asInt < low || asInt > high then
        pfail
    else
        return (key, value)

byrP :: ReadP (String, String)
byrP = do
    s <-numRangeP 1920 2002 "byr"
    eof
    return s

iyrP :: ReadP (String, String)
iyrP = do
    s <- numRangeP 2010 2020 "iyr"
    eof
    return s

eyrP :: ReadP (String, String)
eyrP = do 
    s <- numRangeP 2020 2030 "eyr"
    eof
    return s

hgtP :: ReadP (String, String)
hgtP = do
    string "hgt"
    satisfy (== ':')
    num <- many1 digit
    units <- string "cm" <|> string "in"
    let asInt = read num :: Integer
    if units == "in" && asInt > 76 then
        pfail
    else if units == "cm" && asInt < 150 then
        pfail
    else
        return ("hgt", num ++ units)

hclP :: ReadP (String, String)
hclP = do
    string "hcl"
    satisfy (==':')
    string "#"
    value <- count 6 hex
    eof
    return ("hcl", value)

eclP :: ReadP (String, String)
eclP = do
    string "ecl"
    satisfy (==':')
    value <- string "amb" <|> string "blu" <|> string "brn"<|> string "gry" <|> string "grn" <|> string "hzl" <|> string "oth"
    eof
    return ("ecl", value)

pidP :: ReadP (String, String)
pidP = do
    string "pid"
    satisfy (==':')
    value <- count 9 digit
    eof
    return ("pid", value)

cidP :: ReadP (String, String)
cidP = do
    string "cid"
    satisfy (==':')
    value <- many1 (digit <|> letter)
    eof
    return ("cid", value)

fieldP :: ReadP (String, String)
fieldP = do
    byrP <|> iyrP <|> eyrP<|> hgtP <|> hclP <|> eclP <|> pidP <|> cidP

-- Used to reduce a list of (key, value) tuples into a tuple of all optional fields
accumulateFields :: (Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h) -> (String, h) -> (Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h)
accumulateFields (_, iyr, eyr, hgt, hcl, ecl, pid, cid) ("byr", value) = (Just value, iyr, eyr, hgt, hcl, ecl, pid, cid)
accumulateFields (byr, _, eyr, hgt, hcl, ecl, pid, cid) ("iyr", value) = (byr, Just value, eyr, hgt, hcl, ecl, pid, cid)
accumulateFields (byr, iyr, _, hgt, hcl, ecl, pid, cid) ("eyr", value) = (byr, iyr, Just value, hgt, hcl, ecl, pid, cid)
accumulateFields (byr, iyr, eyr, _, hcl, ecl, pid, cid) ("hgt", value) = (byr, iyr, eyr, Just value, hcl, ecl, pid, cid)
accumulateFields (byr, iyr, eyr, hgt, _, ecl, pid, cid) ("hcl", value) = (byr, iyr, eyr, hgt, Just value, ecl, pid, cid)
accumulateFields (byr, iyr, eyr, hgt, hcl, _, pid, cid) ("ecl", value) = (byr, iyr, eyr, hgt, hcl, Just value, pid, cid)
accumulateFields (byr, iyr, eyr, hgt, hcl, ecl, _, cid) ("pid", value) = (byr, iyr, eyr, hgt, hcl, ecl, Just value, cid)
accumulateFields (byr, iyr, eyr, hgt, hcl, ecl, pid, _) ("cid", value) = (byr, iyr, eyr, hgt, hcl, ecl, pid, Just value)
accumulateFields x _ =  x

-- Returns true if all necessary fields are present (last field represents cid which is optional)
isPassport :: (Maybe a1, Maybe a2, Maybe a3, Maybe a4, Maybe a5, Maybe a6, Maybe a7, h) -> Bool
isPassport (Just _, Just _, Just _, Just _, Just _, Just _, Just _, _) = True
isPassport _ = False

-- apply the accumulate function over a list of tuples of fields from one passport entry in the data
toPassport :: Foldable t => t (String, String) -> (Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String)
toPassport xs = foldl accumulateFields (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) xs

-- group input into sections separated by empty lines, then re join them with a space
passportLines :: String -> [String]
passportLines = map (intercalate " ") . filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines

-- parse each space key:value string into a field tuple, only valid data will be parsed
parseFields :: [String] -> [(String, String)]
parseFields = map (\(a, _) -> a) . concat . map (readP_to_S fieldP)

main :: IO ()
main = do
    file <- readFile "input.txt"
    let p = passportLines file
    let passportFields = map parseFields $ map words p
    let passports = map isPassport $ map toPassport passportFields
    let count = length [ x | x <- passports, x == True]
    putStrLn $ show $ count
    -- putStrLn $ show $ readP_to_S fieldP2 "hgt:74in"
    -- putStrLn $ show $ readP_to_S fieldP2 "cikjd:456"