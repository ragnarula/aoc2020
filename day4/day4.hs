import Data.List
import Text.ParserCombinators.ReadP
import Control.Applicative

mapField :: (Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h) -> ([Char], h) -> (Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h)
mapField (_, iyr, eyr, hgt, hcl, ecl, pid, cid) ("byr", value) = (Just value, iyr, eyr, hgt, hcl, ecl, pid, cid)
mapField (byr, _, eyr, hgt, hcl, ecl, pid, cid) ("iyr", value) = (byr, Just value, eyr, hgt, hcl, ecl, pid, cid)
mapField (byr, iyr, _, hgt, hcl, ecl, pid, cid) ("eyr", value) = (byr, iyr, Just value, hgt, hcl, ecl, pid, cid)
mapField (byr, iyr, eyr, _, hcl, ecl, pid, cid) ("hgt", value) = (byr, iyr, eyr, Just value, hcl, ecl, pid, cid)
mapField (byr, iyr, eyr, hgt, _, ecl, pid, cid) ("hcl", value) = (byr, iyr, eyr, hgt, Just value, ecl, pid, cid)
mapField (byr, iyr, eyr, hgt, hcl, _, pid, cid) ("ecl", value) = (byr, iyr, eyr, hgt, hcl, Just value, pid, cid)
mapField (byr, iyr, eyr, hgt, hcl, ecl, _, cid) ("pid", value) = (byr, iyr, eyr, hgt, hcl, ecl, Just value, cid)
mapField (byr, iyr, eyr, hgt, hcl, ecl, pid, _) ("cid", value) = (byr, iyr, eyr, hgt, hcl, ecl, pid, Just value)
mapField x _ =  x


isPassport :: (Maybe a1, Maybe a2, Maybe a3, Maybe a4, Maybe a5, Maybe a6, Maybe a7, h) -> Bool
isPassport (Just _, Just _, Just _, Just _, Just _, Just _, Just _, _) = True
isPassport _ = False

valid :: ReadP Char
valid = satisfy (\x -> (x >= 'a' && x<= 'z') || (x >= '0' && x <= '9') || x == '#')

fieldParser :: ReadP ([Char], [Char])
fieldParser = do
    key <- many1 valid
    satisfy (== ':')
    value <- many1 valid
    eof
    return (key, value)
    
parse :: [String] -> [([Char], [Char])]
parse = map ((\(a, _) -> a) . head . readP_to_S fieldParser)

toPassport :: Foldable t => t ([Char], h) -> (Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h, Maybe h)
toPassport xs = foldl mapField (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) xs

passports :: String -> [Bool]
passports = map isPassport . map toPassport . map parse . map words . map (intercalate " ") . filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines

solution :: String -> Int
solution f = length [ x | x <- passports f, x == True]

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ show $ solution file
    -- putStrLn $ show $ toPassport $ parse ["cid:456"]