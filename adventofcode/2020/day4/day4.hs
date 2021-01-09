import Data.Char
import Data.List

partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy _ [] = []
partitionBy f (x:xs)
  | f x = partitionBy f xs
  | otherwise = (x:ts):partitionBy f rs
  where
    ts = takeWhile (not . f) xs
    rs = drop (length ts) xs

fields = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

isValid1 :: String -> Bool
isValid1 ps = null $ fields \\ kvp
  where pairs = partitionBy (== ' ') ps
        kv = map (partitionBy (== ':')) pairs
        kvp = map (\[k, _] -> k) kv

part1 :: [String] -> Int
part1 = length . filter isValid1

data PassField = Byr Int
               | Cid String
               | Ecl String
               | Eyr Int
               | Hcl String
               | Hgt Int String
               | Iyr Int
               | Pid String
               deriving (Show)

isValidField :: PassField -> Bool
isValidField (Byr x) = x `elem` [1920..2002]
isValidField (Cid x) = True
isValidField (Ecl x) = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField (Eyr x) = x `elem` [2020..2030]
isValidField (Hcl (x:xs))
  | x /= '#' = False
  | length xs /= 6 = False
  | otherwise = all (`elem` "abcdef0123456789") xs

isValidField (Hgt x "cm") = x `elem` [150..193]
isValidField (Hgt x "in") = x `elem` [59..76]
isValidField (Hgt x _) = False

isValidField (Iyr x) = x `elem` [2010..2020]
isValidField (Pid x)
  | length x /= 9 = False
  | otherwise = all isDigit x

isValid2 :: [PassField] -> Bool
isValid2 = all isValidField

parsePassField :: (String, String) -> PassField
parsePassField ("byr", x) = Byr (read x)
parsePassField ("ecl", x) = Ecl x
parsePassField ("eyr", x) = Eyr (read x)
parsePassField ("hcl", x) = Hcl x
parsePassField ("hgt", x) = Hgt (read val) dim
  where (val, dim) = span isDigit x
parsePassField ("iyr", x) = Iyr (read x)
parsePassField ("pid", x) = Pid x
parsePassField ("cid", x) = Cid x

parsePassport :: String -> [PassField]
parsePassport ps = map parsePassField kvp
  where pairs = partitionBy (== ' ') ps
        kv = map (partitionBy (== ':')) pairs 
        kvp = map (\[k, v] -> (k, v)) kv

part2 :: [String] -> Int
part2 = length . filter isValid2 . map parsePassport . filter isValid1

solve :: [String] -> String
solve ls = unlines $ map show [part1 ps, part2 ps]
  where ps = map unwords $ partitionBy (== "") ls

main :: IO ()
main = interact $ solve . lines
