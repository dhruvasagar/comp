import Text.Printf

isLeapJulian :: Int -> Bool
isLeapJulian n = mod n 4 == 0

isLeapGregorian :: Int -> Bool
isLeapGregorian n = (mod n 400 == 0) || (mod n 4 == 0 && mod n 100 /= 0)

isLeap :: Int -> Bool
isLeap n
  | n < 1918 = isLeapJulian n
  | otherwise = isLeapGregorian n

date256 :: Int -> Int
date256 n
  | n == 1918 = 26
  | isLeap n = 12
  | otherwise = 13

dateStr :: Int -> String
dateStr y = printf "%d.09.%d" (date256 y) y

solve :: Int -> String
solve = dateStr

main :: IO ()
main = interact $ solve . read . head . words
