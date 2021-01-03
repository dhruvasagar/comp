isLeapJulian :: Int -> Bool
isLeapJulian n = (mod n 4 == 0)

isLeapGregorian :: Int -> Bool
isLeapGregorian n = (mod n 400 == 0) || (mod n 4 == 0 && mod n 100 /= 0)

isLeap :: Int -> Bool
isLeap n
  | n < 1918 = isLeapJulian n
  | otherwise = isLeapGregorian n

date256 :: Int -> Int
date256 n
  | isLeap n = 12
  | otherwise = 13

dateStr :: Int -> Int -> String
dateStr d y = (show d) ++ ".09." ++ (show y)

solve :: Int -> String
solve n
  | n == 1918 = dateStr ((date256 n) + 13) n
  | otherwise = dateStr (date256 n) n

main :: IO ()
main = interact $ solve . read . head . words
