isPalindrome :: String -> Bool
isPalindrome ns = ns == reverse ns

beautifulDiff :: Int -> Int
beautifulDiff n
  | isPalindrome ns = 0
  | otherwise = abs (n - rn)
  where ns = show n
        rn = read $ reverse $ show n :: Int

isBeautiful :: Int -> Int -> Bool
isBeautiful k ns
  | diff == 0 = True
  | otherwise = mod diff k == 0
  where diff = beautifulDiff ns

solve :: [Int] -> Int
solve [i, j, k]
  | k > j = length $ filter (isPalindrome . show) [i..j]
  | otherwise = length $ filter (isBeautiful k) [i..j]

main :: IO ()
main = interact $ show . solve . map read . words
