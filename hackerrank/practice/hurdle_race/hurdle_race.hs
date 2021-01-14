solve :: [Int] -> Int
solve (k:ns)
  | k > n = 0
  | otherwise = n - k
  where n = maximum ns

main :: IO ()
main = interact $ show . solve . map read . tail . words
