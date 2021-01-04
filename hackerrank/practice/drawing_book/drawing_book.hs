solve :: [Int] -> Int
solve [n, p]
  | p <= (div n 2) = div p 2
  | odd n = div (n - p) 2
  | otherwise = div (n - p + 1) 2

main :: IO ()
main = interact $ show . solve . map read . words
