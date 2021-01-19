solve :: [Int] -> Int
solve [d1, m1, y1, d2, m2, y2]
  | y1 < y2 = 0
  | y1 > y2 = 10000
  | m1 < m2 = 0
  | m1 > m2 = 500 * (m1 - m2)
  | d1 < d2 = 0
  | otherwise = 15 * (d1 - d2)

main :: IO ()
main = interact $ show . solve . map read . words
