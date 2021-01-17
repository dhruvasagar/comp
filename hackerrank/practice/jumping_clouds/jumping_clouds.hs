jumpCloud :: Int -> Int -> Int -> [Int] -> Int
jumpCloud i e k cs
  | i == 0 && e < 100 = e
  | otherwise = jumpCloud ni re k cs
  where n = length cs
        ni = mod (i + k) n
        re = e - (2 * (cs !! ni) + 1)

solve :: [Int] -> Int
solve (k:cs) = jumpCloud 0 100 k cs

main :: IO ()
main = interact $ show . solve . map read . tail . words
