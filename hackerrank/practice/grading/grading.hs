rGrade :: Int -> Int
rGrade n
  | n >= 38 && m5 >= 3 = n5
  | otherwise = n
  where m5 = mod n 5
        n5 = (div n 5 + 1) * 5

solve :: [Int] -> [Int]
solve xs = map rGrade xs

main = interact $ unlines . map show . solve . map read . tail . words
