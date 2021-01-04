solve :: [Int] -> [String]
solve [x, y, z]
  | diffa < diffb = ["Cat A"]
  | diffa > diffb = ["Cat B"]
  | otherwise = ["Mouse C"]
  where
    diffa = abs (x-z)
    diffb = abs (y-z)
solve (x:y:z:xs) = solve [x, y, z] ++ solve xs

main :: IO ()
main = interact $ unlines . solve . map read . tail . words
