growth :: Int -> Int -> Int
growth r 0 = r
growth r m
  | odd m = 2 * r
  | otherwise = r + 1

compoundGrowth :: Int -> Int
compoundGrowth p = foldl growth 1 [0..p]

solve :: [Int] -> [Int]
solve = map compoundGrowth

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
