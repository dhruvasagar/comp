viralGrowth :: Int -> Int
viralGrowth s = ns
  where l = s `div` 2
        ns = 3 * l

viralGrowthCumulative :: Int -> Int
viralGrowthCumulative n =
  sum $ map (`div` 2) $ take n $ iterate viralGrowth 5

solve :: Int -> Int
solve = viralGrowthCumulative

main :: IO ()
main = interact $ show . solve . head . map read . words
